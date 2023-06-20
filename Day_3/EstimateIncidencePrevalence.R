# Get cohort definition IDs:
study_cohort_set <- cdm[[study_table_name]] %>%
  cohort_set()

covid_id <- study_cohort_set %>% 
  filter(cohort_name == "covid") %>%
  pull(cohort_definition_id)

ra_id <- study_cohort_set %>% 
  filter(cohort_name == "rheumatoid_arthritis") %>%
  pull(cohort_definition_id)

malaria_id <- study_cohort_set %>% 
  filter(cohort_name == "malaria") %>%
  pull(cohort_definition_id)

ra_id_no_cov <- study_cohort_set %>% 
  filter(cohort_name == "rheumatoid_arthristis_no_covid") %>%
  pull(cohort_definition_id)

# Names of denominator tables cohort tables to generate
ip_general_table_name     <- paste0(stem_table, "_ip_general")     # Denominator table
ip_covid_table_name       <- paste0(stem_table, "_ip_covid")       # Denominator table
ip_ra_table_name          <- paste0(stem_table, "_ip_ra")          # Denominator table
ip_malaria_table_name     <- paste0(stem_table, "_ip_malaria")     # Denominator table
ip_ra_no_covid_table_name <- paste0(stem_table, "_ip_ra_no_covid") # Denominator table

## Instantiate denominator cohorts ----
# function to generate .csv of the cohort's attrition (+ cohort set)
exportAttrition <- function(cohort, path) {
  cohort %>%
    cohort_set() %>%
    select(-"cohort_name") %>%
    inner_join(cohort %>%
                 cohort_attrition(),
               by = "cohort_definition_id") %>%
    write_csv(file = path)
}

# General denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_general_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365, 
  temporary = FALSE
)
exportAttrition(cdm[[ip_general_table_name]], here(output_folder, "attrition_ip_general_population.csv"))

# Covid denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_covid_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = covid_id,
  temporary = FALSE
)
exportAttrition(cdm[[ip_covid_table_name]], here(output_folder, "attrition_ip_covid_population.csv"))

# RA denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_ra_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = ra_id,
  temporary = FALSE
)
exportAttrition(cdm[[ip_ra_table_name]], here(output_folder, "attrition_ip_ra_population.csv"))

# RA denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_ra_no_covid_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = ra_id_no_cov,
  temporary = FALSE
)
exportAttrition(cdm[[ip_ra_no_covid_table_name]], here(output_folder, "attrition_ip_ra_no_cov_population.csv"))

# Malaria denominator 
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_malaria_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = malaria_id,
  temporary = FALSE
)
exportAttrition(cdm[[ip_malaria_table_name]], here(output_folder, "attrition_ip_malaria_population.csv"))

## Estimate incidence prevalence ----
getIncidencePrevalence <- function(denominator_table_name, outcome_table_name) {
  
  # estimate incidence
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    outcomeTable = outcome_table_name,
    interval = c("weeks", "months"),
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  )
  
  # estimate prevalence
  prev <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    outcomeTable = outcome_table_name,
    interval = c("weeks", "months"),
    minCellCount = minimum_counts
  )
  
  return(list("incidence" = inc, "prevalence" = prev))
}

ip_general <- getIncidencePrevalence(ip_general_table_name, prevalent_users_table_name)     # general pop
ip_covid   <- getIncidencePrevalence(ip_covid_table_name, prevalent_users_table_name)       # covid pop
ip_ra      <- getIncidencePrevalence(ip_ra_table_name, prevalent_users_table_name)          # ra pop
ip_ra_no   <- getIncidencePrevalence(ip_ra_no_covid_table_name, prevalent_users_table_name) # ra -covid pop
ip_malaria <- getIncidencePrevalence(ip_malaria_table_name, prevalent_users_table_name)     # malaria pop

# Export incidence results
incidence <- ip_general$incidence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(ip_covid$incidence) %>%
  union_all(ip_ra$incidence) %>%
  union_all(ip_ra_no$incidence) %>%
  union_all(ip_malaria$incidence) 
write_csv(incidence, file = here(output_folder, "incidence.csv"))

prevalence <- ip_general$prevalence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(ip_covid$prevalence) %>%
  union_all(ip_ra$prevalence) %>%
  union_all(ip_ra_no$prevalence) %>%
  union_all(ip_malaria$prevalence) 
write_csv(prevalence, file = here(output_folder, "prevalence.csv"))

