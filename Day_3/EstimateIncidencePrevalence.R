## Variables ----
# Names of already insatiated cohort tables
study_table_name         <- paste0(stem_table, "_study")
hcq_users_table_name     <- paste0(stem_table, "_hcq")
mtx_users_table_name     <- paste0(stem_table, "_mtx")

table_names <- c(study_table_name, hcq_users_table_name, mtx_users_table_name)

# Connect to the database and call the cohorts:
cdm <- cdmFromCon(
  con = db, 
  cdmSchema = cdm_database_schema,
  writeSchema = results_database_schema,
  cdmName = db_name,
  cohortTables = table_names
)

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


# Names of denominator tables cohort tables to generate
ip_general_table_name    <- paste0(stem_table, "_ip_general")   # Denominator table
ip_covid_table_name      <- paste0(stem_table, "_ip_covid")     # Denominator table
ip_ra_table_name         <- paste0(stem_table, "_ip_ra")        # Denominator table
ip_malaria_table_name    <- paste0(stem_table, "_ip_malaria")   # Denominator table


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
    interval = "months",
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  )
  
  # estimate prevalence
  prev <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    outcomeTable = outcome_table_name,
    interval = "months",
    minCellCount = minimum_counts
  )
  
  return(list("incidence" = inc, "prevalence" = prev))
}

# HCQ
hcq_general <- getIncidencePrevalence(ip_general_table_name, hcq_users_table_name) # general pop
hcq_covid   <- getIncidencePrevalence(ip_covid_table_name, hcq_users_table_name)   # covid pop
hcq_ra      <- getIncidencePrevalence(ip_ra_table_name, hcq_users_table_name)      # ra pop
hcq_malaria <- getIncidencePrevalence(ip_malaria_table_name, hcq_users_table_name) # malaria pop

# MTX
mtx_general <- getIncidencePrevalence(ip_general_table_name, mtx_users_table_name) # general pop
mtx_ra      <- getIncidencePrevalence(ip_ra_table_name, mtx_users_table_name)      # ra pop


# Export HCQ results 
# hcq incidence results 
incidence_hcq <- hcq_general$incidence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(hcq_covid$incidence) %>%
  union_all(hcq_ra$incidence) %>%
  union_all(hcq_malaria$incidence) 
# export to csv 
write_csv(incidence_hcq, 
          file = here(output_folder, "incidence_hcq.csv"))

# hcq prevalence results 
prevalence_hcq <- hcq_general$prevalence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(hcq_covid$prevalence) %>%
  union_all(hcq_ra$prevalence) %>%
  union_all(hcq_malaria$prevalence) 
# export to csv 
write_csv(prevalence_hcq, 
          file = here(output_folder, "prevalence_hcq.csv"))


# Export MTX results 
# hcq incidence results 
incidence_mtx <- mtx_general$incidence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(mtx_ra$incidence) 
# export to csv 
write_csv(incidence_mtx, 
          file = here(output_folder, "incidence_mtx.csv"))

# hcq prevalence results 
prevalence_mtx <- mtx_general$prevalence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(mtx_ra$prevalence)
# export to csv 
write_csv(prevalence_mtx, 
          file = here(output_folder, "prevalence_mtx.csv"))
