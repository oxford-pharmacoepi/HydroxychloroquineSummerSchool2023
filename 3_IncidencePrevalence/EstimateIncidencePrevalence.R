# Names of denominator cohort tables to generate
ip_general_table_name      <- paste0(stem_table, "_ip_general")       # Denominator table
ip_ra_table_name           <- paste0(stem_table, "_ip_ra")            # Denominator table
ip_ra_no_covid_table_name  <- paste0(stem_table, "_ip_ra_no_covid")   # Denominator table
ip_sle_table_name          <- paste0(stem_table, "_ip_sle")           # Denominator table
ip_sle_no_covid_table_name <- paste0(stem_table, "_ip_sle_no_covid")  # Denominator table


## Instantiate denominator cohorts ----
# function to generate .csv of the cohort's attrition (+ cohort set)
exportAttrition <- function(cohort, path) {
  cohort %>%
    cohort_set() %>%
    select("cohort_definition_id", "cohort_name") %>%
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
  daysPriorHistory = 365
)
exportAttrition(cdm[[ip_general_table_name]], here(output_folder, "attrition_ip_general_population.csv"))

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
  strataRequirementsAtEntry = FALSE
)
exportAttrition(cdm[[ip_ra_table_name]], here(output_folder, "attrition_ip_ra_population.csv"))

# RA no Covid denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_ra_no_covid_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = ra_no_covid_id,
  strataRequirementsAtEntry = FALSE
)
exportAttrition(cdm[[ip_ra_no_covid_table_name]], here(output_folder, "attrition_ip_ra_no_covid_population.csv"))

# SLE denominator 
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_sle_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = sle_id,
  strataRequirementsAtEntry = FALSE
)
exportAttrition(cdm[[ip_sle_table_name]], here(output_folder, "attrition_ip_sle_population.csv"))

# SLE no covid denominator 
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_sle_no_covid_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = sle_no_covid_id,
  strataRequirementsAtEntry = FALSE
)
exportAttrition(cdm[[ip_sle_no_covid_table_name]], here(output_folder, "attrition_ip_sle_no_covid_population.csv"))

## Estimate incidence prevalence ----
getIncidencePrevalence <- function(denominator_table_name) {
  
  # estimate incidence
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    outcomeTable = users_table_name,
    interval = c("months"),
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  )
  
  # estimate prevalence
  prev <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    outcomeTable = users_table_name,
    interval = c("months"),
    minCellCount = minimum_counts
  )
  
  return(list("incidence" = inc, "prevalence" = prev))
}

ip_general      <- getIncidencePrevalence(ip_general_table_name)      # general pop
ip_ra           <- getIncidencePrevalence(ip_ra_table_name)           # ra pop
ip_ra_no_covid  <- getIncidencePrevalence(ip_ra_no_covid_table_name)  # ra no covid pop
ip_sle          <- getIncidencePrevalence(ip_sle_table_name)          # sle pop
ip_sle_no_covid <- getIncidencePrevalence(ip_sle_no_covid_table_name) # sle no covid pop


# Export incidence results
incidence <- ip_general$incidence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(ip_ra$incidence) %>%
  union_all(ip_ra_no_covid$incidence) %>%
  union_all(ip_sle$incidence) %>%
  union_all(ip_sle_no_covid$incidence)
write_csv(incidence, file = here(output_folder, "incidence.csv"))

# Export prevalence results
prevalence <- ip_general$prevalence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(ip_ra$prevalence) %>%
  union_all(ip_ra_no_covid$prevalence) %>%
  union_all(ip_sle$prevalence) %>%
  union_all(ip_sle_no_covid$prevalence)
write_csv(prevalence, file = here(output_folder, "prevalence.csv"))


## Table estimates by window ----
getWindowEstimates <- function(temp_id) {
  # denominators ---
  # pre-covid
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "pre_covid",
    cohortDateRange = c(study.start, as.Date("2020-02-29")),
    ageGroup = list(c(0,150)), 
    sex = c("Both"),
    daysPriorHistory = 365,                                           
    strataTable = study_table_name,
    strataCohortId = temp_id,
    strataRequirementsAtEntry = FALSE
  )
  
  # march - april
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "hcq_time",
    cohortDateRange = c(as.Date("2020-03-01"), as.Date("2020-04-30")),
    ageGroup = list(c(0,150)),
    sex = c("Both"),
    daysPriorHistory = 365,                                           
    strataTable = study_table_name,
    strataCohortId = temp_id,
    strataRequirementsAtEntry = FALSE
  )
  
  # may - end
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "post_hcq",
    cohortDateRange = c(as.Date("2020-05-01"), study.end),
    ageGroup = list(c(0,150)), 
    sex = c("Both"),
    daysPriorHistory = 365,                                           
    strataTable = study_table_name,
    strataCohortId = temp_id,
    strataRequirementsAtEntry = FALSE
  )
  
  # incidence estimates ---
  # pre
  inc_pre <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "pre_covid",
    outcomeTable = users_table_name,
    interval = c("overall"),
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
   ) %>%
    mutate(window = "pre_covid")
  
  # hcq
  inc_hcq <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "hcq_time",
    outcomeTable = users_table_name,
    interval = c("overall"),
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  ) %>%
    mutate(window = "hcq_time")
  
  # post
  inc_post <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "post_hcq",
    outcomeTable = users_table_name,
    interval = c("overall"),
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  ) %>%
    mutate(window = "post_hcq")
  
  return(inc_pre %>%
           union_all(inc_hcq) %>%
           union_all(inc_post))
}

# Save results
estimates_overall <- getWindowEstimates(ra_no_covid_id)  %>%
  union_all(getWindowEstimates(ra_id)) %>%
  union_all(getWindowEstimates(sle_id)) %>%
  union_all(getWindowEstimates(sle_no_covid_id))

write_csv(estimates_overall, file = here(output_folder, "overall_window_estimates.csv"))

