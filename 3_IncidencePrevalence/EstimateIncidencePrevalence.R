# Names of denominator cohort tables to generate
ip_general_table_name      <- paste0(stem_table, "_ip_general")       # Denominator table


## Instantiate denominator cohorts ----
# function to generate .csv of the cohort's attrition (+ cohort set)
exportAttrition <- function(cohort, path) {
  cohort %>%
    cohort_set() %>%
    inner_join(cohort %>%
                 cohort_attrition(),
               by = "cohort_definition_id") %>%
    write_csv(file = path)
}

filterCohort <- function(cohort_name) {
  ids <- cohortCount(cdm[[cohort_name]]) %>% 
    filter(number_records > 100) %>% 
    pull()
  return(ids)
}

# General denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_general_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorObservation = 365,
  overwrite = TRUE
)
exportAttrition(cdm[[ip_general_table_name]], here(output_folder, "attrition_ip_general_population.csv"))

## Estimate incidence prevalence ----
getIncidencePrevalence <- function(denominator_table_name) {
  
  # estimate incidence
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    denominatorCohortId = filterCohort(denominator_table_name),
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
    denominatorCohortId = filterCohort(denominator_table_name),
    outcomeTable = users_table_name,
    interval = c("months"),
    minCellCount = minimum_counts
  )
  
  return(list("incidence" = inc, "prevalence" = prev))
}


if (!is.null(filterCohort(ip_general_table_name))) {
  ip_general <- getIncidencePrevalence(ip_general_table_name)
  incidence  <- ip_general$incidence %>%
    mutate(denominator_target_cohort_name = "general")
  prevalence <- ip_general$prevalence %>%
    mutate(denominator_target_cohort_name = "general")
}

# Export incidence results
write_csv(incidence, file = here(output_folder, "incidence.csv"))
# Export prevalence results
write_csv(prevalence, file = here(output_folder, "prevalence.csv"))


## Table estimates by window ----
# pre-covid
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "pre_covid",
  cohortDateRange = c(study.start, as.Date("2020-02-29")),
  ageGroup = list(c(0,150)), 
  sex = c("Both"),
  daysPriorObservation = 365,                                           
  targetCohortTable = study_table_name,
  targetCohortId = temp_id,
  overwrite = TRUE
)

# march - april
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = paste0(prefix, "hcq_time"),
  cohortDateRange = c(as.Date("2020-03-01"), as.Date("2020-04-30")),
  ageGroup = list(c(0,150)),
  sex = c("Both"),
  daysPriorObservation = 365,                                           
  targetCohortTable = study_table_name,
  targetCohortId = temp_id,
  overwrite = TRUE
)

# may - end
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = paste0(prefix, "post_hcq"),
  cohortDateRange = c(as.Date("2020-05-01"), study.end),
  ageGroup = list(c(0,150)), 
  sex = c("Both"),
  daysPriorObservation = 365,                                           
  targetCohortTable = study_table_name,
  targetCohortId = temp_id,
  overwrite = TRUE
)

# incidence estimates ----
# pre
if (!is.null(filterCohort("pre_covid"))) {
  inc_pre <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "pre_covid",
    denominatorCohortId = filterCohort("pre_covid"),
    outcomeTable = users_table_name,
    interval = c("overall"),
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  ) %>%
    mutate(window = "pre_covid")
} else {
  inc_pre <- NULL
}


# hcq
if (!is.null(filterCohort("hcq_time"))) {
  inc_hcq <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "hcq_time",
    denominatorCohortId = filterCohort("hcq_time"),
    outcomeTable = users_table_name,
    interval = c("overall"),
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  ) %>%
    mutate(window = "hcq_time")
} else {
  inc_hcq <- NULL
}

# post
if (!is.null(filterCohort("post_hcq"))) {
  inc_post <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "post_hcq",
    denominatorCohortId = filterCohort("post_hcq"),
    outcomeTable = users_table_name,
    interval = c("overall"),
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  ) %>%
    mutate(window = "post_hcq")
} else {
  inc_post <- NULL
}

estimates_overall <- inc_pre %>%
  union_all(inc_hcq) %>%
  union_all(inc_post)

write_csv(estimates_overall, file = here(output_folder, "overall_window_estimates.csv"))

