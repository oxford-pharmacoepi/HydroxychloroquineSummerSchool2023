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

# RA denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_ra_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorObservation = 365,                                           
  targetCohortTable = study_table_name,
  targetCohortId = ra_id,
  overwrite = TRUE
)
exportAttrition(cdm[[ip_ra_table_name]], here(output_folder, "attrition_ip_ra_population.csv"))

# RA no Covid denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_ra_no_covid_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorObservation = 365,                                           
  targetCohortTable = study_table_name,
  targetCohortId = ra_no_covid_id,
  overwrite = TRUE
)
exportAttrition(cdm[[ip_ra_no_covid_table_name]], here(output_folder, "attrition_ip_ra_no_covid_population.csv"))

# SLE denominator 
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_sle_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorObservation = 365,                                           
  targetCohortTable = study_table_name,
  targetCohortId = sle_id,
  overwrite = TRUE
)
exportAttrition(cdm[[ip_sle_table_name]], here(output_folder, "attrition_ip_sle_population.csv"))

# SLE no covid denominator 
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_sle_no_covid_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorObservation = 365,                                           
  targetCohortTable = study_table_name,
  targetCohortId = sle_no_covid_id,
  overwrite = TRUE
)
exportAttrition(cdm[[ip_sle_no_covid_table_name]], here(output_folder, "attrition_ip_sle_no_covid_population.csv"))

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

incidence  <- NULL
prevalence <- NULL
if (!is.null(filterCohort(ip_general_table_name))) {
  ip_general <- getIncidencePrevalence(ip_general_table_name)
  incidence  <- incidence %>% 
    union_all(ip_general$incidence %>%
                mutate(denominator_target_cohort_name = "general"))
  prevalence  <- prevalence %>% 
    union_all(ip_general$prevalence %>%
                mutate(denominator_target_cohort_name = "general"))
}
if (!is.null(filterCohort(ip_ra_table_name))) {
  ip_ra <- getIncidencePrevalence(ip_ra_table_name)
  incidence  <- incidence %>% union_all(ip_ra$incidence)
  prevalence  <- prevalence %>% union_all(ip_ra$prevalence)
}
if (!is.null(filterCohort(ip_ra_no_covid_table_name))) {
  ip_ra_no_covid <- getIncidencePrevalence(ip_ra_no_covid_table_name)
  incidence  <- incidence %>% union_all(ip_ra_no_covid$incidence)
  prevalence  <- prevalence %>% union_all(ip_ra_no_covid$prevalence)
}
if (!is.null(filterCohort(ip_sle_table_name))) {
  ip_sle <- getIncidencePrevalence(ip_sle_table_name)
  incidence  <- incidence %>% union_all(ip_sle$incidence)
  prevalence  <- prevalence %>% union_all(ip_sle$prevalence)
}
if (!is.null(filterCohort(ip_sle_no_covid_table_name))) {
  ip_sle_no_covid <- getIncidencePrevalence(ip_sle_no_covid_table_name)
  incidence  <- incidence %>% union_all(ip_sle_no_covid$incidence)
  prevalence  <- prevalence %>% union_all(ip_sle_no_covid$prevalence)
}

# Export incidence results
write_csv(incidence, file = here(output_folder, "incidence.csv"))
# Export prevalence results
write_csv(prevalence, file = here(output_folder, "prevalence.csv"))


## Table estimates by window ----
getWindowEstimates <- function(temp_id) {
  # denominators ----
  if (!is.null(temp_id)) {
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
      name = "hcq_time",
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
      name = "post_hcq",
      cohortDateRange = c(as.Date("2020-05-01"), study.end),
      ageGroup = list(c(0,150)), 
      sex = c("Both"),
      daysPriorObservation = 365,                                           
      targetCohortTable = study_table_name,
      targetCohortId = temp_id,
      overwrite = TRUE
    )
  } else {
    # pre-covid
    cdm <- generateDenominatorCohortSet(
      cdm = cdm,
      name = "pre_covid",
      cohortDateRange = c(study.start, as.Date("2020-02-29")),
      ageGroup = list(c(0,150)), 
      sex = c("Both"),
      daysPriorObservation = 365, 
      overwrite = TRUE
    )
    
    # march - april
    cdm <- generateDenominatorCohortSet(
      cdm = cdm,
      name = "hcq_time",
      cohortDateRange = c(as.Date("2020-03-01"), as.Date("2020-04-30")),
      ageGroup = list(c(0,150)),
      sex = c("Both"),
      daysPriorObservation = 365,  
      overwrite = TRUE
    )
    
    # may - end
    cdm <- generateDenominatorCohortSet(
      cdm = cdm,
      name = "post_hcq",
      cohortDateRange = c(as.Date("2020-05-01"), study.end),
      ageGroup = list(c(0,150)), 
      sex = c("Both"),
      daysPriorObservation = 365,    
      overwrite = TRUE
    )
  }
  
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
  
  rm(cdm$pre_covid)
  rm(cdm$hcq_time)
  rm(cdm$post_hcq)
  
  return(inc_pre %>%
           union_all(inc_hcq) %>%
           union_all(inc_post))
}

# Save results
estimates_overall <- getWindowEstimates(NULL)  %>%
  union_all(getWindowEstimates(ra_no_covid_id)) %>%
  union_all(getWindowEstimates(ra_id)) %>%
  union_all(getWindowEstimates(sle_id)) %>%
  union_all(getWindowEstimates(sle_no_covid_id))

write_csv(estimates_overall, file = here(output_folder, "overall_window_estimates.csv"))

