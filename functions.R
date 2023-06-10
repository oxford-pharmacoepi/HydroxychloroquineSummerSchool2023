# generate .csv of the cohort's attrition (+ cohort set)
exportAttrition <- function(cohort, path) {
  cohort %>%
    cohort_set() %>%
    select(-"cohort_name") %>%
    inner_join(cohort %>%
                 cohort_attrition()) %>%
    write_csv(file = path)
}

# get HCQ incidence and prevalence on a population
getHCQIncidencePrevalence <- function(denominator_cohort_name) {
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = denominator_cohort_name,
    outcomeTable = hcqTableName,
    interval = "months",
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  )
  
  prev <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = denominator_cohort_name,
    outcomeTable = hcqTableName,
    interval = "months",
    minCellCount = minimum_counts
  )
  
  return(list("incidence" = inc, "prevalence" = prev))
}