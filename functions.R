# generate .csv of the cohort's attrition (+ cohort set)
exportAttrition <- function(cohort, path) {
  cohort %>%
    cohort_set() %>%
    select(-"cohort_name") %>%
    inner_join(cohort %>%
                 cohort_attrition(),
               by = "cohort_definition_id") %>%
    write_csv(file = path)
}

# get incidence and prevalence 
getIncidencePrevalence <- function(denominator_table_name, output_table_name) {
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    outcomeTable = output_table_name,
    interval = "months",
    outcomeWashout = 365,
    repeatedEvents = FALSE,
    minCellCount = minimum_counts
  )
  
  prev <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = denominator_table_name,
    outcomeTable = output_table_name,
    interval = "months",
    minCellCount = minimum_counts
  )
  
  return(list("incidence" = inc, "prevalence" = prev))
}
