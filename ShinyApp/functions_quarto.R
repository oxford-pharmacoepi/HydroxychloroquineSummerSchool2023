classes <- dplyr::tibble(
  primary_class = "cdm_snapshot",
  column_name = c("attribute", "value")
) %>%
  dplyr::union_all(dplyr::tibble(
    primary_class = "summary_codelist",
    column_name = c(
      "group_name", "group_level", "strata_name", "strata_level", "window_name",
      "variable", "variable_type", "variable_level", "estimate_type", 
      "estimate", "cdm_name", "generated_by"
    )
  )) %>%
  dplyr::union_all(dplyr::tibble(
    primary_class = "table_characteristics",
    column_name = c(
      "group_name", "group_level", "strata_name", "strata_level", "variable",
      "variable_level", "variable_type", "estimate_type", "estimate", "cdm_name"
    )
  )) %>%
  dplyr::union_all(dplyr::tibble(
    primary_class = "summary",
    column_name = c(
      "cdm_name", "estimate", "estimate_type", "generated_by", "group_level",
      "group_name", "strata_level", "strata_name", "variable", "variable_level",
      "variable_type" 
    )
  )) %>%
  dplyr::union_all(dplyr::tibble(
    primary_class = "denominator_attrition",
    column_name = c(
      "cohort_definition_id", "age_group", "sex", "days_prior_history", 
      "start_date", "end_date", "strata_cohort_definition_id", 
      "strata_cohort_name", "number_records", "number_subjects", "reason_id",
      "reason", "excluded_records", "excluded_subjects"
    )
  )) %>%
  dplyr::union_all(dplyr::tibble(
    primary_class = "incidence_estimates",
    column_name = c(
      "analysis_id", "n_persons", "person_days", "n_events", 
      "incidence_start_date", "incidence_end_date", "person_years", 
      "incidence_100000_pys", "incidence_100000_pys_95CI_lower", 
      "incidence_100000_pys_95CI_upper", "cohort_obscured", "result_obscured",
      "outcome_cohort_id", "outcome_cohort_name", "analysis_outcome_washout", 
      "analysis_repeated_events", "analysis_interval", 
      "analysis_complete_database_intervals", "denominator_cohort_id",
      "analysis_min_cell_count", "denominator_cohort_name", 
      "denominator_age_group", "denominator_sex", 
      "denominator_days_prior_history", "denominator_start_date",
      "denominator_end_date", "denominator_strata_cohort_definition_id",
      "denominator_strata_cohort_name", "cdm_name"
    )
  )) %>%
  dplyr::union_all(dplyr::tibble(
    primary_class = "prevalence_estimates",
    column_name = c(
      "analysis_id", "prevalence_start_date", "prevalence_end_date", "n_cases",
      "n_population", "prevalence", "prevalence_95CI_lower", 
      "prevalence_95CI_upper", "cohort_obscured", "result_obscured",
      "outcome_cohort_id", "outcome_cohort_name", 
      "analysis_outcome_lookback_days", "analysis_type", "analysis_interval",
      "analysis_complete_database_intervals", "analysis_time_point",
      "analysis_full_contribution", "analysis_min_cell_count",
      "denominator_cohort_id", "denominator_cohort_name", 
      "denominator_age_group", "denominator_sex", 
      "denominator_days_prior_history", "denominator_start_date", 
      "denominator_end_date", "denominator_strata_cohort_definition_id",
      "denominator_strata_cohort_name", "cdm_name"
    )
  )) %>%
  dplyr::union_all(dplyr::tibble(
    primary_class = "cohort_count",
    column_name = c(
      "cohort_name", "cohort_definition_id", "number_records", "number_subjects"
    )
  )) %>%
  dplyr::union_all(dplyr::tibble(
    primary_class = "cohort_attrition",
    column_name = c(
      "cohort_definition_id", "number_records", "number_subjects", "reason_id",
      "reason", "excluded_records", "excluded_subjects", "cohort_name"
    )
  )) 

subClasses <- dplyr::tibble(
  primary_class = c("summary_codelist", "summary_codelist", "summary", "summary"),
  secondary_class = c("atc_summary", "icd10_summary", "indication", "drug_use"),
  distinguish_content = c("summariseCodelistATC", "summariseCodelistICD10", "summariseIndication", "summariseDrugUse"),
  distinguish_variable = "generated_by"
)

identifyClass <- function(x) {
  cl <- sort(colnames(x))
  idClass <- NULL
  for (checkClass in unique(classes$primary_class)) {
    columns <- sort(classes$column_name[classes$primary_class == checkClass])
    if (length(cl) == length(columns)){
      if (all(cl == columns)) {
        idClass <- checkClass
        break
      }
    }
  }
  if (!is.null(idClass) & idClass %in% subClasses$primary_class) {
    x <- utils::head(x, 1)
    options <- subClasses %>%
      dplyr::filter(.data$primary_class == idClass)
    for (i in 1:nrow(options)) {
      distinguishVariable <- options$distinguish_variable[i]
      distinguishContent <- options$distinguish_content[i]
      if (grepl(distinguishContent, x[[distinguishVariable]])) {
        return(options$secondary_class[i])
      }
    }
  }
  return(idClass)
}
readFiles <- function(path) {
  files <- list.files(path, full.names = TRUE)
  result <- list()
  settings <- dplyr::tibble(
    name = character(), path = character(), class = character()
  )
  for (k in seq_along(files)) {
    if (tools::file_ext(files[k]) == "csv") {
      x <- readr::read_csv(
        files[k], col_types = readr::cols(.default = readr::col_character())
      )
      name <- tools::file_path_sans_ext(basename(files[k]))
      result[[name]] <- x
      settings <- settings %>%
        dplyr::union_all(dplyr::tibble(
          name = name, path = files[k], class = identifyClass(x)
        ))
    }
  }
  attr(result, "settings") <- settings
  return(result)
}

getElementType <- function(elements, type) {
  elementsType <- attr(elements, "settings") %>%
    dplyr::filter(.data$class == .env$type) %>%
    dplyr::pull("name")
  return(elements[names(elements) %in% elementsType])
}

niceNum <- function(x, bigMark = ",", decimalMark = ".", significativeDecimals = 0) {
  suppressWarnings(y <- as.numeric(x))
  xnew <- base::format(
    round(y, significativeDecimals),
    big.mark = bigMark,
    decimal.mark = decimalMark,
    nsmall = significativeDecimals
  )
  x[!is.na(y)] <- xnew[!is.na(y)]
  return(x)
}
