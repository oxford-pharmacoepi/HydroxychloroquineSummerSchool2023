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
  primary_class = "summary_codelist",
  secondary_class = c("atc_summary", "icd10_summary"),
  distinguish_content = c("summariseCodelistATC", "summariseCodelistICD10"),
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
displayCdmSnapshot <- function(elements) {
  lapply(getElementType(elements, "cdm_snapshot"), function(x) {
    tidyr::pivot_wider(x, names_from = "attribute", values_from = "value")
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      person_cnt = niceNum(.data$person_cnt), 
      observation_period_cnt = niceNum(.data$observation_period_cnt)
    ) %>%
    dplyr::select(
      "CDM name" = "cdm_name", "Source name" = "cdm_source_name", 
      "CDM version" = "cdm_version", "Holder" = "cdm_holder", 
      "Relesae date" = "cdm_release_date", 
      "Vocabulary version" = "vocabulary_version", 
      "Number individuals" = "person_cnt", 
      "Number observation periods" = "observation_period_cnt"
    )
}
displayCohortCount <- function(elements) {
  getElementType(elements, "cohort_count") %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      number_records = niceNum(.data$number_records),
      number_subjects = niceNum(.data$number_subjects)
    ) %>%
    dplyr::select("cohort_name", "number_records", "number_subjects")
}
displayCohortAttrition <- function(elements) {
  getElementType(elements, "cohort_attrition") %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      number_records = niceNum(.data$number_records),
      number_subjects = niceNum(.data$number_subjects),
      excluded_records = niceNum(.data$excluded_records),
      excluded_subjects = niceNum(.data$excluded_subjects)
    ) %>%
    dplyr::arrange(.data$cohort_name, .data$reason_id) %>%
    dplyr::select(
      "cohort_name", "number_records", "number_subjects", "reason_id", "reason",
      "excluded_records", "excluded_subjects"
    )
}
displayDenominatorAttrition <- function(denominatorAttrition, 
                                        ageGroup, 
                                        sex, 
                                        daysPriorHistory, 
                                        startDate,
                                        endDate,
                                        strataCohortName) {
  denominatorAttrition %>%
    dplyr::filter(.data$age_group == .env$ageGroup) %>%
    dplyr::filter(.data$sex == .env$sex) %>%
    dplyr::filter(.data$days_prior_history == .env$daysPriorHistory) %>%
    dplyr::filter(.data$start_date == .env$startDate) %>%
    dplyr::filter(.data$end_date == .env$endDate) %>%
    dplyr::filter(.data$strata_cohort_name == .env$strataCohortName) %>%
    dplyr::arrange(as.numeric(.data$reason_id)) %>%
    dplyr::select(
      "reason_id", "reason", "number_records", "number_subjects", 
      "excluded_records", "excluded_subjects"
    )
}
displayIncidence <- function(incidence, 
                             ageGroup,
                             sex,
                             daysPriorHistory,
                             strataCohortName,
                             incidenceStartDate) {
  incidence %>%
    dplyr::filter(.data$denominator_age_group %in%.env$ageGroup) %>%
    dplyr::filter(.data$denominator_sex %in% .env$sex) %>%
    dplyr::filter(.data$denominator_days_prior_history %in% .env$daysPriorHistory) %>%
    dplyr::filter(.data$denominator_strata_cohort_name %in% .env$strataCohortName) %>%
    dplyr::filter(.data$incidence_start_date %in% .env$incidenceStartDate) %>%
    dplyr::mutate(
      incidence_100000_pys = as.numeric(incidence_100000_pys),
      incidence_100000_pys_95CI_lower = as.numeric(incidence_100000_pys_95CI_lower),
      incidence_100000_pys_95CI_upper = as.numeric(incidence_100000_pys_95CI_upper)
    ) %>%
    dplyr::mutate(
      Incidence = paste0(
        round(incidence_100000_pys), " [", 
        round(incidence_100000_pys_95CI_lower), " - ", 
        round(incidence_100000_pys_95CI_upper), "]"
      )
    ) %>%
    dplyr::select(c(
      "incidence_start_date", "incidence_end_date", "Number individuals" = "n_persons",
      "Number events" = "n_events", "Incidence", "denominator_age_group",
      "denominator_sex", "denominator_strata_cohort_name"
    ))
}
displayTableOne <- function(elements) {
  x <- getElementType(elements, "table_characteristics") %>%
    dplyr::bind_rows() %>%
    dplyr::select(-c("group_name", "group_level", "strata_name", "cdm_name")) %>%
    dplyr::mutate(estimate = niceNum(.data$estimate, significativeDecimals = 0)) %>%
    dplyr::mutate(estimate = gsub(" ", "", .data$estimate)) %>%
    tidyr::pivot_wider(names_from = "estimate_type", values_from = "estimate") %>%
    dplyr::mutate(
      "count (%)" = dplyr::if_else(!is.na(.data[["%"]]), paste0(count, " (", `%`, "%)"), as.character(NA)),
      "median [Q25 - Q75]" = dplyr::if_else(!is.na(.data$median), paste0(median, " [", q25, " - ", q75, "]"), as.character(NA)),
      count = dplyr::if_else(!is.na(.data[["%"]]), as.character(NA), count)
    ) %>%
    dplyr::select(-c("median", "q25", "q75", "%")) %>%
    tidyr::pivot_longer(c("count", "count (%)", "median [Q25 - Q75]"), names_to = "estimate_type", values_to = "estimate") %>%
    dplyr::filter(!is.na(.data$estimate)) %>%
    tidyr::pivot_wider(names_from = "strata_level", values_from = "estimate")
  x1 <- x %>%
    dplyr::filter(!grepl("_m365_to_0|_minf_to_0", .data$variable))
  x2 <- x %>%
    dplyr::filter(grepl("_minf_to_0", .data$variable))
  x3 <- x %>%
    dplyr::filter(grepl("_m365_to_0", .data$variable))
  x <- x1 %>%
    dplyr::bind_rows(dplyr::tibble(variable = "Conditions any time prior")) %>%
    dplyr::union_all(
      x2 %>%
        dplyr::mutate(variable = gsub("_minf_to_0", "", .data$variable))
    ) %>%
    dplyr::bind_rows(dplyr::tibble(variable = "Medications prior year")) %>%
    dplyr::union_all(
      x3 %>%
        dplyr::mutate(variable = gsub("_m365_to_0", "", .data$variable))
    ) %>%
    dplyr::select(
      c("variable", "variable_level", "estimate_type", "Overall", "before", "during", "after")
    )
  return(x)
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
