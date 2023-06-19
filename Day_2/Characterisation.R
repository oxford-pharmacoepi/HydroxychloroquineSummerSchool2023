cdm <- cdmSubsetCohort(cdm, new_users_table_name)

## Prepare data ----
# Add calendar windows to new user cohorts
cdm[[new_users_table_name]] <- cdm[[new_users_table_name]] %>%
  mutate(window = case_when(
    .data$cohort_start_date >= !! window.before[1] & .data$cohort_start_date <= !! window.before[2] ~
      "before",
    .data$cohort_start_date >= !! window.hcq[1] & .data$cohort_start_date <= !! window.hcq[2] ~
      "during",
    .data$cohort_start_date >= !! window.after[1] & .data$cohort_start_date <= !! window.after[2] ~
      "after",
    .default = NA
  )) %>%
  filter(!is.na(window))

# Instantiate medications and general conditions for Table One
# Names for the instantiated cohorts
medications_table_name <- paste0(stem_table, "_medications")
conditions_table_name  <- paste0(stem_table, "_conditions")

# Get concept sets
medications_concept_list <- readConceptList(
  cdm,
  path = here("Day_2", "TableCharacteristics", "MedicationsConceptSet")
)
conditions_concept_list <- readConceptList(
  cdm,
  path = here("Day_2", "TableCharacteristics", "GeneralConditionsConceptSet")
)

# Generate cohorts in cdm
cdm <- generateConceptCohortSet(cdm,
                                medications_table_name,
                                medications_concept_list)

cdm <- generateConceptCohortSet(cdm,
                                conditions_table_name,
                                conditions_concept_list)


## Characterisation ----
## 1. Indication ----
indication_table <- cdm[[new_users_table_name]] %>%
  addIndication(
    cdm = cdm, 
    indicationCohortName = study_table_name, 
    indicationGap = c(0, 7, 30, 365$)
  ) %>%
  summariseIndication(
    cdm = cdm, 
    strata = list("Calendar time" = "window")
  )

write_csv(indication_table, here(output_folder, "indication.csv"))

## 2. Drug use ----
#  2.1. HCQ drug use
drug_use_table_hcq <- cdm[[new_users_table_name]] %>%
  filter(cohort_definition_id == 1) %>%
  addDrugUse(cdm = cdm, ingredientConceptId = 1777087) %>%
  summariseDrugUse(
    cdm = cdm, 
    drugUseEstimates = c("median", "q25", "q75", "mean", "sd"), 
    strata = list("Calendar time" = "window")
  )
drug_use_table_mtx <- cdm[[new_users_table_name]] %>%
  filter(cohort_definition_id == 2) %>%
  addDrugUse(cdm = cdm, ingredientConceptId = 1305058) %>%
  summariseDrugUse(
    cdm = cdm, 
    drugUseEstimates = c("median", "q25", "q75", "mean", "sd"), 
    strata = list("Calendar time" = "window")
  )

drug_use_table <- drug_use_table_hcq %>% union_all(drug_use_table_mtx)

write_csv(drug_use_table, here(output_folder, "drug_use.csv"))

## 3. Table One ----
# Package: PatientProfiles

#  3.1. HCQ new users
table_one <- cdm[[new_users_table_name]] %>% 
  select(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "window"
  ) %>%
  addDemographics(
    cdm = cdm, 
    ageGroup = list(c(0,19), c(20,39), c(40,59), c(60,79), c(80,150))
  ) %>%
  addIntersect(
    cdm = cdm, 
    tableName = "visit_occurrence", 
    value = "flag", 
    window = c(-365,0),
    targetEndDate = NULL, 
    nameStyle = "number_visits"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = medications_table_name,
    window = c(-365, 0),
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = conditions_table_name,
    window = c(-Inf, 0),
  ) %>%
  left_join(
    cdm[[new_users_table_name]] %>%
      cohort_set() %>%
      select("cohort_definition_id", "cohort_name"), 
    by = "cohort_definition_id", 
    copy = TRUE
  )

variables <- list(
  dates = c("cohort_start_date", "cohort_end_date"),
  numeric = c(
    "age", "number_visits", "prior_history",
    "future_observation"
  ),
  categorical = c("sex", "age_group", "window")
)
variables$covariates <- colnames(table_one)[
  !c(colnames(table_one) %in% c(
    "subject_id", "cohort_definition_id", "cohort_name", unlist(variables))
  )
]

# set functions
functions <- list(
  dates = c("median", "q25", "q75"),
  numeric = c("median", "q25", "q75"),
  categorical = c("count", "%"),
  covariates = c("count", "%")
)

table_one <- table_one %>%
  summariseResult(
    group = list("Cohort name" = "cohort_name"), 
    strata = list("Calendar time" = "window"),
    variables = variables, 
    functions = functions, 
    minCellCount = minimum_counts
  ) %>%
  mutate(
    cdm_name = cdmName(cdm))

# Export
write_csv(table_one, here(output_folder, "table_one.csv"))

## 4. Large Scale Characteristics ----
# Get ATC and ICD10 codes (package CodelistGenerator)
atc_codes   <- getATCCodes(cdm, "ATC 3rd")
icd10_codes <- getICD10StandardCodes(cdm, "ICD10 SubChapter")

# Characteristics (package DrugUtilisation)
#  4.1. ATC 
result_ATC <- summariseCharacteristicsFromCodelist(
  cohort = cdm[[new_users_table_name]],
  cdm = cdm,
  conceptSetList = atc_codes,
  strata = list("Calendar time" = "window"),
  window = list(c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365)),
  overlap = TRUE,
  minCellCount = minimum_counts
)
write_csv(result_ATC, here(output_folder, "characteristics_atc.csv"))

#  4.2. ICD10 
result_ICD10 <- summariseCharacteristicsFromCodelist(
  cohort = cdm[[new_users_table_name]],
  cdm = cdm,
  conceptSetList = icd10_codes,
  strata = list("Calendar time" = "window"),
  window = list(c(-Inf, -1), c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365), c(1, Inf)),
  overlap = FALSE,
  minCellCount = minimum_counts
)
write_csv(result_ICD10, here(output_folder, "characteristics_icd10.csv"))

