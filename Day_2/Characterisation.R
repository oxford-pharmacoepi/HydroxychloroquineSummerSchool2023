## Variables ----
# Names of already insatiated cohort tables
study_table_name         <- paste0(stem_table, "_study")
hcq_new_users_table_name <- paste0(stem_table, "_hcq_new")

table_names <- c(study_table_name, hcq_new_users_table_name)

# Connect to the database and call the cohorts:
cdm <- cdmFromCon(
  con = db, 
  cdmSchema = cdm_database_schema,
  writeSchema = results_database_schema,
  cdmName = db_name,
  cohortTables = table_names
)


## Prepare data ----
# Subset of cdm (we are characterising new users of HCQ)
cdm_hcq <- CDMConnector::cdm_subset_cohort(cdm, hcq_new_users_table_name)

# Add calendar windows to new user cohorts:
cdm_hcq[[hcq_new_users_table_name]] <- cdm_hcq[[hcq_new_users_table_name]] %>%
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
medications_table_name_hcq <- paste0(stem_table, "_medications_hcq")
conditions_table_name_hcq  <- paste0(stem_table, "_conditions_hcq")

# Get concept sets
medications_concept_list <- readConceptList(
  cdm_hcq,
  path = here("Day_2", "TableCharacteristics", "MedicationsConceptSet")
)
conditions_concept_list <- readConceptList(
  cdm_hcq,
  path = here("Day_2", "TableCharacteristics", "GeneralConditionsConceptSet")
)

# Generate cohorts in cdm
cdm_hcq <- generateConceptCohortSet(cdm_hcq,
                                    medications_table_name_hcq,
                                    medications_concept_list)

cdm_hcq <- generateConceptCohortSet(cdm_hcq,
                                    conditions_table_name_hcq,
                                    conditions_concept_list)



## Characterisation ----
## 1. Indication ----
hcq_indication_table <- cdm_hcq[[hcq_new_users_table_name]] %>%
  addIndication(cdm = cdm_hcq, 
                indicationCohortName = study_table_name, 
                indicationGap = c(0, 7, 30)) # Indication at 0, 7, and 30 days

result_indication_hcq <- summariseIndication(cohort = hcq_indication_table, 
                                             cdm = cdm_hcq, 
                                             strata = list("Calendar time" = "window"))

write_csv(result_indication_hcq, here(output_folder, "indication_hcq.csv"))

## 2. Table One ----
# Package: PatientProfiles
hcq_new_users_table_one <- cdm_hcq[[hcq_new_users_table_name]] %>% 
  select(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "window"
  ) %>%
  addDemographics(
    cdm = cdm_hcq, 
    ageGroup = age_groups
    ) %>%
  addIntersect(
    cdm = cdm_hcq, 
    tableName = "visit_occurrence", 
    value = "flag", 
    window = c(-365,0),
    targetEndDate = NULL, 
    nameStyle = "number_visits"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm_hcq,
    targetCohortTable = medications_table_name_hcq,
    window = c(-365, 0),
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm_hcq,
    targetCohortTable = conditions_table_name_hcq,
    window = c(-Inf, 0),
  ) %>%
  left_join(cdm_hcq[[hcq_new_users_table_name]] %>%
              cohort_set(), 
            by = "cohort_definition_id", 
            copy = TRUE)

# set variables
variables <- list(
  dates = c("cohort_start_date", "cohort_end_date"),
  numeric = c(
    "age", "number_visits", "prior_history",
    "future_observation"
  ),
  categorical = c("sex", "age_group")
)

variables$covariates <- colnames(hcq_new_users_table_one)[!c(colnames(hcq_new_users_table_one) %in% 
                                                               c("subject_id", 
                                                                 "cohort_definition_id", 
                                                                 unlist(variables), 
                                                                 medications_table_name_hcq,
                                                                 conditions_table_name_hcq))]

# set functions
functions <- list(
  dates = c("median", "q25", "q75"),
  numeric = c("median", "q25", "q75"),
  categorical = c("count", "%"),
  covariates = c("count", "%")
)

# summarise results
hcq_new_users_table_one <- hcq_new_users_table_one %>%
  summariseResult(
    group = list("Cohort name" = "cohort_name"), 
    strata = list("Calendar time" = "window"),
    variables = variables, 
    functions = functions, 
    minCellCount = minimum_counts
  ) %>%
  mutate(
    cdm_name = cdmName(cdm))

write_csv(hcq_new_users_table_one, here(output_folder, "table_one_hcq.csv"))

## 3. Large Scale Characteristics ----
# Get ATC and ICD10 codes (package CodelistGenerator)
atc_codes   <- getATCCodes(cdm_hcq, "ATC 3rd")
icd10_codes <- getICD10StandardCodes(cdm_hcq, "ICD10 SubChapter")

# Characteristics (package DrugUtilisation)
# ATC
result_ATC_hcq <- summariseCharacteristicsFromCodelist(
  cohort = cdm_hcq[[hcq_new_users_table_name]],
  cdm = cdm_hcq,
  conceptSetList = atc_codes,
  strata = list("Calendar time" = "window"),
  window = list(c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365)),
  overlap = TRUE,
  minCellCount = minimum_counts
)
write_csv(result_ATC_hcq, here(output_folder, "characteristics_atc_hcq.csv"))

# ICD10:
result_ICD10_hcq <- summariseCharacteristicsFromCodelist(
  cohort = cdm_hcq[[hcq_new_users_table_name]],
  cdm = cdm_hcq,
  conceptSetList = icd10_codes,
  strata = list("Calendar time" = "window"),
  window = list(c(-Inf, -1), c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365), c(1, Inf)),
  overlap = FALSE,
  minCellCount = minimum_counts
)
write_csv(result_ICD10_hcq, here(output_folder, "characteristics_icd10_hcq.csv"))

