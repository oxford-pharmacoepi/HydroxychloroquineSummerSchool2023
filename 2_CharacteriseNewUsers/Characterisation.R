## Prepare data ----
# Add calendar windows + indication to new user cohorts
cdm[[new_users_table_name]] <- cdm[[new_users_table_name]] %>%
  mutate(window = case_when(
    .data$cohort_start_date >= !! window.before[1] & .data$cohort_start_date <= !! window.before[2] ~
      "before_pandemic",
    .data$cohort_start_date >= !! window.hcq[1] & .data$cohort_start_date <= !! window.hcq[2] ~
      "during_hcq_use_for_covid",
    .data$cohort_start_date >= !! window.after[1] & .data$cohort_start_date <= !! window.after[2] ~
      "after_fda_retraction",
    .default = NA
    )
  ) %>%
  filter(!is.na(window)) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = study_table_name,
    targetCohortId = covid_id,
    window = c(-21,0),
    nameStyle = "covid"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = study_table_name,
    targetCohortId = ra_id,
    window = c(-Inf,0),
    nameStyle = "rheumatoid_arthritis"
  ) %>%
  addCohortIntersectFlag(
    cdm = cdm,
    targetCohortTable = study_table_name,
    targetCohortId = sle_id,
    window = c(-Inf,0),
    nameStyle = "sle"
  ) %>%
  mutate(
    indication = case_when(
      covid == 1 & rheumatoid_arthritis == 0 & sle == 0 ~ "covid",
      covid == 0 & rheumatoid_arthritis == 1 & sle == 0 ~ "rheumatoid_arthritis",
      covid == 0 & rheumatoid_arthritis == 0 & sle == 1 ~ "sle",
      covid == 0 & rheumatoid_arthritis == 0 & sle == 0 ~ "none",
      .default = "multiple"
    )
  ) %>%
  select(-c("covid", "rheumatoid_arthritis", "sle"))
  
# Instantiate medications and general conditions for Table One
# Names for the instantiated cohorts
medications_table_name <- paste0(stem_table, "_medications")
conditions_table_name  <- paste0(stem_table, "_conditions")

# Get concept sets
medications_concept_list <- readConceptList(
  cdm,
  path = here("2_CharacteriseNewUsers", "TableCharacteristics", "MedicationsConceptSet")
)
conditions_concept_list <- readConceptList(
  cdm,
  path = here("2_CharacteriseNewUsers", "TableCharacteristics", "GeneralConditionsConceptSet")
)

# Generate cohorts in cdm
cdm <- generateConceptCohortSet(cdm,
                                       medications_table_name,
                                       medications_concept_list)

cdm <- generateConceptCohortSet(cdm,
                                       conditions_table_name,
                                       conditions_concept_list)


## Characterisation ----
## 1. Drug use ----
#  1.1. HCQ drug use
drug_use_table_hcq <- cdm[[new_users_table_name]] %>%
  filter(cohort_definition_id == 1) %>%
  addDrugUse(cdm = cdm, ingredientConceptId = 1777087) %>%
  summariseDrugUse(
    cdm = cdm, 
    drugUseEstimates = c("median", "q25", "q75", "mean", "sd"), 
    strata = list("Calendar time" = "window", 
                  "Indication" = "indication")
  )
drug_use_table_mtx <- cdm[[new_users_table_name]] %>%
  filter(cohort_definition_id == 2) %>%
  addDrugUse(cdm = cdm, ingredientConceptId = 1305058) %>%
  summariseDrugUse(
    cdm = cdm, 
    drugUseEstimates = c("median", "q25", "q75", "mean", "sd"), 
    strata = list("Calendar time" = "window",
                  "Indication" = "indication")
  )

write_csv(drug_use_table_hcq %>% 
            union_all(drug_use_table_mtx), here(output_folder, "drug_use.csv"))

## 2. Table One ----
# Package: PatientProfiles
#  2.1. HCQ new users
table_one <- cdm[[new_users_table_name]] %>% 
  select(
    "cohort_definition_id", "subject_id", "cohort_start_date",
    "cohort_end_date", "window", "indication"
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
  categorical = c("sex", "age_group", "window", "indication")
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
    strata = list("Calendar time" = "window",
                  "Indication" = "indication"),
    variables = variables, 
    functions = functions, 
    minCellCount = minimum_counts
  ) %>%
  mutate(cdm_name = cdmName(cdm))

# Export
write_csv(table_one, here(output_folder, "table_one.csv"))

## 3. Large Scale Characteristics ----
# Get ATC and ICD10 codes (package CodelistGenerator)
# atc_codes   <- getATCCodes(cdm, "ATC 3rd")
# icd10_codes <- getICD10StandardCodes(cdm, "ICD10 SubChapter")
load(here("Data", "icd10.RData"))
load(here("Data", "atc.RData"))

# Characteristics (package DrugUtilisation)
#  3.1. ATC 
result_ATC <- summariseCharacteristicsFromCodelist(
  cohort = cdm[[new_users_table_name]],
  cdm = cdm,
  conceptSetList = atc_codes,
  strata = list("Calendar time" = "window",
                "Indication" = "indication"),
  window = list(c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365)),
  overlap = TRUE,
  minCellCount = minimum_counts
)
write_csv(result_ATC, here(output_folder, "characteristics_atc.csv"))

#  3.2. ICD10 
result_ICD10 <- summariseCharacteristicsFromCodelist(
  cohort = cdm[[new_users_table_name]],
  cdm = cdm,
  conceptSetList = icd10_codes,
  strata = list("Calendar time" = "window",
                "Indication" = "indication"),
  window = list(c(-Inf, -1), c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365), c(1, Inf)),
  overlap = FALSE,
  minCellCount = minimum_counts
)
write_csv(result_ICD10, here(output_folder, "characteristics_icd10.csv"))

