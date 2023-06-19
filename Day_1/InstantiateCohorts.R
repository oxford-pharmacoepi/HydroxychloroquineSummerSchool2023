## Variables ----
# Names for cohort tables
study_table_name         <- paste0(stem_table, "_study")
new_users_table_name <- paste0(stem_table, "_new")
prevalent_users_table_name     <- paste0(stem_table, "_prevalent")

# Study dates
study.start <- as.Date("2019-01-01")
study.end   <- as.Date("2022-06-01")


## Instantiate JSON files----
# Package: CDMConnector
json_cohort_set <- readCohortSet(
  path = here("Day_1", "Cohorts")
)

# Instantiate cohorts from json
cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = json_cohort_set,
  name = study_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)

# Export cohort counts
write_csv(
  cohortCount(cdm[[study_table_name]]) %>%
    left_join(cohortSet(cdm[[study_table_name]]), by = "cohort_definition_id"), 
  file = here(output_folder, "cohort_count_json.csv")
)

# Export attrition
write_csv(
  cohortAttrition(cdm[[study_table_name]]) %>%
    left_join(cohortSet(cdm[[study_table_name]]), by = "cohort_definition_id"), 
  file = here(output_folder, "cohort_attrition_json.csv")
)

## Instantiate hydroxychloroquine and methotrexate ----
# Package: CodelistGenerator + DrugUtilisation
cl <- getDrugIngredientCodes(cdm, c("hydroxychloroquine", "methotrexate"))

conceptList <- list()
conceptList$prevalent_users_hydroxychloroquine <- cl$`Ingredient: hydroxychloroquine (1777087)`
conceptList$prevalent_users_methotrexate <- cl$`Ingredient: methotrexate (1305058)`

# prevalent users
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = prevalent_users_table_name,
  conceptSetList = conceptList,
  summariseMode = "AllEras", 
  gapEra = 30
)

# Export cohort counts
write_csv(
  cohortCount(cdm[[prevalent_users_table_name]]) %>%
    left_join(cohortSet(cdm[[prevalent_users_table_name]])), 
  file = here(output_folder, "cohort_count_prevalent.csv")
)
# Export attrition
write_csv(
  cohortAttrition(cdm[[prevalent_users_table_name]]) %>%
    left_join(cohortSet(cdm[[prevalent_users_table_name]])), 
  file = here(output_folder, "cohort_attrition_prevalent.csv")
)

conceptList <- list()
conceptList$new_users_hydroxychloroquine <- cl$`Ingredient: hydroxychloroquine (1777087)`
conceptList$new_users_methotrexate <- cl$`Ingredient: methotrexate (1305058)`

# New users
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = new_users_table_name,
  conceptSetList = conceptList,
  summariseMode = "FirstEra",
  daysPriorHistory = 365,
  gapEra = 30,
  priorUseWashout = 365,
  cohortDateRange = as.Date(c(study.start, study.end))
)

# Export cohort counts
write_csv(
  cohortCount(cdm[[new_users_table_name]]) %>%
    left_join(cohortSet(cdm[[new_users_table_name]])), 
  file = here(output_folder, "cohort_count_new.csv")
)
# Export attrition
write_csv(
  cohortAttrition(cdm[[new_users_table_name]]) %>%
    left_join(cohortSet(cdm[[new_users_table_name]])), 
  file = here(output_folder, "cohort_attrition_new.csv")
)
