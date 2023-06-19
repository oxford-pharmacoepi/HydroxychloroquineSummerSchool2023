## Variables ----
# Names for cohort tables
study_table_name         <- paste0(stem_table, "_study")
hcq_new_users_table_name <- paste0(stem_table, "_hcq_new")
hcq_users_table_name     <- paste0(stem_table, "_hcq")
#mtx_new_users_table_name <- paste0(stem_table, "_mtx_new")
#mtx_users_table_name     <- paste0(stem_table, "_mtx")

table_names <- c(study_table_name, hcq_new_users_table_name, hcq_users_table_name,
                 mtx_new_users_table_name, mtx_users_table_name)

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

write_csv(
  cohortCount(cdm[[study_table_name]]) %>%
    left_join(cohortSet(cdm[[study_table_name]]), by = "cohort_definition_id"), 
  file = here(output_folder, "cohort_count_json.csv")
)
write_csv(
  cohortAttrition(cdm[[study_table_name]]) %>%
    left_join(cohortSet(cdm[[study_table_name]]), by = "cohort_definition_id"), 
  file = here(output_folder, "cohort_attrition_json.csv")
)

## Instantiate hydroxychloroquine ----
# Package: CodelistGenerator + DrugUtilisation
hcq_concept_list <- getDrugIngredientCodes(cdm, "hydroxychloroquine")

# Users
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = hcq_users_table_name,
  conceptSetList = hcq_concept_list,
  summariseMode = "AllEras", 
  gapEra = 30
)
write_csv(
  cohortCount(cdm[[hcq_users_table_name]]) %>%
    mutate(cohort_name = "hydroxychloriquine prevalent users"), 
  file = here(output_folder, "cohort_count_prevalent.csv")
)
write_csv(
  cohortAttrition(cdm[[hcq_users_table_name]]) %>%
    mutate(cohort_name = "hydroxychloriquine prevalent users"), 
  file = here(output_folder, "cohort_attrition_prevalent.csv")
)

# New users
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = hcq_new_users_table_name,
  conceptSetList = hcq_concept_list,
  summariseMode = "FirstEra",
  daysPriorHistory = 365,
  gapEra = 30,
  priorUseWashout = 365,
  cohortDateRange = as.Date(c(study.start, study.end))
)
write_csv(
  cohortCount(cdm[[hcq_new_users_table_name]]) %>%
    mutate(cohort_name = "hydroxychloriquine new users"), 
  file = here(output_folder, "cohort_count_new.csv")
)
write_csv(
  cohortAttrition(cdm[[hcq_new_users_table_name]]) %>%
    mutate(cohort_name = "hydroxychloriquine new users"), 
  file = here(output_folder, "cohort_attrition_new.csv")
)
