## Variables ----
# Names for cohort tables
study_table_name         <- paste0(stem_table, "_study")
hcq_new_users_table_name <- paste0(stem_table, "_hcq_new")
hcq_users_table_name     <- paste0(stem_table, "_hcq")
mtx_new_users_table_name <- paste0(stem_table, "_mtx_new")
mtx_users_table_name     <- paste0(stem_table, "_mtx")

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




