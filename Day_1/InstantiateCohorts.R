## Variables ----
# Names for cohort tables
study_table_name         <- paste0(stem_table, "_study")
new_users_table_name <- paste0(stem_table, "_new")
users_table_name     <- paste0(stem_table, "_prevalent")

# Study dates
study.start <- as.Date("2019-01-01")
study.end   <- as.Date("2022-06-01")

if (! hdm_second_run) {

## Instantiate JSON files----
# Package: CDMConnector
json_cohort_set <- readCohortSet(
  path = here("Day_1", "Cohorts")
)

covid_id <- json_cohort_set %>% 
  filter(cohort_name == "covid") %>%
  pull(cohort_definition_id)

ra_id <- json_cohort_set %>% 
  filter(cohort_name == "rheumatoid_arthritis") %>%
  pull(cohort_definition_id)

covid_no_ra_id <- json_cohort_set %>% 
  filter(cohort_name == "covid_no_ra") %>%
  pull(cohort_definition_id)

ra_no_covid_id <- json_cohort_set %>% 
  filter(cohort_name == "rheumatoid_arthritis_no_covid") %>%
  pull(cohort_definition_id)

malaria_id <- json_cohort_set %>% 
  filter(cohort_name == "malaria") %>%
  pull(cohort_definition_id)

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
conceptList$users_hydroxychloroquine <- cl$`Ingredient: hydroxychloroquine (1777087)`
conceptList$users_methotrexate       <- cl$`Ingredient: methotrexate (1305058)`

# prevalent users
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = users_table_name,
  conceptSetList = conceptList,
  summariseMode = "AllEras", 
  gapEra = 30
)

# Export cohort counts
write_csv(
  cohortCount(cdm[[users_table_name]]) %>%
    left_join(cohortSet(cdm[[users_table_name]]) %>%
                select("cohort_definition_id", "cohort_name")), 
  file = here(output_folder, "cohort_count_prevalent.csv")
)
# Export attrition
write_csv(
  cohortAttrition(cdm[[users_table_name]]) %>%
    left_join(cohortSet(cdm[[users_table_name]]) %>%
                select("cohort_definition_id", "cohort_name")), 
  file = here(output_folder, "cohort_attrition_prevalent.csv")
)

conceptList <- list()
conceptList$new_users_hydroxychloroquine <- cl$`Ingredient: hydroxychloroquine (1777087)`
conceptList$new_users_methotrexate       <- cl$`Ingredient: methotrexate (1305058)`

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
    left_join(cohortSet(cdm[[new_users_table_name]]) %>%
                select("cohort_definition_id", "cohort_name")), 
  file = here(output_folder, "cohort_count_new.csv")
)
# Export attrition
write_csv(
  cohortAttrition(cdm[[new_users_table_name]]) %>%
    left_join(cohortSet(cdm[[new_users_table_name]]) %>%
                select("cohort_definition_id", "cohort_name")), 
  file = here(output_folder, "cohort_attrition_new.csv")
)

} else {
  
  medications_table_name    <- paste0(stem_table, "_medications")
  conditions_table_name     <- paste0(stem_table, "_conditions")
  ip_general_table_name     <- paste0(stem_table, "_ip_general")     # Denominator table
  ip_covid_table_name       <- paste0(stem_table, "_ip_covid")       # Denominator table
  ip_covid_no_ra_table_name <- paste0(stem_table, "_ip_covid_no_ra") # Denominator table
  ip_ra_table_name          <- paste0(stem_table, "_ip_ra")          # Denominator table
  ip_ra_no_covid_table_name <- paste0(stem_table, "_ip_ra_no_covid") # Denominator table
  ip_malaria_table_name     <- paste0(stem_table, "_ip_malaria")     # Denominator table
  
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = cdm_database_schema,
    write_schema = results_database_schema,
    cdm_name = db_name,
    cohort_tables = c(study_table_name, new_users_table_name, users_table_name,
                      medications_table_name, conditions_table_name, ip_general_table_name,
                      ip_covid_table_name, ip_covid_no_ra_table_name, ip_ra_table_name,
                      ip_ra_no_covid_table_name, ip_malaria_table_name)
  )
  
  json_cohort_set <- cdm[[study_table_name]] %>%
    cohort_set()
  
  covid_id <- json_cohort_set %>% 
    filter(cohort_name == "covid") %>%
    pull(cohort_definition_id)
  
  ra_id <- json_cohort_set %>% 
    filter(cohort_name == "rheumatoid_arthritis") %>%
    pull(cohort_definition_id)
  
  covid_no_ra_id <- json_cohort_set %>% 
    filter(cohort_name == "covid_no_ra") %>%
    pull(cohort_definition_id)
  
  ra_no_covid_id <- json_cohort_set %>% 
    filter(cohort_name == "rheumatoid_arthritis_no_covid") %>%
    pull(cohort_definition_id)
  
  malaria_id <- json_cohort_set %>% 
    filter(cohort_name == "malaria") %>%
    pull(cohort_definition_id)
  
  load(here("atc.RData"))
  load(here("icd.RData"))
}
