## Instantiate JSON cohorts ----
info(logger, "1.1. Starting to instantiate json")
# Pakcage: CDMConnector

# Read json files
json_cohort_set <- readCohortSet(
  path = here("1_InstantiateCohorts", "Cohorts")
)

# instantiate cohorts from json
cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = json_cohort_set,
  name = study_table_name,
  computeAttrition = TRUE,
  overwrite = TRUE
)
info(logger, "1.1. Finished to instantiate json")

# get cohort definitions id's
json_covid_id <- json_cohort_set %>% 
  filter(cohort_name == "covid") %>%
  pull(cohort_definition_id)

json_ra_id <- json_cohort_set %>% 
  filter(cohort_name == "RA") %>%
  pull(cohort_definition_id)

json_malaria_id <- json_cohort_set %>% 
  filter(cohort_name == "malaria") %>%
  pull(cohort_definition_id)

## Instantiate hydroxychloroquine ----
info(logger, "1.2. starting to instantiate drug cohorts")
# Package: DrugUtilisation
hcq_concept_list <- getDrugIngredientCodes(cdm, "hydroxychloroquine")

# Users
info(logger, "1.2. starting to instantiate drug cohorts - start users HCQ")
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = hcq_users_table_name,
  conceptSetList = hcq_concept_list,
  summariseMode = "AllEras", 
  gapEra = 30
)
info(logger, "1.2. starting to instantiate drug cohorts - finish users HCQ")

# New users
info(logger, "1.2. starting to instantiate drug cohorts - start new users HCQ")
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
exportAttrition(cdm[[hcq_new_users_table_name]], here(output_folder, "attrition_new_users_hcq.csv"))
info(logger, "1.2. starting to instantiate drug cohorts - finish new users HCQ")

## Instantiate methotrexate ----
# Package: DrugUtilisation
mtx_concept_list <- getDrugIngredientCodes(cdm, "methotrexate")

# Users
info(logger, "1.2. starting to instantiate drug cohorts - start users MTX")
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = mtx_users_table_name,
  conceptSetList = mtx_concept_list,
  summariseMode = "AllEras", 
  gapEra = 30
)
info(logger, "1.2. starting to instantiate drug cohorts - finish users MTX")

# New users
info(logger, "1.2. starting to instantiate drug cohorts - start new users MTX")
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = mtx_new_users_table_name,
  conceptSetList = mtx_concept_list,
  summariseMode = "FirstEra",
  daysPriorHistory = 365,
  gapEra = 30,
  priorUseWashout = 365,
  cohortDateRange = as.Date(c(study.start, study.end))
)
info(logger, "1.2. starting to instantiate drug cohorts - finish new usersMTX")

exportAttrition(cdm[[mtx_new_users_table_name]], here(output_folder, "attrition_new_users_mtx.csv"))

# Instantiate denominator cohorts for incidence and prevalence estimation ----
info(logger, "1.3. starting to instantiate IP denominator cohorts")
# Package: IncidencePrevalence
# General denominator 
info(logger, "1.3. starting to instantiate IP denominator cohorts - start general")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_general_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365, 
  temporary = FALSE
)
exportAttrition(cdm[[ip_general_table_name]], here(output_folder, "attrition_ip_general_population.csv"))
info(logger, "1.3. starting to instantiate IP denominator cohorts - finish general")

# Covid denominator
info(logger, "1.3. starting to instantiate IP denominator cohorts - start covid")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_covid_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = json_covid_id,
  temporary = FALSE
)
exportAttrition(cdm[[ip_covid_table_name]], here(output_folder, "attrition_ip_covid_population.csv"))
info(logger, "1.3. starting to instantiate IP denominator cohorts - finish covid")

# RA denominator
info(logger, "1.3. starting to instantiate IP denominator cohorts - start RA")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_ra_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = json_ra_id,
  temporary = FALSE
)
exportAttrition(cdm[[ip_ra_table_name]], here(output_folder, "attrition_ip_ra_population.csv"))
info(logger, "1.3. starting to instantiate IP denominator cohorts - finish RA")

# Malaria denominator
info(logger, "1.3. starting to instantiate IP denominator cohorts - start malaria")
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = ip_malaria_table_name,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = study_table_name,
  strataCohortId = json_malaria_id,
  temporary = FALSE
)
exportAttrition(cdm[[ip_malaria_table_name]], here(output_folder, "attrition_ip_malaria_population.csv"))
info(logger, "1.3. starting to instantiate IP denominator cohorts - finish malaria")

## Instantiate cohorts for table characteristics ----
# Pakcage: DrugUtilisation

info(logger, "1.4. starting to instantiate cohorts for table on")
# Medications ----
medications_concept_list <- readConceptList(
  cdm,
  path = here("1_InstantiateCohorts", "Cohorts", "TableCharacteristics", "MedicationsConceptSet")
)

info(logger, "1.4. starting to instantiate cohorts for table one - start medication")
cdm <- generateConceptCohortSet(cdm,
                                medications_table_name,
                                medications_concept_list)
info(logger, "1.4. starting to instantiate cohorts for table one - finish medication")
# General conditions ----
conditions_concept_list <- readConceptList(
  cdm,
  path = here("1_InstantiateCohorts", "Cohorts", "TableCharacteristics", "GeneralConditionsConceptSet")
)

info(logger, "1.4. starting to instantiate cohorts for table one - start conditions")
cdm <- generateConceptCohortSet(cdm,
                                conditions_table_name,
                                conditions_concept_list)
info(logger, "1.4. starting to instantiate cohorts for table one - finish conditions")
