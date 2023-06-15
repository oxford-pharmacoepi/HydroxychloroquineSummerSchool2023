## ED CODE INSTANTIATE + DIAGNOSIS? ----



## temporal cohort instantiation using atlas generated jsons ----

# Instantiate json cohorts ----
# Pakcage: CDMConnector

# Read json files
json_cohort_set <- readCohortSet(
  path = here("1_InstantiateCohorts", "Cohorts")
)

# instantiate cohorts from json
cdm <- generateCohortSet(
  cdm = cdm,
  cohortSet = json_cohort_set,
  name = strataTableName,
  computeAttrition = TRUE,
  overwrite = TRUE
)





# Instantiate hydroxychloroquine ----
# Generate concept list (package: CodelistGenerator)
conceptList_hcq <- getDrugIngredientCodes(cdm, "hydroxychloroquine")

# Instantiate hidroxychloroquine cohort from concept list (package: DrugUtilisation)
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = hcqUsersTableName,
  conceptSetList = conceptList_hcq,
  summariseMode = "AllEras", 
  gapEra = 30
)

# Instantiate hidroxychloroquine cohort from concept list (package: DrugUtilisation)
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = hcqNewUsersTableName,
  conceptSetList = conceptList_hcq,
  summariseMode = "FirstEra",
  daysPriorHistory = 365,
  gapEra = 30,
  priorUseWashout = 365,
  #cohortDateRange = as.Date(c("2019-01-01", "2022-12-31"))
)

# Instantiate population cohorts ----
# Package: IncidencePrevalence

# General denominator 
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = generalDenTableName,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365, 
  temporary = FALSE
)

exportAttrition(cdm[[generalDenTableName]], here(output_folder, "attrition_general_population.csv"))

# Covid denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = covidDenTableName,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = strataTableName,
  strataCohortId = json_covid_id,
  temporary = FALSE
)

exportAttrition(cdm[[covidDenTableName]], here(output_folder, "attrition_covid_population.csv"))

# RA denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = raDenTableName,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = strataTableName,
  strataCohortId = json_ra_id,
  temporary = FALSE
)

exportAttrition(cdm[[raDenTableName]], here(output_folder, "attrition_ra_population.csv"))

# Malaria denominator
cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = malariaDenTableName,
  cohortDateRange = c(study.start, study.end),
  ageGroup = age_groups, 
  sex = c("Both", "Female", "Male"),
  daysPriorHistory = 365,                                           
  strataTable = strataTableName,
  strataCohortId = json_malaria_id,
  temporary = FALSE
)

exportAttrition(cdm[[malariaDenTableName]], here(output_folder, "attrition_malaria_population.csv"))


# Instantiate denominators (general, covid, ra, malaria) without age and sex strata ----
age_group <- "0;150"
sex       <- "Both"

# cohort reference
population_cohort_reference <- cdm[[generalDenTableName]] %>% # general pop
  inner_join(cdm[[generalDenTableName]] %>%
               cohort_set() %>% 
               filter(.data$age_group == .env$age_group & .data$sex == .env$sex) %>%
               select("cohort_definition_id"),
             copy = TRUE,
             by = "cohort_definition_id") %>%
  mutate(cohort_definition_id = 1) %>%
  union_all(cdm[[covidDenTableName]] %>% # covid pop
              inner_join(cdm[[covidDenTableName]] %>%
                           cohort_set() %>% 
                           filter(.data$age_group == .env$age_group & .data$sex == .env$sex) %>%
                           select("cohort_definition_id"),
                         copy = TRUE,
                         by = "cohort_definition_id") %>%
              mutate(cohort_definition_id = 2)) %>%
  union_all(cdm[[raDenTableName]] %>% # ra pop
              inner_join(cdm[[raDenTableName]] %>%
                           cohort_set() %>% 
                           filter(.data$age_group == .env$age_group & .data$sex == .env$sex) %>%
                           select("cohort_definition_id"),
                         copy = TRUE,
                         by = "cohort_definition_id") %>%
              mutate(cohort_definition_id = 3)) %>%
  union_all(cdm[[malariaDenTableName]] %>% # malaria pop
              inner_join(cdm[[malariaDenTableName]] %>%
                           cohort_set() %>% 
                           filter(.data$age_group == .env$age_group & .data$sex == .env$sex) %>%
                           select("cohort_definition_id"),
                         copy = TRUE,
                         by = "cohort_definition_id") %>%
              mutate(cohort_definition_id = 4)) %>%
  compute()
  
# cohort set
population_cohort_set <- tibble(
  cohort_definition_id = 1:4,
  cohort_name = c("general", "covid", "ra", "malaria")
)

# cohort count
population_cohort_count <- population_cohort_reference %>%
  group_by(cohort_definition_id) %>%
  mutate(number_records = n()) %>%
  ungroup() %>%
  select(-cohort_start_date, -cohort_end_date) %>%
  distinct() %>%
  group_by(cohort_definition_id) %>%
  mutate(number_subjects = n()) %>%
  ungroup() %>%
  select(-subject_id) %>%
  distinct()

# cohort attrition
population_cohort_attrition <- cdm[[generalDenTableName]] %>% # general pop
  inner_join(cdm[[generalDenTableName]] %>%
               cohort_set() %>% 
               filter(.data$age_group == .env$age_group & .data$sex == .env$sex) %>%
               select("cohort_definition_id"),
             copy = TRUE,
             by = "cohort_definition_id") %>%
  inner_join(cdm[[generalDenTableName]] %>% 
               cohort_attrition(),
             copy = TRUE,
             by = "cohort_definition_id") %>%
  mutate(cohort_definition_id = 1) %>%
  union_all(cdm[[covidDenTableName]] %>% # covid pop
              inner_join(cdm[[covidDenTableName]] %>%
                           cohort_set() %>% 
                           filter(.data$age_group == .env$age_group & .data$sex == .env$sex) %>%
                           select("cohort_definition_id"),
                         copy = TRUE,
                         by = "cohort_definition_id") %>%
              inner_join(cdm[[generalDenTableName]] %>% 
                           cohort_attrition(),
                         copy = TRUE,
                         by = "cohort_definition_id") %>%
              mutate(cohort_definition_id = 2)) %>%
  union_all(cdm[[raDenTableName]] %>% # ra pop
              inner_join(cdm[[raDenTableName]] %>%
                           cohort_set() %>% 
                           filter(.data$age_group == .env$age_group & .data$sex == .env$sex) %>%
                           select("cohort_definition_id"),
                         copy = TRUE,
                         by = "cohort_definition_id") %>%
              inner_join(cdm[[generalDenTableName]] %>% 
                           cohort_attrition(),
                         copy = TRUE,
                         by = "cohort_definition_id") %>%
              mutate(cohort_definition_id = 3)) %>%
  union_all(cdm[[malariaDenTableName]] %>% # malaria pop
              inner_join(cdm[[malariaDenTableName]] %>%
                           cohort_set() %>% 
                           filter(.data$age_group == .env$age_group & .data$sex == .env$sex) %>%
                           select("cohort_definition_id"),
                         copy = TRUE,
                         by = "cohort_definition_id") %>%
              inner_join(cdm[[generalDenTableName]] %>% 
                           cohort_attrition(),
                         copy = TRUE,
                         by = "cohort_definition_id") %>%
              mutate(cohort_definition_id = 4)) %>%
  select(-subject_id, -cohort_start_date, -cohort_end_date) %>%
  distinct() %>%
  compute()
  
## instantiate ----
# write cohort_set table
DBI::dbWriteTable(db, 
                  DBI::Id(schema = results_database_schema,
                          table = paste0(populationCohortName, "_set")),
                  population_cohort_set,
                  overwrite = TRUE)

# instantiate cohort
cdm[[populationCohortName]] <- newGeneratedCohortSet(
  cohortRef = computeQuery(population_cohort_reference, 
                           populationCohortName, 
                           FALSE, 
                           results_database_schema, 
                           TRUE))

attr(cdm[[populationCohortName]], "cohort_set")       <- tbl(db, 
                                                            sql(paste0("SELECT*FROM ", results_database_schema, ".", populationCohortName, "_set")))
attr(cdm[[populationCohortName]], "cohort_count")     <- population_cohort_count
attr(cdm[[populationCohortName]], "cohort_attrition") <- population_cohort_attrition

