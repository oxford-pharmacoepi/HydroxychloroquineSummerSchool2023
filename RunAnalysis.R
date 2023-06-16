# create logger ----
log_file <- here(output_folder, "log.txt")
if (file.exists(log_file)) {
  unlink(log_file)
}

logger          <- create.logger()
logfile(logger) <- log_file
level(logger)   <- "INFO"
info(logger, "CREATE LOGGER")

# STEP 0 Load study parameters and functions ----
info(logger, "STEP 0: INITIAL SETTINGS")

info(logger, "LOAD STUDY PARAMETERS")
age_groups <- list(c(0,19), c(20,39), c(40,59), c(60,79), c(80,150), c(0,150))
window.before <- c(study.start, covid.start - 1)
window.hcq <- c(covid.start, hcq.end)
window.after <- c(hcq.end + 1, study.end)

# NAMES FOR COHORT TABLES
stem_table               <- tolower(stem_table)
study_table_name         <- paste0(stem_table, "_study")
medications_table_name   <- paste0(stem_table, "_medications")
conditions_table_name    <- paste0(stem_table, "_conditions")
hcq_new_users_table_name <- paste0(stem_table, "_hcq_new")
hcq_users_table_name     <- paste0(stem_table, "_hcq")
mtx_new_users_table_name <- paste0(stem_table, "_mtx_new")
mtx_users_table_name     <- paste0(stem_table, "_mtx")
ip_general_table_name    <- paste0(stem_table, "_ip_general")
ip_covid_table_name      <- paste0(stem_table, "_ip_covid")
ip_ra_table_name         <- paste0(stem_table, "_ip_ra")
ip_malaria_table_name    <- paste0(stem_table, "_ip_malaria")

table_names <- c(study_table_name, medications_table_name, conditions_table_name, 
                 hcq_new_users_table_name, hcq_users_table_name, mtx_new_users_table_name,
                 mtx_users_table_name, ip_general_table_name, ip_covid_table_name,
                 ip_ra_table_name, ip_malaria_table_name)


info(logger, "LOAD STUDY FUNCTIONS")
source(here("functions.R"))

snapshotDb <- dplyr::as_tibble(
  do.call(cbind.data.frame,CDMConnector::snapshot(cdm))
)
write_csv(snapshotDb, here(output_folder, paste0("cdm_snapshot_", cdmName(cdm), ".csv")))

# STEP 1 Instantiate cohorts ----
if (instantiate_cohorts) {
  info(logger, "STEP 1: INSTANTIATE JSON COHORTS")
  source(here("1_InstantiateCohorts", "InstantiateCohorts.R"))
  
} else {
  cdm <- cdmFromCon(
    con = db, 
    cdmSchema = cdm_database_schema,
    writeSchema = results_database_schema,
    cdmName = db_name,
    cohortTables = table_names
  )
}


# STEP 2 Estimate incidence prevalence ----
if (incidence_prevalence) {
  info(logger, "STEP 2: ESTIMATE INCIDENCE PREVALENCE")
  source(here("2_IncidencePrevalence","EstimateIncidencePrevalence.R"))
}

# STEP 3 Characterisation ----
if (characterisation) {
  info(logger, "STEP 3: CHARACTERISATION")
  source(here("3_Characterisation","Characterisation.R"))
}



# info(logger, "ZIPPING RESULTS")
# output_folder <- basename(output_folder)
# zip(
#   zipfile = file.path(paste0(output_folder, "/Results_", db_name, ".zip")),
#   files = list.files(output_folder, full.names = TRUE)
# )