if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}


# create logger ----
log_file <- here(output_folder, "log.txt")
if (file.exists(log_file)) {
  unlink(log_file)
}

logger <- create.logger()
logfile(logger) <- log_file
level(logger)   <- "INFO"
info(logger, "CREATE LOGGER")

# STEP 0 Load study parameters and functions ----
info(logger, "STEP 0 INITIAL SETTINGS")

info(logger, "LOAD STUDY PARAMETERS")
age_groups <- list(c(0,19), c(20,39), c(40,59), c(60,79), c(80,150), c(0,150))
window.before <- c(study.start, covid.start - 1)
window.hcq <- c(covid.start, hcq.end)
window.after <- c(hcq.end + 1, study.end)

# COHORT NAMES 
stem_table           <- tolower(stem_table)
strataTableName      <- paste0(stem_table, "_strata")
hcqTableName         <- paste0(stem_table, "_hcq")
generalDenTableName  <- paste0(stem_table, "_general")
covidDenTableName    <- paste0(stem_table, "_covid")
raDenTableName       <- paste0(stem_table, "_ra")
malariaDenTableName  <- paste0(stem_table, "_malaria")
populationCohortName <- paste0(stem_table, "_population")


info(logger, "LOAD STUDY FUNCTIONS")
source(here("functions.R"))

snapshotDb <- dplyr::as_tibble(
  do.call(cbind.data.frame,CDMConnector::snapshot(cdm))
)
write_csv(snapshotDb, here(output_folder, paste0("cdm_snapshot_", cdmName(cdm), ".csv")))

# STEP 1 Instantiate cohorts ----
if (instantiate_cohorts) {
  info(logger, "INSTANTIATE JSON COHORTS")
  source(here("1_InstantiateCohorts", "InstantiateCohorts.R"))
  
} else {
  cdm <- cdmFromCon(
    con = db, 
    cdmSchema = cdm_database_schema,
    writeSchema = results_database_schema,
    cdmName = db_name,
    cohortTables = c(strataTableName, hcqTableName, generalDenTableName, 
                     covidDenTableName, raDenTableName, malariaDenTableName,
                     populationCohortName)
  )
}


# STEP 2 Estimate incidence prevalence ----
if (incidence_prevalence) {
  info(logger, "ESTIMATE INCIDENCE PREVALENCE")
  source(here("2_IncidencePrevalence","EstimateIncidencePrevalence.R"))
}



# info(logger, "ZIPPING RESULTS")
# output_folder <- basename(output_folder)
# zip(
#   zipfile = file.path(paste0(output_folder, "/Results_", db_name, ".zip")),
#   files = list.files(output_folder, full.names = TRUE)
# )