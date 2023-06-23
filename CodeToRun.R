# ADD NECESSARY PACKAGES
library(DBI)
library(CDMConnector)
library(dplyr)
library(dbplyr)
library(tidyr)
library(here)
library(readr)
library(stringr)
library(log4r)
library(remotes)
library(testthat)

# Install packages from GitHub:
# install_github("ohdsi/CirceR")
# install_github("darwin-eu-dev/PatientProfiles")
# install_github("darwin-eu-dev/DrugUtilisation")
# install_github("darwin-eu-dev/CodelistGenerator")
# install_github("darwin-eu-dev/IncidencePrevalence@strata_prior_hist")
# install_github("ohdsi/Capr")
library(CirceR)
library(PatientProfiles)
library(DrugUtilisation)
library(CodelistGenerator)
library(IncidencePrevalence)
library(Capr)

# database metadata and connection details -----
# The name/ acronym for the database
db_name <- "PHARMETRICS"

# Set output folder location -----
# the path to a folder where the results from this analysis will be saved
output_folder <- here(paste0("Results_", db_name))
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Database connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below
# https://darwin-eu.github.io/CDMConnector/articles/DBI_connection_examples.html 
# for more details.
# you may need to install another package for this 
# eg for postgres 

server_dbi <- "cdm_iqvia_pharmetrics_plus_202203"
user       <- Sys.getenv("DB_USER")
password   <- Sys.getenv("DB_PASSWORD")
port       <- Sys.getenv("DB_PORT")
host       <- Sys.getenv("DB_HOST")

db <- dbConnect(
  RPostgres::Postgres(),
  dbname = server_dbi,
  port = port,
  host = host,
  user = user,
  password = password
)

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "public_100k"

# The name of the schema where results tables will be created 
results_database_schema <- "results"

# Name of stem outcome table in the result schema where the outcome cohorts will
# be stored. 
# Notes: 
# - if there is an existing table in your results schema with the same names it
#   will be overwritten
# - more than one cohort will be created
# - name must be lower case
stem_table <- "nmb"

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema,
  cdm_name = db_name
)

# check database connection
# running the next line should give you a count of your person table====++
cdm$person %>% 
  tally()


# Study parameters:
# minimum counts that can be displayed according to data governance
minimum_counts <- 5

# Study dates
study.start <- as.Date("2019-01-01")
covid.start <- as.Date("2020-02-01")
hcq.end     <- as.Date("2020-06-15")
study.end   <- as.Date("2022-06-01")

# Windows 
window.before <- c(study.start, covid.start - 1)
window.hcq <- c(covid.start, hcq.end)
window.after <- c(hcq.end + 1, study.end)

# Age groups
age_groups <- list(c(0,19), c(20,39), c(40,59), c(60,79), c(80,150), c(0, 150))

write_csv(snapshot(cdm), here(output_folder, "cdm_snapshot.csv"))

# is your source code ICD10?
hdm_second_run <- TRUE

# Jobs to Run
source(here("Day_1", "InstantiateCohorts.R"))
source(here("Day_2", "Characterisation.R"))
source(here("Day_3", "EstimateIncidencePrevalence.R"))



