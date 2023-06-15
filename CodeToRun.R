# ADD NECESSARY PACKAGES
library(DBI)
library(CDMConnector)
library(dplyr)
library(dbplyr)
library(here)
library(readr)
library(stringr)
library(log4r)
library(remotes)
library(testthat)

# Install PaitentProfiles and DrugUtilisation packages from GutHub (order matters):
# install_github("ohdsi/CirceR")
# install_github("darwin-eu-dev/PatientProfiles")
# install_github("darwin-eu-dev/DrugUtilisation")
# install_github("darwin-eu-dev/CodelistGenerator")
# install_github("darwin-eu-dev/IncidencePrevalence@strata_prior_hist")
library(CirceR)
library(PatientProfiles)
library(DrugUtilisation)
library(CodelistGenerator)
library(IncidencePrevalence)

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

server_dbi <- Sys.getenv("DB_SERVER_DBI_ph")
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

# minimum counts that can be displayed according to data governance
minimum_counts <- 5

# Study dates
study.start <- as.Date("2019-01-01")
covid.start <- as.Date("2020-02-01")
hcq.end     <- as.Date("2020-06-15")
study.end   <- as.Date("2022-06-01")

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(
  con = db,
  cdm_schema = cdm_database_schema,
  write_schema = results_database_schema,
  cdm_name = db_name
)

# check database connection
# running the next line should give you a count of your person table
cdm$person %>% 
  tally()

# jobs to run
instantiate_cohorts  <- TRUE
incidence_prevalence <- TRUE
characterisation     <- FALSE

# Run the study ------
source(here("RunAnalysis.R"))
# after the study is run you should have a zip folder in your output folder to share

dbDisconnect(db)

print("Done!")
print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
print("-- Thank you for running the study!")