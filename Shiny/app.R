library(shiny)
library(shinydashboard)
library(here)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)

readr::read_csv(
  here::here("Results_PHARMETRICS", "characteristics_atc_hcq.csv"), 
  col_types = readr::cols(.default = readr::col_character())
) %>%
  dplyr::mutate(generated_by = "DrugUtilisation_0.2.0_summariseCodelistATC") %>%
  readr::write_csv(
    here::here("Results_PHARMETRICS", "characteristics_atc_hcq.csv")
  )

readr::read_csv(
  here::here("Results_PHARMETRICS", "characteristics_icd10_hcq.csv"), 
  col_types = readr::cols(.default = readr::col_character())
) %>%
  dplyr::mutate(generated_by = "DrugUtilisation_0.2.0_summariseCodelistICD10") %>%
  readr::write_csv(
    here::here("Results_PHARMETRICS", "characteristics_icd10_hcq.csv")
  )

source(here("Shiny", "functionsSG.R"))

elements <- readFiles(here::here("Results_PHARMETRICS"))
settings <- attr(elements, "settings")

# ui ----
ui <- dashboardPage(
  dashboardHeader(title = "Hydroxicloroquine"),
  ## menu ----
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Databases details", tabName = "database_details",
        menuSubItem("CDM snapshot", tabName = "cdm_snapshot")
      ),
      menuItem(
        "Cohort details", tabName = "cohort_details",
        menuSubItem("Cohort counts", tabName = "cohort_counts"),
        menuSubItem("Cohort attrition", tabName = "cohort_attrition")
      ),
      menuItem(
        "Incidence Prevalence", tabName = "incidence_prevalence",
        menuSubItem("Denominator population", tabName = "denomiator_population"),
        menuSubItem("Incidence estimates", tabName = "incidence_estimates"),
        menuSubItem("Prevalence estimates", tabName = "prevalence_estimates")
      ),
      menuItem(
        "Characterization", tabName = "characterization",
        menuSubItem("Table characteristics", tabName = "table_characteristics"),
        menuSubItem("ATC characterization", tabName = "atc_characterization"),
        menuSubItem("ICD10 characterization", tabName = "icd10_characterization")
      )
    )
  ),
  ## body ----
  dashboardBody(
    tabItems(
      ### cdm_snapshot ----
      tabItem(
        tabName = "cdm_snapshot", 
        h3("Details of the databases that participated in the study"),
        p("Identifier 'cdm_name' is the one used in the multiple selection panels of the shiny"),
        DTOutput("cdm_snapshot")
      ),
      ### cohort_counts ----
      tabItem(
        tabName = "cohort_counts", 
        h3("Details of the databases that participated in the study")
      ),
      ### cohort_attrition -----
      tabItem(
        tabName = "cohort_attrition", 
        h3("Details of the databases that participated in the study")
      ),
      ### denomiator_population ----
      tabItem(
        tabName = "denomiator_population", 
        h3("Details of the databases that participated in the study")
      ),
      ### incidence_estimates ----
      tabItem(
        tabName = "incidence_estimates", 
        h3("Details of the databases that participated in the study")
      ),
      ### prevalence_estimates ----
      tabItem(
        tabName = "prevalence_estimates", 
        h3("Details of the databases that participated in the study")
      ),
      ### table_characteristics ----
      tabItem(
        tabName = "table_characteristics", 
        h3("Details of the databases that participated in the study")
      ),
      ### atc_characterization ----
      tabItem(
        tabName = "atc_characterization", 
        h3("Details of the databases that participated in the study")
      ),
      ### icd10_characterization ----
      tabItem(
        tabName = "icd10_characterization", 
        h3("Details of the databases that participated in the study")
      )
    )
  ),
  ## parameters ----
  "Hydroxychloroquine study"
)

# server ----
server <- function(input, output, session) {
  output$cdm_snapshot <- renderDataTable({
    DT::datatable(
      displayCdmSnapshot(elements), 
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
}

# app ----
shinyApp(ui, server)
