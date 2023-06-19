library(shiny)
library(shinydashboard)
library(here)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)
library(scales)
library(tidyr)

resultsFolder <- "Results_PHARMETRICS"

readr::read_csv(
  here::here(resultsFolder, "characteristics_atc_hcq.csv"), 
  col_types = readr::cols(.default = readr::col_character())
) %>%
  dplyr::mutate(generated_by = "DrugUtilisation_0.2.0_summariseCodelistATC") %>%
  readr::write_csv(
    here::here(resultsFolder, "characteristics_atc_hcq.csv")
  )

readr::read_csv(
  here::here(resultsFolder, "characteristics_icd10_hcq.csv"), 
  col_types = readr::cols(.default = readr::col_character())
) %>%
  dplyr::mutate(generated_by = "DrugUtilisation_0.2.0_summariseCodelistICD10") %>%
  readr::write_csv(
    here::here(resultsFolder, "characteristics_icd10_hcq.csv")
  )

source(here("Shiny", "functionsSG.R"))

elements <- readFiles(here::here(resultsFolder))
settings <- attr(elements, "settings")

incidence <- getElementType(elements, "incidence_estimates") %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    denominator_strata_cohort_name = dplyr::if_else(
      is.na(.data$denominator_strata_cohort_name),
      "general",
      .data$denominator_strata_cohort_name
    ),
    n_persons = niceNum(.data$n_persons),
    person_days = niceNum(.data$person_days),
    n_events = niceNum(.data$n_events),
    person_years = niceNum(.data$person_years, significativeDecimals = 2),
    incidence_100000_pys = niceNum(.data$incidence_100000_pys, significativeDecimals = 2),
    incidence_100000_pys_95CI_lower = niceNum(.data$incidence_100000_pys_95CI_lower, significativeDecimals = 2),
    incidence_100000_pys_95CI_upper = niceNum(.data$incidence_100000_pys_95CI_upper, significativeDecimals = 2)
  )

denominatorAttrition <- getElementType(elements, "denominator_attrition") %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    strata_cohort_name = dplyr::if_else(
      is.na(.data$strata_cohort_name),
      "general",
      .data$strata_cohort_name
    ),
    excluded_records = dplyr::if_else(
      is.na(.data$excluded_records), "0", .data$excluded_records
    ),
    excluded_subjects = dplyr::if_else(
      is.na(.data$excluded_subjects), "0", .data$excluded_subjects
    )
  ) %>%
  dplyr::mutate(
    number_records = niceNum(.data$number_records),
    number_subjects = niceNum(.data$number_subjects),
    excluded_records = niceNum(.data$excluded_records),
    excluded_subjects = niceNum(.data$excluded_subjects)
  )

atc <- getElementType(elements, "atc_summary") %>%
  dplyr::bind_rows() %>%
  dplyr::select(-c("group_name", "group_level", "strata_name", "cdm_name", "variable_type", "variable_level", "estimate_type", "generated_by"))
atc <- atc %>%
  filter(variable != "denominator") %>%
  left_join(
    atc %>%
      filter(variable == "denominator") %>%
      mutate(denominator = estimate) %>%
      select(-c("variable", "estimate")),
    by = c("strata_level", "window_name")
  ) %>%
  mutate(
    percentage = as.numeric(estimate)/as.numeric(denominator),
    variable = gsub("ATC 3rd: ", "", variable)
  ) %>%
  mutate(percentage = if_else(percentage>1, 1, percentage)) %>%
  select(-c("estimate", "denominator")) %>%
  rename("ATC 3rd" = "variable")

icd10 <- getElementType(elements, "icd10_summary") %>%
  dplyr::bind_rows() %>%
  dplyr::select(-c("group_name", "group_level", "strata_name", "cdm_name", "variable_type", "variable_level", "estimate_type", "generated_by"))
icd10 <- icd10 %>%
  filter(variable != "denominator") %>%
  left_join(
    icd10 %>%
      filter(variable == "denominator") %>%
      mutate(denominator = estimate) %>%
      select(-c("variable", "estimate")),
    by = c("strata_level", "window_name")
  ) %>%
  mutate(
    percentage = as.numeric(estimate)/as.numeric(denominator)
  ) %>%
  mutate(percentage = if_else(percentage>1, 1, percentage)) %>%
  select(-c("estimate", "denominator")) %>%
  rename("ICD10 Subchapter" = "variable")

indication <- getElementType(elements, "indication") %>%
  bind_rows() %>%
  filter(estimate_type == "%") %>%
  select(-c("group_name", "group_level", "strata_name", "variable_level", "variable_type", "cdm_name", "generated_by", "estimate_type")) %>%
  mutate(variable = gsub("indication_gap_", "", variable), estimate = as.numeric(estimate)/100) %>%
  separate_wider_delim(variable, delim = "_", names = c("gap", "Indication"), too_many = "merge")

incidence_estimates <- getElementType(elements, "incidence_estimates") %>%
  bind_rows() %>%
  mutate(
    denominator_age_group = gsub(";", " to ", denominator_age_group),
    denominator_age_group = if_else(denominator_age_group == "0 to 150", "All ages", denominator_age_group),
    denominator_age_group = if_else(denominator_age_group == "80 to 150", ">=80", denominator_age_group),
    denominator_age_group = factor(denominator_age_group, levels = c("All ages", "0 to 19", "20 to 39", "40 to 59", "60 to 79", ">=80")),
    incidence_100000_pys = as.numeric(incidence_100000_pys), 
    incidence_100000_pys_95CI_lower = as.numeric(incidence_100000_pys_95CI_lower),
    incidence_100000_pys_95CI_upper = as.numeric(incidence_100000_pys_95CI_upper),
    incidence_start_date = as.Date(incidence_start_date),
    incidence_end_date = as.Date(incidence_end_date)
  ) %>%
  select(
    "incidence_start_date", "incidence_end_date", "incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper", 
    "outcome_cohort_name", "denominator_age_group", "denominator_sex", "denominator_strata_cohort_name",
    "n_persons", "n_events"
  )

prevalence_estimates <- getElementType(elements, "prevalence_estimates") %>%
  bind_rows() %>%
  mutate(
    denominator_age_group = gsub(";", " to ", denominator_age_group),
    denominator_age_group = if_else(denominator_age_group == "0 to 150", "All ages", denominator_age_group),
    denominator_age_group = if_else(denominator_age_group == "80 to 150", ">=80", denominator_age_group),
    denominator_age_group = factor(denominator_age_group, levels = c("All ages", "0 to 19", "20 to 39", "40 to 59", "60 to 79", ">=80")),
    prevalence = as.numeric(prevalence), 
    prevalence_95CI_lower  = as.numeric(prevalence_95CI_lower ),
    prevalence_95CI_upper  = as.numeric(prevalence_95CI_upper ),
    prevalence_start_date  = as.Date(prevalence_start_date ),
    prevalence_end_date  = as.Date(prevalence_end_date )
  ) %>%
  select(
    "prevalence_start_date ", "prevalence_end_date ", "incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper", 
    "outcome_cohort_name", "denominator_age_group", "denominator_sex", "denominator_strata_cohort_name",
    "n_persons", "n_events"
  )

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
        menuSubItem("Indication", tabName = "indication"),
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
      ### cohort_count ----
      tabItem(
        tabName = "cohort_counts", 
        h3("Counts for the cohorts instantiated in the study"),
        DTOutput("cohort_count")
      ),
      ### cohort_attrition -----
      tabItem(
        tabName = "cohort_attrition", 
        h3("Attrition for the different cohorts in the study"),
        DTOutput("cohort_attrition")
      ),
      ### denomiator_population ----
      tabItem(
        tabName = "denomiator_population", 
        h3("Attrition for the denominator population"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "denominator_attrition_age_group",
            label = "Age group",
            choices = unique(denominatorAttrition$age_group),
            selected = unique(denominatorAttrition$age_group)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "denominator_attrition_sex",
            label = "Sex",
            choices = unique(denominatorAttrition$sex),
            selected = unique(denominatorAttrition$sex)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "denominator_attrition_days_prior_history",
            label = "Days prior history",
            choices = unique(denominatorAttrition$days_prior_history),
            selected = unique(denominatorAttrition$days_prior_history)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "denominator_attrition_start_date",
            label = "Start date",
            choices = unique(denominatorAttrition$start_date),
            selected = unique(denominatorAttrition$start_date)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "denominator_attrition_end_date",
            label = "End date",
            choices = unique(denominatorAttrition$end_date),
            selected = unique(denominatorAttrition$end_date)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "denominator_attrition_strata_cohort_name",
            label = "Strata",
            choices = unique(denominatorAttrition$strata_cohort_name),
            selected = unique(denominatorAttrition$strata_cohort_name)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = FALSE
          )
        ),
        DTOutput("denominator_attrition")
      ),
      ### incidence_estimates ----
      tabItem(
        tabName = "incidence_estimates", 
        h3("Incidence estimates and plots"),
        hr(),
        h3("Outcome"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_outcome_cohort_name",
            label = "Age group",
            choices = unique(incidence_estimates$outcome_cohort_name),
            selected = unique(incidence_estimates$outcome_cohort_name)[1],
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        ),
        h3("Denominator population"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_denominator_age_group",
            label = "Age group",
            choices = levels(incidence_estimates$denominator_age_group),
            selected = "All ages",
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_denominator_sex",
            label = "Sex",
            choices = unique(incidence_estimates$denominator_sex),
            selected = "Both",
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        ),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_denominator_strata_cohort_name",
            label = "Strata",
            choices = unique(incidence_estimates$denominator_strata_cohort_name),
            selected = "general",
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        ),
        hr(),
        h3("Dates estimates"),
        div(
          style = "display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "incidence_estimates_incidence_start_date",
            label = "Incidence date",
            choices = as.character(unique(incidence_estimates$incidence_start_date)),
            selected = as.character(unique(incidence_estimates$incidence_start_date)),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            ),
            multiple = TRUE
          )
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table of estimates", 
            DTOutput('tbl_incidence_estimates') %>% withSpinner()
          ), 
          tabPanel(
            "Plot of estimates",
            tags$h5("Plotting options"),
            div(
              style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "incidence_x_axis",
                label = "X axis",
                choices = c(
                  "incidence_start_date",
                  "outcome_cohort_name",
                  "denominator_age_group", 
                  "denominator_sex",
                  "denominator_strata_cohort_name"
                ),
                selected = "incidence_start_date",
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"),
                multiple = FALSE
              )
            ),
            div(
              style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "incidence_plot_facet",
                label = "Facet by",
                choices = c(
                  "outcome_cohort_name", 
                  "denominator_age_group",
                  "denominator_sex",
                  "denominator_strata_cohort_name"
                ),
                selected = c("outcome_cohort_name"),
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
              )
            ),
            div(
              style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "incidence_plot_group",
                label = "Colour by",
                choices = c(
                  "outcome_cohort_name", 
                  "denominator_age_group",
                  "denominator_sex",
                  "denominator_strata_cohort_name"
                ),
                selected="outcome_cohort_name",
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = TRUE
              )
            ),
            plotlyOutput('plot_incidence_estimates', height = "800px") %>% withSpinner(), 
          )
        )
      ),
      ### prevalence_estimates ----
      tabItem(
        tabName = "prevalence_estimates", 
        h3("Details of the databases that participated in the study"),
        tags$hr(),
        tags$h5("Population settings"),
        div(
          style="display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_denominator_age_group_selector",
            label = "Age group",
            choices = levels(prevalence_estimates$denominator_age_group),
            selected = "All ages",
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"),
            multiple = TRUE)
        ),
        div(
          style="display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_denominator_sex_selector",
            label = "Sex",
            choices = unique(prevalence_estimates$denominator_sex),
            selected = "Both",
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"),
            multiple = TRUE)
        ),
        div(
          style="display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_denominator_strata_cohort_name",
            label = "Strata cohort",
            choices = unique(prevalence_estimates$denominator_strata_cohort_name),
            selected = "general",
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"),
            multiple = TRUE)
        ),
        tags$hr(),
        tags$h5("Analysis settings"),
        div(
          style="display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(
            inputId = "prevalence_start_date_selector",
            label = "Prevalence start date",
            choices = as.character(unique(prevalence_estimates$prevalence_start_date)),
            selected = as.character(unique(prevalence_estimates$prevalence_start_date)),
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"),
            multiple = TRUE)
        ),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Table of estimates", 
            DTOutput('tbl_prevalence_estimates') %>% withSpinner()
          ), 
          tabPanel(
            "Plot of estimates",
            tags$hr(),
            tags$h5("Plotting options"),
            div(
              style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "prevalence_x_axis",
                label = "X axis",
                choices = c("denominator_age_group", 
                            "denominator_sex",
                            "denominator_days_prior_history",
                            "prevalence_start_date"),
                selected = "prevalence_start_date",
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"),
                multiple = FALSE,)
            ),
            div(
              style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "prevalence_plot_facet",
                label = "Facet by",
                choices = c("denominator_age_group", 
                            "denominator_sex",
                            "denominator_days_prior_history",
                            "prevalence_start_date"),
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"),
                multiple = TRUE,)
            ),
            div(
              style="display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "prevalence_plot_group",
                label = "Colour by",
                choices = c("denominator_age_group", 
                            "denominator_sex",
                            "denominator_days_prior_history",
                            "outcome_cohort_name",
                            "database_name",
                            "prevalence_start_date"),
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"),
                multiple = TRUE,)
            ),
            plotlyOutput('plot_prevalence_estimates', height = "800px") %>% withSpinner() 
          )
        )
      ),
      ### indication ----
      tabItem(
        tabName = "indication",
        h3("Characterisation of the indication"),
        hr(),
        pickerInput(
          inputId = "indication_gap",
          label = "Indication gap",
          choices = c(0, 7, 30),
          selected = 0,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            `selected-text-format` = "count > 3"
          ),
          multiple = FALSE
        ),
        hr(),
        DTOutput("indication")
      ),
      ### table_characteristics ----
      tabItem(
        tabName = "table_characteristics", 
        h3("Baseline characteristics of new users of hydroxychloroquine"),
        DTOutput("table_characteristics")
      ),
      ### atc_characterization ----
      tabItem(
        tabName = "atc_characterization", 
        h3("Explore the ATC characterisation"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Compare windows",
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "atc_strata_level",
                label = "Choose calendar time",
                choices = unique(atc$strata_level),
                selected = unique(atc$strata_level)[1],
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = FALSE
              )
            ),
            DTOutput('atc_window') %>% withSpinner()
          ),
          tabPanel(
            "Compare calendar times",
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "atc_window_name",
                label = "Choose window",
                choices = unique(atc$window_name),
                selected = unique(atc$window_name)[1],
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = FALSE
              )
            ),
            DTOutput('atc_calendar') %>% withSpinner()
          )
        )
      ),
      ### icd10_characterization ----
      tabItem(
        tabName = "icd10_characterization", 
        h3("Explore the ICD10 characterisation"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Compare windows",
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "icd10_strata_level",
                label = "Choose calendar time",
                choices = unique(icd10$strata_level),
                selected = unique(icd10$strata_level)[1],
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = FALSE
              )
            ),
            DTOutput('icd10_window') %>% withSpinner()
          ),
          tabPanel(
            "Compare calendar times",
            div(
              style = "display: inline-block;vertical-align:top; width: 150px;",
              pickerInput(
                inputId = "icd10_window_name",
                label = "Choose window",
                choices = unique(icd10$window_name),
                selected = unique(icd10$window_name)[1],
                options = list(
                  `actions-box` = TRUE,
                  size = 10,
                  `selected-text-format` = "count > 3"
                ),
                multiple = FALSE
              )
            ),
            DTOutput('icd10_calendar') %>% withSpinner()
          )
        )
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
  output$cohort_count <- renderDataTable({
    DT::datatable(
      displayCohortCount(elements), 
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
  output$cohort_attrition <- renderDataTable({
    DT::datatable(
      displayCohortAttrition(elements), 
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
  output$denominator_attrition <- renderDataTable({
    DT::datatable(
      displayDenominatorAttrition(
        denominatorAttrition = denominatorAttrition, 
        ageGroup = input$denominator_attrition_age_group,
        sex = input$denominator_attrition_sex,
        daysPriorHistory = input$denominator_attrition_days_prior_history,
        startDate = input$denominator_attrition_start_date,
        endDate = input$denominator_attrition_end_date,
        strataCohortName = input$denominator_attrition_strata_cohort_name
      ), 
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
  output$table_characteristics <- renderDataTable({
    DT::datatable(
      displayTableOne(elements), 
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
  output$atc_window <- renderDataTable({
    DT::datatable(
      atc %>%
        dplyr::filter(.data$strata_level == input$atc_strata_level) %>%
        dplyr::select(-"strata_level") %>%
        tidyr::pivot_wider(names_from = "window_name", values_from = "percentage")
    ) %>%
      formatPercentage(c("-365 to -1", "-30 to -1", "0 to 0", "1 to 30", "1 to 365"), 0)
  })
  output$atc_calendar <- renderDataTable({
    DT::datatable(
      atc %>%
        dplyr::filter(.data$window_name == input$atc_window_name) %>%
        dplyr::select(-"window_name") %>%
        tidyr::pivot_wider(names_from = "strata_level", values_from = "percentage")
    ) %>%
      formatPercentage(c("Overall", "before", "after", "during"), 0)
  })
  output$icd10_window <- renderDataTable({
    DT::datatable(
      icd10 %>%
        dplyr::filter(.data$strata_level == input$icd10_strata_level) %>%
        dplyr::select(-"strata_level") %>%
        tidyr::pivot_wider(names_from = "window_name", values_from = "percentage")
    ) %>%
      formatPercentage(c("-Inf to -1", "-365 to -1", "-30 to -1", "0 to 0", "1 to 30", "1 to 365", "1 to Inf"), 0)
  })
  output$icd10_calendar <- renderDataTable({
    DT::datatable(
      icd10 %>%
        dplyr::filter(.data$window_name == input$icd10_window_name) %>%
        dplyr::select(-"window_name") %>%
        tidyr::pivot_wider(names_from = "strata_level", values_from = "percentage")
    ) %>%
      formatPercentage(c("Overall", "before", "after", "during"), 0)
  })
  output$tbl_incidence_estimates <- renderDataTable({
    DT::datatable(
      displayIncidence(
        incidence = incidence_estimates, 
        ageGroup = input$incidence_estimates_denominator_age_group,
        sex = input$incidence_estimates_denominator_sex,
        strataCohortName = input$incidence_estimates_denominator_strata_cohort_name,
        incidenceStartDate = input$incidence_estimates_incidence_start_date
      ), 
      options = list(
        lengthChange = FALSE, 
        searching = FALSE, 
        ordering = FALSE, 
        paging = FALSE
      )
    )
  })
  output$plot_incidence_estimates <- renderPlotly({ 
    
    table <- incidence_estimates %>%
      dplyr::filter(.data$denominator_age_group %in% input$incidence_estimates_denominator_age_group) %>%
      dplyr::filter(.data$denominator_sex %in% input$incidence_estimates_denominator_sex) %>%
      dplyr::filter(.data$denominator_strata_cohort_name %in% input$incidence_estimates_denominator_strata_cohort_name) %>%
      dplyr::filter(as.character(.data$incidence_start_date) %in% input$incidence_estimates_incidence_start_date) %>%
      dplyr::mutate(
        incidence_100000_pys = as.numeric(incidence_100000_pys),
        incidence_100000_pys_95CI_lower = as.numeric(incidence_100000_pys_95CI_lower),
        incidence_100000_pys_95CI_upper = as.numeric(incidence_100000_pys_95CI_upper)
      )
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    if(is.null(input$incidence_plot_group)){
      if(!is.null(input$incidence_plot_facet)){
        p<-table %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group) ){ 
      
      if(is.null(input$incidence_plot_facet) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet) ){
        if(!is.null(input$incidence_plot_group) ){ 
          p<-table %>% 
            unite("Group", 
                  c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
            unite("facet_var", 
                  c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
            ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                              ymin = "incidence_100000_pys_95CI_lower",
                              ymax = "incidence_100000_pys_95CI_upper",
                              group="Group",
                              colour="Group")) +
            geom_point(position=position_dodge(width=1))+
            geom_errorbar(width=0, position=position_dodge(width=1)) +
            facet_wrap(vars(facet_var),ncol = 2)+  
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })
  output$indication <- renderDataTable({
    DT::datatable(
      indication %>%
        dplyr::filter(gap == input$indication_gap) %>%
        select(-"gap") %>%
        pivot_wider(names_from = "strata_level", values_from = "estimate") %>%
        select(c("Indication", "Overall", "before", "after", "during"))
    ) %>%
      formatPercentage(c("Overall", "before", "after", "during"), 0)
  })

}

# app ----
shinyApp(ui, server)
