library(here)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(grid)
library(ggfortify)
library(ggh4x)


resultsFolder <- "Results_PHARMETRICS"

cdc_covid <- tibble(read.csv(here("cdc_covid.csv"))) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y")) %>%
  group_by(start_date, end_date) %>%
  summarise(new_cases = sum(new_cases), .groups = "drop") %>%
  filter(start_date <= as.Date("2022-04-01")) %>%
  mutate(facet = "Weekly COVID-19 cases",
         color_aesthetic = "COVID-19 weekly cases")


# COVID + RA on hydroxychloroquine
inc <- tibble(read.csv(here(resultsFolder, "incidence.csv"))) %>%
  mutate(incidence_start_date = as.Date(incidence_start_date, format = "%Y-%m-%d"),
         incidence_end_date = as.Date(incidence_end_date, format = "%Y-%m-%d")) %>%
  filter(incidence_end_date <= as.Date("2022-04-01")) %>%
  filter(denominator_age_group == "0;150") %>%
  filter(denominator_sex == "Both") %>%
  filter(denominator_strata_cohort_name  %in% c("general", "covid_no_ra", "rheumatoid_arthritis_no_covid")) %>%
  filter(analysis_interval == "months") 

prev <- tibble(read.csv(here(resultsFolder, "prevalence.csv"))) %>%
  mutate(prevalence_start_date = as.Date(prevalence_start_date, format = "%Y-%m-%d"),
         prevalence_end_date = as.Date(prevalence_end_date, format = "%Y-%m-%d")) %>%
  filter(prevalence_end_date <= as.Date("2022-04-01")) %>%
  filter(denominator_age_group == "0;150") %>%
  filter(denominator_sex == "Both") %>%
  filter(denominator_strata_cohort_name  %in% c("general", "covid_no_ra", "rheumatoid_arthritis_no_covid")) %>%
  filter(analysis_interval == "months")

data_ip_cases <- inc %>%
  select(start_date = incidence_start_date, 
         point = incidence_100000_pys, 
         denominator = denominator_strata_cohort_name,
         lower = incidence_100000_pys_95CI_lower,
         upper = incidence_100000_pys_95CI_upper,
         outcome = outcome_cohort_name) %>%
  mutate(facet = "Incidence") %>%
  union_all(prev %>%
              select(start_date = prevalence_start_date, 
                     point = prevalence, 
                     denominator = denominator_strata_cohort_name,
                     outcome = outcome_cohort_name) %>%
              mutate(facet = "Prevalence",
                     lower = NA,
                     upper = NA)) %>%
  # union_all(cdc_covid %>%
  #             select(start_date,
  #                    point = new_cases,
  #                    facet) %>%
  #             mutate(
  #               denominator = "rheumatoid_arthritis_no_covid",
  #               facet = "COVID-19 weekly cases",
  #               lower = NA,
  #               upper = NA,
  #               outcome = "COVID-19 weekly cases")) %>%
  # union_all(cdc_covid %>%
  #             select(start_date,
  #                    point = new_cases,
  #                    facet) %>%
  #             mutate(
  #               facet = "COVID-19 weekly cases",
  #               denominator = "covid_no_ra",
  #               lower = NA,
  #               upper = NA,
  #               outcome = "COVID-19 weekly cases")) %>%
  # union_all(cdc_covid %>%
  #             select(start_date,
  #                    point = new_cases,
  #                    facet) %>%
  #             mutate(
  #               facet = "COVID-19 weekly cases",
  #               denominator = "general",
  #               lower = NA,
  #               upper = NA,
  #               outcome = "COVID-19 weekly cases")) %>%
  mutate(denominator = 
           factor(denominator, 
                  levels = c(
                    "general",
                    "rheumatoid_arthritis_no_covid",
                    "covid_no_ra"),
                  labels = c(
                    "General",
                    "Rheumatoid arthritis",
                    "COVID-19"
                  )),
         facet = 
           factor(
             facet,
             levels = c("Incidence", "Prevalence", "COVID-19 weekly cases"),
             labels = c("Incidence (100,000 pys)", 
                        "Prevalence", 
                        "COVID-19 weekly cases")),
         outcome = case_when(
           outcome == "users_hydroxychloroquine" ~ "Hydroxychloroquine",
           outcome == "users_methotrexate" ~ "Methotrexate",
           outcome == "COVID-19 weekly cases" ~ "COVID-19",
           .default = NA
         ) )

###################

gg <- data_ip_cases %>%
  ggplot(aes(x = start_date, y = point, color = outcome)) +
  geom_point() +
  geom_line() +
  facet_grid2(rows = vars(facet),
              cols = vars(denominator),
              scales = "free_y", 
              independent = "y") +
  scale_color_manual(values = c("#2a9d8f", "#f4a261")) +
  xlab("Date") +
  ylab("") +
  guides(colour = guide_legend(" "))


ggsave(
  paste0("Figure1.png"),
  plot = gg,
  path = here(),
  scale = 1,
  width = 4000,
  height = 1800,
  units = "px",
  dpi = 300
)

###################



prev <- tibble(read.csv(here(resultsFolder, "prevalence.csv"))) %>%
  mutate(prevalence_start_date = as.Date(prevalence_start_date, format = "%Y-%m-%d"),
         prevalence_end_date = as.Date(prevalence_end_date, format = "%Y-%m-%d")) %>%
  filter(prevalence_end_date <= as.Date("2022-04-01")) %>%
  filter(denominator_age_group == "0;150") %>%
  filter(denominator_sex == "Both") %>%
  filter(denominator_strata_cohort_name  %in% c("covid_no_ra", "rheumatoid_arthritis_no_covid")) %>%
  filter(analysis_interval == "weeks") %>%
  filter(outcome_cohort_name == "users_hydroxychloroquine")

prev %>%
  ggplot(aes(x = prevalence_start_date, y = prevalence, color = denominator_strata_cohort_name)) +
  geom_point() +
  geom_line() 


# 
prev <- tibble(read.csv(here(resultsFolder, "prevalence.csv"))) %>%
  mutate(prevalence_start_date = as.Date(prevalence_start_date, format = "%Y-%m-%d"),
         prevalence_end_date = as.Date(prevalence_end_date, format = "%Y-%m-%d")) %>%
  filter(prevalence_end_date <= as.Date("2022-04-01")) %>%
  filter(denominator_age_group == "0;150") %>%
  filter(denominator_sex == "Both") %>%
  filter(analysis_interval == "months") %>%
  filter(denominator_strata_cohort_name != "malaria") %>%
  mutate(denominator_strata_cohort_name = 
           factor(denominator_strata_cohort_name,
                  levels = c("general", "covid", "covid_no_ra", "rheumatoid_arthritis", 
                             "rheumatoid_arthritis_no_covid")
                  )
         )
  
prev %>% 
  ggplot(aes(x = prevalence_start_date, y = prevalence, color = outcome_cohort_name)) +
  geom_point() +
  geom_line() +
  facet_grid(rows = vars(denominator_strata_cohort_name), scales = "free_y")

