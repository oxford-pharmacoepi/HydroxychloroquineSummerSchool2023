library(here)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(grid)
library(ggfortify)
library(ggh4x)
library(ggpubr)

# GET DATA -----
resultsFolder <- "Results_PHARMETRICS"

covid_us <- tibble(read.csv(here("center_disease_control.csv"))) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y")) %>%
  group_by(start_date, end_date) %>%
  summarise(new_cases = sum(new_cases), .groups = "drop") %>%
  filter(start_date <= as.Date("2022-04-01")) %>%
  mutate(facet = "COVID-19 weekly cases",
         color_aesthetic = "COVID-19 weekly cases")

covid_cat <- tibble(read.csv(here("dades_obertes_catalunya.csv"))) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y")) %>%
  group_by(start_date, end_date) %>%
  summarise(new_cases = sum(new_cases), .groups = "drop") %>%
  filter(start_date <= as.Date("2022-04-01")) %>%
  mutate(facet = "COVID-19 weekly cases",
         color_aesthetic = "COVID-19 weekly cases")


# COVID + RA on hydroxychloroquine
inc <- tibble(read.csv(here(resultsFolder, "incidence.csv"))) %>%
  mutate(
      denominator_strata_cohort_name = if_else(
        is.na(denominator_strata_cohort_name),
        "general",
        denominator_strata_cohort_name
      ),
    incidence_start_date = as.Date(incidence_start_date, format = "%Y-%m-%d"),
    incidence_end_date = as.Date(incidence_end_date, format = "%Y-%m-%d")) %>%
  filter(incidence_end_date <= as.Date("2022-04-01")) %>%
  filter(denominator_age_group == "0;150") %>%
  filter(denominator_sex == "Both") %>%
  filter(denominator_strata_cohort_name  %in% c("general", "covid_no_ra", "rheumatoid_arthritis_no_covid")) %>%
  filter(analysis_interval == "months")  %>%
  filter(outcome_cohort_name  %in% c("users_hydroxychloroquine", "users_methotrexate",           
                                     "covid", "rheumatoid_arthritis")) 

prev <- tibble(read.csv(here(resultsFolder, "prevalence.csv"))) %>%
  mutate(
    denominator_strata_cohort_name = if_else(
      is.na(denominator_strata_cohort_name),
      "general",
      denominator_strata_cohort_name
    ),
    prevalence_start_date = as.Date(prevalence_start_date, format = "%Y-%m-%d"),
    prevalence_end_date = as.Date(prevalence_end_date, format = "%Y-%m-%d")) %>%
  filter(prevalence_end_date <= as.Date("2022-04-01")) %>%
  filter(denominator_age_group == "0;150") %>%
  filter(denominator_sex == "Both") %>%
  filter(denominator_strata_cohort_name  %in% c("general", "covid_no_ra", "rheumatoid_arthritis_no_covid")) %>%
  filter(analysis_interval == "months")  %>%
  filter(outcome_cohort_name  %in% c("users_hydroxychloroquine", "users_methotrexate",           
                                     "covid", "rheumatoid_arthritis"))

data_ip_cases <- inc %>%
  select(start_date = incidence_start_date, 
         point = incidence_100000_pys, 
         denominator = denominator_strata_cohort_name,
         lower = incidence_100000_pys_95CI_lower,
         upper = incidence_100000_pys_95CI_upper,
         outcome = outcome_cohort_name,
         n_events,
         n_persons) %>%
  mutate(facet = "Incidence") %>%
  union_all(prev %>%
              select(start_date = prevalence_start_date, 
                     point = prevalence, 
                     denominator = denominator_strata_cohort_name,
                     outcome = outcome_cohort_name,
                     n_events = n_cases,
                     n_persons = n_population) %>%
              mutate(facet = "Prevalence",
                     lower = NA,
                     upper = NA)) %>%
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
             levels = c("Incidence", "Prevalence"),
             labels = c("Incidence (100,000 pys)", 
                        "Prevalence (%)")),
         outcome = case_when(
           outcome == "users_hydroxychloroquine" ~ "Hydroxychloroquine",
           outcome == "users_methotrexate" ~ "Methotrexate",
           outcome == "covid" ~ "COVID-19",
           outcome == "rheumatoid_arthritis" ~ "Rheumatoid arthritis",
           .default = NA
         ) ) 

# FIGURE 1 ----
gg1 <- data_ip_cases %>%
  filter(outcome %in% c("Hydroxychloroquine", "Methotrexate")) %>%
  ggplot(aes(x = start_date, y = point, color = outcome)) +
  geom_point() +
  geom_line() +
  facet_nested(rows = vars(facet),
              cols = vars(denominator),
              scales = "free_y", 
              independent = "y") +
  scale_color_manual(values = c("#2a9d8f", "#f4a261")) +
  xlab("Date") +
  ylab("") +
  guides(colour = guide_legend(" "))

gg2 <- covid_us %>%
  ggplot(aes(x = start_date, y = new_cases, fill = facet, color = facet)) +
  geom_col() +
  facet_grid(vars(facet)) +
  scale_fill_manual(values = c("#ced4da")) +
  scale_color_manual(values = c("#adb5bd")) + 
  xlab("Date") +
  ylab("") +
  theme(legend.title = element_blank())

gg <- ggarrange(gg1, gg2,
          labels = c("A", "B"),
          ncol = 1, nrow = 2,
          common.legend = TRUE,
          heights = c(2,1))

ggsave(
  paste0("Figure1.png"),
  plot = gg,
  path = here(),
  scale = 1,
  width = 4000,
  height = 2000,
  units = "px",
  dpi = 300
)

# FIGURE 2 -----
fig2 <- data_ip_cases %>%
  filter(outcome %in% c("Hydroxychloroquine", "Methotrexate")) %>%
  ggplot(aes(x = start_date, y = point, color = outcome, fill = outcome)) +
  geom_point() +
  geom_line() +
  geom_col(data = data_ip_cases %>%
             mutate(facet = "Absolute number") %>%
             select(start_date, n_persons, denominator, facet) %>%
             mutate(outcome = "Denominator") %>%
             distinct(),
           aes(x = start_date, y = n_persons, fill = outcome)) +
  facet_nested(rows = vars(facet),
               cols = vars(denominator),
               scales = "free_y", 
               independent = "y") +
  scale_color_manual(values = c("#6b705c", "#2a9d8f", "#f4a261")) +
  scale_fill_manual(values = c("#b7b7a4", "#2a9d8f", "#f4a261")) +
  xlab("Date") +
  ylab("") +
  guides(colour = element_blank())

ggsave(
  paste0("Figure2.png"),
  plot = fig2,
  path = here(),
  scale = 1,
  width = 4000,
  height = 2000,
  units = "px",
  dpi = 300
)
