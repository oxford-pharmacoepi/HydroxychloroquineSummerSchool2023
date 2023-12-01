library(here)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyr)
library(grid)
library(ggfortify)
library(ggh4x)
library(ggpubr)
library(lubridate)

# GET DATA -----

# External data 
covid_us <- tibble(read.csv(here("center_disease_control.csv"))) %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y")) %>%
  group_by(start_date, end_date) %>%
  summarise(new_cases = sum(new_cases), .groups = "drop") %>%
  filter(start_date <= as.Date("2022-04-01")) %>%
  mutate(cdm_name = "PHARMETRICS",
         denominator = "Region/Country population") %>%
  select(start_date, new_cases, cdm_name, denominator)

covid_cat <- tibble(read.csv(here("dades_obertes_catalunya.csv"))) %>%
  mutate(data = as.Date(data, format = "%d/%m/%Y")) %>%
  filter(data >= as.Date("2019-01-01")) %>%
  filter(data <= as.Date("2022-04-01")) %>%
  mutate(start_date = floor_date(x = .data$data, unit = "week")) %>%
  group_by(start_date) %>%
  summarise(new_cases = sum(casos, na.rm = TRUE), .groups = "drop") %>%
  mutate(cdm_name = "IMASIS",
         denominator = "Region/Country population") %>%
  select(start_date, new_cases, cdm_name, denominator)


# Study data
resultsFolder <- "data"
source(here("functionsSGByDB.R"))
addCdmName(here(resultsFolder))
elements <- readFiles(here::here(resultsFolder))
settings <- attr(elements, "settings")

incidence_estimates <- getElementType(elements, "incidence_estimates") %>%
  bind_rows() %>% 
  mutate(
    denominator_strata_cohort_name = if_else(
      is.na(denominator_strata_cohort_name),
      "general",
      denominator_strata_cohort_name
    ),
    denominator_age_group = gsub(";", " to ", denominator_age_group),
    denominator_age_group = if_else(denominator_age_group == "0 to 150", "All ages", denominator_age_group),
    denominator_age_group = if_else(denominator_age_group == "80 to 150", ">=80", denominator_age_group),
    denominator_age_group = factor(denominator_age_group, levels = c("All ages", "0 to 19", "20 to 39", "40 to 59", "60 to 79", ">=80")),
    incidence_100000_pys = as.numeric(incidence_100000_pys), 
    incidence_100000_pys_95CI_lower = as.numeric(incidence_100000_pys_95CI_lower),
    incidence_100000_pys_95CI_upper = as.numeric(incidence_100000_pys_95CI_upper),
    incidence_start_date = as.Date(incidence_start_date, format = "%Y-%m-%d"),
    incidence_end_date = as.Date(incidence_end_date, format = "%Y-%m-%d")
  ) %>%
  filter(analysis_interval == "months" &
           denominator_age_group  == "All ages" &
           denominator_sex  == "Both" &
           ! denominator_strata_cohort_name %in% c("covid", "rheumatoid_arthritis",
                                                 "malaria") &
           outcome_cohort_name %in% c("users_hydroxychloroquine", "users_methotrexate") &
         incidence_start_date <= as.Date("2022-03-01")
         ) %>%
  select(
    "start_date" = "incidence_start_date", 
    "incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper", 
    "outcome" = "outcome_cohort_name",  
    "denominator" = "denominator_strata_cohort_name", "cdm_name"
  ) %>%
  mutate(denominator = 
           factor(denominator, 
                  levels = c(
                    "general",
                    "covid_no_ra",
                    "rheumatoid_arthritis_no_covid",
                    "Region/Country population"),
                  labels = c(
                    "General database population",
                    "COVID-19",
                    "Rheumatoid arthritis",
                    "Region/Country population"
                  )),
         outcome = case_when(
           outcome == "users_hydroxychloroquine" ~ "Hydroxychloroquine",
           outcome == "users_methotrexate" ~ "Methotrexate",
           .default = NA
         ))



# FIGURE ----
gg <- incidence_estimates %>%
  filter(outcome %in% c("Hydroxychloroquine", "Methotrexate")) %>%
  ggplot(aes(x = start_date, y = incidence_100000_pys, color = outcome, fill = outcome)) +
  geom_point() +
  geom_line() +
  geom_col(data = covid_us %>%
             union_all(covid_cat) %>%
             mutate(outcome = "Weekly COVID-19 cases",
                    denominator = 
                      factor(denominator, 
                             levels = c(
                               "general",
                               "covid_no_ra",
                               "rheumatoid_arthritis_no_covid",
                               "Region/Country population"),
                             labels = c(
                               "General DB population",
                               "COVID-19",
                               "Rheumatoid arthritis",
                               "Region/Country population"
                             ))),
           aes(x = start_date, y = new_cases, fill = outcome, color = outcome)) +
  facet_grid2(rows = vars(denominator),
               cols = vars(cdm_name),
               scales = "free_y", 
               independent = "y") +
  scale_color_manual(values = c("#2a9d8f", "#f4a261", "#adb5bd")) +
  scale_fill_manual(values = c("#78c5b7", "#f8c6a1", "#ced4da")) +
  scale_y_continuous(labels = label_comma(), limits = c(0, NA)) + 
  xlab("Date") +
  ylab("") +
  guides(colour = element_blank()) +
  theme_update() +
  ylab("Incidence (100,000 pys)") +
  theme(legend.title = element_blank())


# ggsave(
#   paste0("Figure.png"),
#   plot = gg,
#   path = here(),
#   scale = 1,
#   width = 3000,
#   height = 3000,
#   units = "px",
#   dpi = 300
# )



# FIGURE DANI ----
## scales 
y_scales <- list(
  cdm_name == "IMASIS" & denominator == "General database population" ~
    scale_y_continuous(labels = label_comma(), 
                       limits = c(0, NA), 
                       breaks = c(0, 1000, 2000, 3000, 4000, 5000)),
  
  cdm_name == "PHARMETRICS" & denominator == "General database population" ~
    scale_y_continuous(labels = label_comma(), 
                       limits = c(0, NA), 
                       breaks = c(0, 250, 500, 740, 1000, 1250)),
  
  cdm_name == "PHARMETRICS" & denominator == "Rheumatoid arthritis" ~
    scale_y_continuous(labels = label_comma(), 
                       limits = c(0, NA), 
                       breaks = c(0, 2500, 5000, 7500))
)


gg_general <- incidence_estimates %>%
  filter(outcome %in% c("Hydroxychloroquine", "Methotrexate")) %>%
  filter(denominator == "General database population") %>%
  ggplot(aes(x = start_date, 
             y = incidence_100000_pys,
             color = outcome, 
             fill = outcome)) +
  geom_col(data = covid_us %>%
             mutate(outcome = "Weekly COVID-19 cases",
                    denominator = "general",
                    denominator = 
                      factor(denominator, 
                             levels = c(
                               "general",
                               "covid_no_ra",
                               "rheumatoid_arthritis",
                               "Region/Country population"),
                             labels = c(
                               "General database population",
                               "COVID-19",
                               "Rheumatoid arthritis",
                               "Region/Country population"
                             ))),
           aes(x = start_date, y = new_cases/4000, fill = outcome, color = outcome)) +
  geom_col(data = covid_cat %>%
             mutate(outcome = "Weekly COVID-19 cases",
                    denominator = "general",
                    denominator = 
                      factor(denominator, 
                             levels = c(
                               "general",
                               "covid_no_ra",
                               "rheumatoid_arthritis",
                               "Region/Country population"),
                             labels = c(
                               "General database population",
                               "COVID-19",
                               "Rheumatoid arthritis",
                               "Region/Country population"
                             ))),
           aes(x = start_date, y = new_cases/40, fill = outcome, color = outcome)) +
  geom_line(linewidth = 0.55) +
  geom_ribbon(data = incidence_estimates %>%
                filter(outcome %in% c("Hydroxychloroquine", "Methotrexate")) %>%
                filter(denominator == "General database population"),
                aes(ymin = incidence_100000_pys_95CI_lower,
                    ymax = incidence_100000_pys_95CI_upper,
                    x = start_date, 
                    y = incidence_100000_pys,
                    color = outcome, 
                    fill = outcome),
                alpha = 0.4, colour = NA) +
  geom_point(aes(shape = outcome), size = 2) +
  facet_grid2(rows = vars(denominator),
              cols = vars(cdm_name),
              scales = "free_y", 
              independent = "y") +
  scale_color_manual(values = c("#2a9d8f", "#f4a261", "#adb5bd")) +
  scale_fill_manual(values = c("#78c5b7", "#f8c6a1", "#ced4da")) +
  scale_y_continuous(labels = label_comma()) + 
  xlab("Date") +
  ylab("") +
  guides(colour = element_blank()) +
  theme_update() +
  ylab("Incidence (100,000 pys)") +
  theme(legend.title = element_blank()) +
  facetted_pos_scales(y = y_scales)

gg_ra <- incidence_estimates %>%
  filter(outcome %in% c("Hydroxychloroquine", "Methotrexate")) %>%
  filter(denominator == "Rheumatoid arthritis",
         cdm_name == "PHARMETRICS") %>%
  ggplot(aes(x = start_date, 
             y = incidence_100000_pys, 
             ymin = incidence_100000_pys_95CI_lower,
             ymax = incidence_100000_pys_95CI_upper,
             color = outcome, 
             fill = outcome)) +
  # geom_errorbar(width = 15, linewidth = 0.45) +
  geom_ribbon(alpha = 0.4, colour = NA) +
  geom_line(linewidth = 0.55) +
  geom_point(size = 3, aes(shape = outcome)) +
  facet_grid2(rows = vars(denominator),
              cols = vars(cdm_name),
              scales = "free_y", 
              independent = "y") +
  scale_color_manual(values = c("#2a9d8f", "#f4a261", "#adb5bd")) +
  scale_fill_manual(values = c("#78c5b7", "#f8c6a1", "#ced4da")) +
  scale_y_continuous(labels = label_comma()) + 
  xlab("Date") +
  ylab("") +
  guides(colour = element_blank()) +
  theme_update() +
  ylab("Incidence (100,000 pys)") +
  theme(legend.title = element_blank()) 
  # facetted_pos_scales(y = y_scales)

gg_dani <- ggarrange(plotlist = list(gg_general, gg_ra), 
                     nrow = 2, 
                     common.legend = TRUE,
                     widths = c(1.5,1),
                     heights = c(1.25,1),
                     legend = "right",
                     legend.grob = get_legend(gg_general)) +
  bgcolor("white")

ggsave(
  paste0("Figure.tiff"),
  plot = gg_dani,
  path = here(),
  scale = 1,
  width = 3000,
  height = 2000,
  units = "px",
  dpi = 300,
  device='tiff'
)


## SUPPLEMENTARY ----
incidence_estimates <- getElementType(elements, "incidence_estimates") %>%
  bind_rows() %>% 
  mutate(
    denominator_strata_cohort_name = if_else(
      is.na(denominator_strata_cohort_name),
      "general",
      denominator_strata_cohort_name
    ),
    denominator_age_group = gsub(";", " to ", denominator_age_group),
    denominator_age_group = if_else(denominator_age_group == "0 to 150", "All ages", denominator_age_group),
    denominator_age_group = if_else(denominator_age_group == "80 to 150", ">=80", denominator_age_group),
    denominator_age_group = factor(denominator_age_group, levels = c("All ages", "0 to 19", "20 to 39", "40 to 59", "60 to 79", ">=80")),
    incidence_100000_pys = as.numeric(incidence_100000_pys), 
    incidence_100000_pys_95CI_lower = as.numeric(incidence_100000_pys_95CI_lower),
    incidence_100000_pys_95CI_upper = as.numeric(incidence_100000_pys_95CI_upper),
    incidence_start_date = as.Date(incidence_start_date, format = "%Y-%m-%d"),
    incidence_end_date = as.Date(incidence_end_date, format = "%Y-%m-%d")
  ) %>%
  filter(analysis_interval == "months" &
           denominator_age_group  == "All ages" &
           denominator_sex  == "Both" &
           ! denominator_strata_cohort_name %in% c("covid_no_ra", "rheumatoid_arthritis_no_covid",
                                                   "malaria") &
           outcome_cohort_name %in% c("users_hydroxychloroquine", "users_methotrexate") &
           incidence_start_date <= as.Date("2022-03-01")
  ) %>%
  select(
    "start_date" = "incidence_start_date", 
    "incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper", 
    "outcome" = "outcome_cohort_name",  
    "denominator" = "denominator_strata_cohort_name", "cdm_name"
  ) %>%
  mutate(denominator = 
           factor(denominator, 
                  levels = c(
                    "general",
                    "covid",
                    "rheumatoid_arthritis",
                    "Region/Country population"),
                  labels = c(
                    "General database population",
                    "COVID-19",
                    "Rheumatoid arthritis",
                    "Region/Country population"
                  )),
         outcome = case_when(
           outcome == "users_hydroxychloroquine" ~ "Hydroxychloroquine",
           outcome == "users_methotrexate" ~ "Methotrexate",
           .default = NA
         ))

gg_sup <- incidence_estimates %>%
  filter(outcome %in% c("Hydroxychloroquine", "Methotrexate")) %>%
  filter(denominator != "COVID-19") %>%
  ggplot(aes(x = start_date, y = incidence_100000_pys, color = outcome, fill = outcome)) +
  geom_col(data = covid_us %>%
             mutate(outcome = "Weekly COVID-19 cases",
                    denominator = "general",
                    denominator = 
                      factor(denominator, 
                             levels = c(
                               "general",
                               "covid_no_ra",
                               "rheumatoid_arthritis_no_covid",
                               "Region/Country population"),
                             labels = c(
                               "General database population",
                               "COVID-19",
                               "Rheumatoid arthritis",
                               "Region/Country population"
                             ))),
           aes(x = start_date, y = new_cases/4000, fill = outcome, color = outcome)) +
  geom_col(data = covid_cat %>%
             mutate(outcome = "Weekly COVID-19 cases",
                    denominator = "general",
                    denominator = 
                      factor(denominator, 
                             levels = c(
                               "general",
                               "covid_no_ra",
                               "rheumatoid_arthritis_no_covid",
                               "Region/Country population"),
                             labels = c(
                               "General database population",
                               "COVID-19",
                               "Rheumatoid arthritis",
                               "Region/Country population"
                             ))),
           aes(x = start_date, y = new_cases/40, fill = outcome, color = outcome)) +
  geom_point() +
  geom_line() +
  facet_grid2(rows = vars(denominator),
              cols = vars(cdm_name),
              scales = "free_y", 
              independent = "y") +
  scale_color_manual(values = c("#2a9d8f", "#f4a261", "#adb5bd")) +
  scale_fill_manual(values = c("#78c5b7", "#f8c6a1", "#ced4da")) +
  scale_y_continuous(labels = label_comma()) + 
  xlab("Date") +
  ylab("") +
  guides(colour = element_blank()) +
  theme_update() +
  ylab("Incidence (100,000 pys)") +
  theme(legend.title = element_blank()) +
  facetted_pos_scales(y = y_scales)

ggsave(
  paste0("Figure_sup.png"),
  plot = gg_sup,
  path = here(),
  scale = 1,
  width = 3000,
  height = 2000,
  units = "px",
  dpi = 300
)
