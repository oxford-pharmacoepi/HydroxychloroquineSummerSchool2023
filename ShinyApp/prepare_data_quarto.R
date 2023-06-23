
source(here("functions_quarto.R"))

elements <- readFiles(here::here("Results_PHARMETRICS"))

# TABLE ONE ----
## data frame ----
table_one <- getElementType(elements, "table_characteristics") %>%
  dplyr::bind_rows() %>%
  dplyr::filter(strata_name%in% c("Indication", "Overall")) %>%
  dplyr::filter(strata_level != "Malaria") %>%
  dplyr::mutate(strata_level = case_when(
    strata_level == "covid" ~ "COVID-19",
    strata_level == "rheumatoid_arthritis" ~ "RA",
    .default = "Overall"
  )) %>%
  dplyr::select(-c("group_name", "strata_name")) %>%
  dplyr::mutate(estimate = niceNum(.data$estimate, significativeDecimals = 0)) %>%
  dplyr::mutate(estimate = gsub(" ", "", .data$estimate)) %>%
  tidyr::pivot_wider(names_from = "estimate_type", values_from = "estimate") %>%
  dplyr::mutate(
    "Count (%)" = dplyr::if_else(!is.na(.data[["%"]]), paste0(count, " (", `%`, "%)"), as.character(NA)),
    "Median [Q25 - Q75]" = dplyr::if_else(!is.na(.data$median), paste0(median, " [", q25, " - ", q75, "]"), as.character(NA)),
    count = dplyr::if_else(!is.na(.data[["%"]]), as.character(NA), count)
  ) %>%
  dplyr::rename("Count" = "count") %>%
  dplyr::select(-c("median", "q25", "q75", "%")) %>%
  tidyr::pivot_longer(c("Count", "Count (%)", "Median [Q25 - Q75]"), names_to = "estimate_type", values_to = "estimate") %>%
  dplyr::filter(!is.na(.data$estimate)) %>%
  tidyr::pivot_wider(names_from = "strata_level", values_from = "estimate") 
table_one_1 <- table_one %>%
  dplyr::filter(!grepl("_m365_to_0|_minf_to_0", .data$variable)) %>%
  dplyr::filter(variable != "indication") %>%
  dplyr::filter(variable != "window") 
table_one_2 <- table_one %>%
  dplyr::filter(grepl("_minf_to_0", .data$variable))
table_one_3 <- table_one %>%
  dplyr::filter(grepl("_m365_to_0", .data$variable))
table_one <- table_one_1 %>%
  dplyr::bind_rows(dplyr::tibble(variable = "Comorbidities any time prior")) %>%
  dplyr::union_all(
    table_one_2 %>%
      dplyr::mutate(variable = gsub("_minf_to_0", "", .data$variable))
  ) %>%
  dplyr::bind_rows(dplyr::tibble(variable = "Medications prior year")) %>%
  dplyr::union_all(
    table_one_3 %>%
      dplyr::mutate(variable = gsub("_m365_to_0", "", .data$variable))
  ) %>%
  dplyr::filter(!is.na(group_level)) %>%
  select(variable, variable_level, estimate_type, Overall, `COVID-19`, RA, group_level, cdm_name)

table_one <- tibble(
  variable = c("number subjects", "age", "Age group", "age_group", "sex", "prior_history", 
               "Comorbidities**",
               "anxiety", "asthma", "chronic_kidney_disease", "chronic_liver_disease",                    
               "copd", "covid", "dementia", "depressive_disorder", "diabetes", "heart_failure", "hiv",                                   
               "hypertension", "hypothyroidism", "infertility", "inflammatory_bowel_disease",               
               "malaria", "malignant_neoplastic_disease", "myocardial_infarction", "osteoporosis",                             
               "pneumonia", "rheumatoid_arthritis", "stroke", "venous_thromboembolism",
               "Medications***",
               "agents_acting_on_renin_angiotensin_system", "antibacterials_systemic",                  
               "antidepressants", "antiepileptics", "antiinflammatory_antirheumatic_agents", 
               "antineoplastic_agents", "antithrombotics", "beta_blocking_agents",                     
               "calicum_channel_blockers","diuretics", "drugs_acid_related_disorder",
               "drugs_for_obstructive_airway_diseases", "drugs_used_in_diabetes",
               "hormonal_contraceptices_systemic", "immunosupressants","lipid_modifying_agents",                   
               "opioids", "psycholeptics", "psychostimulants")
) %>% left_join(table_one,
                by = "variable") %>%
  filter(variable_level != "Male" | is.na(variable_level)) %>%
  mutate(variable = case_when(
    variable == "age_group" ~ variable_level,
    variable == "sex" ~ "Sex: Female",
    .default = variable
  )) %>%
  mutate(variable = stringr::str_to_sentence(gsub("_", " ", variable))) %>%
  mutate(variable = case_when(
    variable == "Copd" ~ "COPD",
    variable == "Covid" ~ "COVID-19",
    variable == "gerd" ~ "GERD",
    variable == "Hiv" ~ "HIV",
    variable == "Prior history" ~ "Prior history*",
    .default = variable
  )) %>%
  rename("Variable" = "variable",
         "Estimate" = "estimate_type")
  
## captions ----  
captions <- expand_grid(
  cdm_name = unique(table_one$cdm_name),
  group_level = c("new_users_hydroxychloroquine", "new_users_methotrexate")
) %>%
  mutate(drug_name = if_else(
    group_level == "new_users_methotrexate",
    "Methotrexate",
    "Hydroxychloroquine"
  )) %>%
  mutate(caption_to = paste0("Baseline characteristics** of new users of ", drug_name, ", in the database ", cdm_name, ".")) 

# DRUG USE ----
## data frame ----
drug_use <- getElementType(elements, "drug_use") %>%
  bind_rows() %>%
  dplyr::filter(strata_level %in% c("covid", "rheumatoid_arthritis", "Overall")) %>%
  dplyr::mutate(strata_level = case_when(
    strata_level == "covid" ~ "COVID-19",
    strata_level == "rheumatoid_arthritis" ~ "RA",
    .default = "Overall"
  )) %>%
  select(-c("group_name", "strata_name", "variable_level", "variable_type", "generated_by")) %>%
  mutate(estimate = round(as.numeric(estimate))) %>%
  tidyr::pivot_wider(names_from = "estimate_type", values_from = "estimate") %>%
  dplyr::mutate(
    "Median [Q25 - Q75]" = dplyr::if_else(!is.na(.data$median), paste0(median, " [", q25, " - ", q75, "]"), as.character(NA)),
    "Mean (SD)" = dplyr::if_else(!is.na(.data$mean), paste0(mean, " (", sd, ")"), as.character(NA)),
    "Count" = niceNum(count)
  ) %>%
  dplyr::select(-c("median", "q25", "q75", "mean", "sd", "count")) %>%
  tidyr::pivot_longer(c("Count", "Mean (SD)", "Median [Q25 - Q75]"), names_to = "estimate_type", values_to = "estimate") %>%
  dplyr::filter(!is.na(.data$estimate)) %>%
  pivot_wider(names_from = "strata_level", values_from = "estimate") %>%
  select(cdm_name, group_level, variable, estimate_type, Overall, `COVID-19`, RA)
  

drug_use <- tibble(
  variable = c("number subjects", "Cumulative dose", "cumulative_dose", "Duration", 
               "duration", "Initial daily dose","initial_daily_dose", "Number eras",
               "number_eras", "Number exposures", "number_exposures")
) %>% left_join(drug_use,
                by = "variable") %>%
  mutate(variable = case_when(
    variable == "number subjects" ~ "Number subjects (N)",
    variable == "Number eras" ~ "Number eras*",
    variable == "Number exposures" ~ "Number exposures**",
    .default = variable
  )) %>%
  mutate(variable = if_else(
    is.na(estimate_type) | variable == "Number subjects (N)",
    variable,
    estimate_type
  )) %>%
  rename("Variable" = "variable")

## caption ----
captions <- captions %>%
  mutate(caption_du = paste0("Characteristics of drug usage** on new users of ", drug_name, ", in the database ", cdm_name, ".")) 


