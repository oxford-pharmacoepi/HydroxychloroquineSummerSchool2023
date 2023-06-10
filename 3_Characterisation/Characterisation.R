## 1. INDICATION ----
indication_table <- cdm[[hcqTableName]] %>%
  addIndication(cdm, populationCohortName, indicationGap = c(0, 7, 30)) %>% # Indication at 0, 7, and 30 days
  addDemographics(
    cdm,
    ageGroup = age_groups[1:length(age_groups)-1],
    priorHistory = FALSE,
    futureObservation = FALSE,
  ) %>%
  mutate(window = case_when(
    .data$cohort_start_date >= !! window.before[1] & .data$cohort_start_date <= !! window.before[2] ~
      "before",
    .data$cohort_start_date >= !! window.hcq[1] & .data$cohort_start_date <= !! window.hcq[2] ~
      "hcq",
    .data$cohort_start_date >= !! window.after[1] & .data$cohort_start_date <= !! window.after[2] ~
      "after",
    .default = NA
    )
  ) %>%
  filter(!is.na(window)) %>%
  compute()

indication <- indication_table %>%
  group_by(window) %>%
  summarise(
    across(starts_with("indication"), ~ sum(.x)), 
    .groups = "drop"
  ) %>%
  mutate(
    age_group = "0;150",
    sex = "Both"
  ) %>%
  collect() %>%
  union_all(
    indication_table %>%
      group_by(window, age_group, sex) %>%
      summarise(across(starts_with("indication"), ~ sum(.x)), .groups = "drop") %>%
      collect()
  )

write_csv(indication, here(output_folder, "indication_hcq.csv"))


## 2. BASELINE CHARACTERISTICS ----


