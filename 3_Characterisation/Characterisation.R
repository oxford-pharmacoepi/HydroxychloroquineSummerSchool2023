## PREPARE DATA ----
# Add calendar windows to new user cohorts:
cdm[[hcq_new_users_table_name]] <- cdm[[hcq_new_users_table_name]] %>%
  mutate(window = case_when(
    .data$cohort_start_date >= !! window.before[1] & .data$cohort_start_date <= !! window.before[2] ~
      "before",
    .data$cohort_start_date >= !! window.hcq[1] & .data$cohort_start_date <= !! window.hcq[2] ~
      "during",
    .data$cohort_start_date >= !! window.after[1] & .data$cohort_start_date <= !! window.after[2] ~
      "after",
    .default = NA
  )) %>%
  filter(!is.na(window)) 

cdm[[mtx_new_users_table_name]] <- cdm[[mtx_new_users_table_name]] %>%
  mutate(window = case_when(
    .data$cohort_start_date >= !! window.before[1] & .data$cohort_start_date <= !! window.before[2] ~
      "before",
    .data$cohort_start_date >= !! window.hcq[1] & .data$cohort_start_date <= !! window.hcq[2] ~
      "during",
    .data$cohort_start_date >= !! window.after[1] & .data$cohort_start_date <= !! window.after[2] ~
      "after",
    .default = NA
  )) %>%
  filter(!is.na(window))

## 1. INDICATION ----
# Package: DrugUtilisation 
# HCQ
hcq_indication_table <- cdm[[hcq_new_users_table_name]] %>%
  addIndication(cdm, study_table_name, indicationGap = c(0, 7, 30)) # Indication at 0, 7, and 30 days

result_indication_hcq <- summariseIndication(hcq_indication_table, cdm, list("Calendar time" = "window"))

write_csv(result_indication_hcq, here(output_folder, "indication_hcq.csv"))

# MTX
mtx_indication_table <- cdm[[mtx_new_users_table_name]] %>%
  addIndication(cdm, study_table_name, indicationGap = c(0, 7, 30)) # Indication at 0, 7, and 30 days

result_indication_mtx <- summariseIndication(mtx_indication_table, cdm, list("Calendar time" = "window"))

write_csv(result_indication_mtx, here(output_folder, "indication_mtx.csv"))

## 2. BASELINE CHARACTERISTICS ---- 
# Package: DrugUtilisation  
# HCQ
hcq_result_characteristics <- summariseTableOne(
  cdm = cdm,
  cohort = cdm[[hcq_new_users_table_name]],
  ageGroup = age_groups[1:length(age_groups)-1],
  windowVisitOcurrence = c(-365,0),
  strata = list("Calendar time" = "window"),
  covariates =  eval(
    parse(
      text = paste0("list(", medications_table_name, " = c(-365, 0), ",
                    conditions_table_name," = c(-Inf, 0))")))
)

write_csv(hcq_result_characteristics, here(output_folder, "table_one_hcq.csv"))

# MTX
mtx_result_characteristics <- summariseTableOne(
  cdm = cdm,
  cohort = cdm[[mtx_new_users_table_name]],
  ageGroup = age_groups[1:length(age_groups)-1],
  windowVisitOcurrence = c(-365,0),
  strata = list("Calendar time" = "window"),
  covariates =  eval(
    parse(
      text = paste0("list(", medications_table_name, " = c(-365, 0), ",
                    conditions_table_name," = c(-Inf, 0))")))
)

write_csv(mtx_result_characteristics, here(output_folder, "table_one_mtx.csv"))

## 3. LARGE SCALE CHARACTERISATION ----
# Get ATC and ICD10 codes (package CodelistGenerator)
atc_codes   <- getATCCodes(cdm, "ATC 3rd")
icd10_codes <- getICD10StandardCodes(cdm, "ICD10 SubChapter")

# Large Scale Characteristics - ATC ----
# HCQ
result_ATC_hcq <- summariseCharacteristicsFromCodelist(
  cohort = cdm[[hcq_new_users_table_name]],
  cdm = cdm,
  conceptSetList = atc_codes,
  strata = list("Calendar time" = "window"),
  window = list(c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365)),
  overlap = TRUE,
  minCellCount = minimum_counts
)
write_csv(result_ATC_hcq, here(output_folder, "characteristics_atc_hcq.csv"))

# MTX
result_ATC_mtx <- summariseCharacteristicsFromCodelist(
  cohort = cdm[[mtx_new_users_table_name]],
  cdm = cdm,
  conceptSetList = atc_codes,
  strata = list("Calendar time" = "window"),
  window = list(c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365)),
  overlap = TRUE,
  minCellCount = minimum_counts
)
write_csv(result_ATC_mtx, here(output_folder, "characteristics_atc_mtx.csv"))

# Large Scale Characteristics - ICD10 ----
# HCQ
result_ICD10_hcq <- summariseCharacteristicsFromCodelist(
  cohort = cdm[[hcq_new_users_table_name]],
  cdm = cdm,
  conceptSetList = icd10_codes,
  strata = list("Calendar time" = "window"),
  window = list(c(-Inf, -1), c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365), c(1, Inf)),
  overlap = FALSE,
  minCellCount = minimum_counts
)
write_csv(result_ICD10_hcq, here(output_folder, "characteristics_icd10_hcq.csv"))

# MTX
result_ICD10_mtx <- summariseCharacteristicsFromCodelist(
  cohort = cdm[[mtx_new_users_table_name]],
  cdm = cdm,
  conceptSetList = icd10_codes,
  strata = list("Calendar time" = "window"),
  window = list(c(-Inf, -1), c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365), c(1, Inf)),
  overlap = FALSE,
  minCellCount = minimum_counts
)
write_csv(result_ICD10_mtx, here(output_folder, "characteristics_icd10_mtx.csv"))


save(atc_codes, file = "atc.RData")
save(icd10_codes, file = "icd.RData")
