load("atc.RData")
result_ATC_hcq <- summariseCharacteristicsFromCodelist(
  cohort = cdm[[hcq_new_users_table_name]],
  cdm = cdm,
  conceptSetList = atc_codes,
  strata = list("Calendar time" = "window"),
  window = list(c(-365, -1), c(-30, -1), c(0, 0), c(1, 30), c(1, 365)),
  overlap = TRUE,
  minCellCount = minimum_counts
)
