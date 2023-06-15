# Incidence and Prevalence----
# HCQ 
hcq_general <- getIncidencePrevalence(ip_general_table_name, hcq_users_table_name) # general pop
hcq_covid   <- getIncidencePrevalence(ip_covid_table_name, hcq_users_table_name)   # covid pop
hcq_ra      <- getIncidencePrevalence(ip_ra_table_name, hcq_users_table_name)      # ra pop
hcq_malaria <- getIncidencePrevalence(ip_malaria_table_name, hcq_users_table_name) # malaria pop

# MTX 
mtx_general <- getIncidencePrevalence(ip_general_table_name, mtx_users_table_name) # general pop
mtx_ra      <- getIncidencePrevalence(ip_ra_table_name, mtx_users_table_name)      # ra pop

# Export HCQ results ----
# hcq incidence results 
incidence_hcq <- hcq_general$incidence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  # union_all(hcq_covid$incidence) %>%
  union_all(hcq_ra$incidence) %>%
  union_all(hcq_malaria$incidence) 
# export to csv 
write_csv(incidence_hcq, 
          file = here(output_folder, "incidence_hcq.csv"))

# hcq prevalence results 
prevalence_hcq <- hcq_general$prevalence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  # union_all(hcq_covid$prevalence) %>%
  union_all(hcq_ra$prevalence) %>%
  union_all(hcq_malaria$prevalence) 
# export to csv 
write_csv(prevalence_hcq, 
          file = here(output_folder, "prevalence_hcq.csv"))

# Export MTX results ----
# mtx incidence results 
incidence_mtx <- mtx_general$incidence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(mtx_ra$incidence)
# export to csv 
write_csv(incidence_mtx, 
          file = here(output_folder, "incidence_mtx.csv"))

# mtx prevalence results 
prevalence_mtx <- mtx_general$prevalence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(mtx_ra$prevalence) 
# export to csv 
write_csv(prevalence_mtx, 
          file = here(output_folder, "prevalence_mtx.csv"))