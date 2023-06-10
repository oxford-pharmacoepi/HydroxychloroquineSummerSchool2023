# Incidence and Prevalence----
general <- getHCQIncidencePrevalence(generalDenTableName) # general pop
covid   <- getHCQIncidencePrevalence(covidDenTableName)   # covid pop
ra      <- getHCQIncidencePrevalence(raDenTableName)      # ra pop
malaria <- getHCQIncidencePrevalence(malariaDenTableName) # malaria pop

# all incidence results 
incidence_hcq <- general$incidence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(covid$incidence) %>%
  union_all(ra$incidence) %>%
  union_all(malaria$incidence) 
# export to csv 
write_csv(incidence_hcq, 
          file = here(output_folder, "incidence_hcq.csv"))

# all prevalence results 
prevalence_hcq <- general$prevalence %>%
  mutate(denominator_strata_cohort_name = "general") %>%
  union_all(covid$prevalence) %>%
  union_all(ra$prevalence) %>%
  union_all(malaria$prevalence) 
# export to csv 
write_csv(prevalence_hcq, 
          file = here(output_folder, "prevalence_hcq.csv"))
