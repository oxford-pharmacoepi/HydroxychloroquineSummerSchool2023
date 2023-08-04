# Use of Hydroxychloroquine 

Code for the research study conducted during the practical sessions of RWE Summer School 2023.

Sciprt `CodeToRun.R` loads the necessary packages, connecto to the database, and set some study parameters. Next, it runs the script contained in each of the folders ("1_InstantiateCohorts", "2_CharacteriseNewUsers", and "3_IncidencePrevalence"). Each folder contains analytical code to:
- 1_InstantiateCohorts: instantiate COVID-19, rheumatoid arthritis, and malaria cohorts from JSON files, and a cohort of new users of hydroxychloroquine.
- 2_CharacteriseNewUsers: characterise new users of hydroxychloroquine and methotrexate.
- 3_IncidencePrevalence: estimate incidence rates and prevalence of hydroxychloroquine and methotrexate in the general population, and on those with COVID-19, rheumatoid arthritis, and malaria during the study period.

Finally, the folder "Shiny" creates a shiny app to visualised the results obtained.

