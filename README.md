# Use of Hydroxychloroquine 

Code for the research study conducted during the practical sessions of RWE Summer School 2023.

Sciprt `CodeToRun.R` loads the necessary packages, connecto to the database, and set some study parameters. Next, it runs the script contained in each of the folders ("Day_1", "Day_2", and "Day_3"). Each folder contains analysis code that use the functions studied in the course during the day that the name indicates:
- Day_1: instantiates COVID-19, rheumatoid arthritis, and malaria cohorts from JSON files, and a cohort of new users of hydroxychloroquine.
- Day_2: characterisation of new users of hydroxychloroquine.
- Day_3: estimate incidence rates and prevalence of hydroxychloroquine in the general population, and on those with COVID-19, rheumatoid arthritis, and malaria during the study period.

Finally, the folder "Shiny" creates a shiny app to visualised the results obtained.

