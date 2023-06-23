### DEVELOP PLOT
design <- matrix(c(1,2,3,1,2,3,4,4,4), 3, 3, byrow = TRUE)
layout(design)

data_ip_cases %>%
  ggplot(aes(x = start_date, y = point, color = outcome)) +
  geom_point() +
  geom_line() +
    facet_manual(rows = vars(facet),
              cols = vars(denominator),
              scales = "free_y", 
              independent = "y",
              design = design) +
  scale_color_manual(values = c("#000000", "#2a9d8f", "#f4a261")) +
  xlab("Date") +
  ylab("") +
  guides(colour = guide_legend(" "))
