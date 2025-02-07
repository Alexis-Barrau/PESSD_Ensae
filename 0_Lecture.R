data <- read_dta("LYCEES_base.dta")

# Transformation des variables labellisées en facteurs si nécessaire
data <- data %>% mutate_if(is.labelled, as_factor)
