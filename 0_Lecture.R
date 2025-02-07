library(haven)
library(tidyverse)


base <- read_dta("LYCEES_base.dta")

# Transformation des variables labellisées en facteurs si nécessaire
base <- base %>% mutate_if(is.labelled, as_factor)
