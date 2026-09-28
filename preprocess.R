library(here)
library(readxl)
library(tidyverse)

pfas <- read_xlsx(here::here("data", "testing_results.xlsx"),
                  sheet = "PFAS")
mp <- read_xlsx(here::here("data", "testing_results.xlsx"),
                sheet = "Microplastics")
chain.length <- read_xlsx(here::here("data", "chain_length.xlsx"))

mp <- mp %>%
  rename(
    id = "Sample ID",
    category = "Type",
    bottle_material = "Bottle Material",
    source_type = "Source Type",
    adv_filtration = "Advanced Filtration",
    polymer = "MP Polymer",
    unit = "Unit",
    particles = "Result"
  ) %>%
  mutate(particles = replace_values(particles, "ND" ~ "0")) %>%
  # this is a typo
  mutate(particles = replace_values(particles, "2.9" ~ "3")) %>%
  mutate(particles = as.numeric(particles)) %>%
  # we will calculate our own total later
  filter(polymer != "total" & polymer != "Total") %>%
  mutate(adv_filtration = fct_recode(adv_filtration, "Yes" = "Y", "No" = "N")) %>%
  mutate(category = as_factor(category),
         bottle_material = as_factor(bottle_material),
         source_type = as_factor(source_type),
         adv_filtration = as_factor(adv_filtration))

pfas <- pfas %>%
  rename(
    id = "Sample ID",
    category = "Type",
    source_type = "Source Type",
    adv_filtration = "Advanced Filtration",
    analyte = "PFAS Analyte",
    analyte_abbrv = "PFAS Abbreviation",
    unit = "Unit",
    concentration = "Result"
  ) %>%
  # make sure it is read as a number
  mutate(concentration = as.numeric(concentration)) %>%
  # add in chain length information
  left_join(chain.length, join_by(analyte_abbrv == analyte_abbrv)) %>%
  mutate(adv_filtration = fct_recode(
    adv_filtration, "Yes" = "Y", "No" = "N")
  ) %>%
  mutate(source_type = fct_recode(
    source_type, "Non-spring" = "Non-Spring")
  ) %>%
  mutate(category = as_factor(category),
         source_type = as_factor(source_type),
         adv_filtration = as_factor(adv_filtration))

saveRDS(mp, file = here::here("out", "microplastics.rds"))
saveRDS(pfas, file = here::here("out", "pfas.rds"))
