# This file creates a table of SGLT2 vs non-SGLT2 patients across DM types
# It needs 01-tidy to be run first

# If 01-tidy not already run, run this
library(here)
source(here("scripts/01-tidy.R"))

# Specific packages for this script
library(gtsummary)
library(gt)

theme_gtsummary_journal(journal = "jama")

tbl_unit_mort <-
  data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  mutate(type_group = case_when(type == "1" ~ "Type 1",
                                type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                                type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i")) %>% 
  tbl_summary(by = unit,
              include = c(unit_outcome, hospital_outcome),
              label = list(unit_outcome ~ "Critical care outcome",
                           hospital_outcome ~ "Hospital outcome"),
              type = all_continuous() ~ "continuous2",
  ) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Critical care and hospital mortality depending on critical care area") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Critical care unit**")

tbl_unit_mort

bed_day <- data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  mutate(type_group = case_when(type == "1" ~ "Type 1",
                                type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                                type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i")) %>% 
  group_by(type_group) %>% 
  summarise(bed_days = sum(cc_los))


