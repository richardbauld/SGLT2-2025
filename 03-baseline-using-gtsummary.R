# This file creates a table of SGLT2 vs non-SGLT2 patients across DM types
# It needs 01-tidy to be run first

# If 01-tidy not already run, run this
library(here)
source(here("scripts/01-tidy.R"))

# Specific packages for this script
library(gtsummary)
library(gt)

theme_gtsummary_journal(journal = "jama")


## Baseline table ----
#tbl_base <-
data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  tbl_summary(by = type,
              include = c(age, sex, bicarb, cc_los),
              label = list(age ~ "Age, years",
                           sex ~ "Sex",
                           bicarb ~ "Bicarbonate, mmol/L",
                           cc_los ~ "Length of stay, days"),
              type = all_continuous() ~ "continuous2",
              statistic = list(age ~ c(
                "{median} ({p25} - {p75})",
                "{min} - {max}"),
                bicarb ~ c(
                  "{median} ({p25} - {p75})",
                  "{min} - {max}"),
                cc_los ~ c(
                  "{median} ({p25} - {p75})",
                  "{min} - {max}")
              )) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Baseline characteristics") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Diabetes type**")

  
## Save table ----
# To output table as png, need to set it to be as a gt table instead
tbl_baseline_gt <- as_gt(tbl_base)
# Need this bit to point Rstudio to where chrome is
Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_baseline_gt, filename = here("output/table1.png"))

## SGLT2i yes or no ----
tbl_dka <-
  data_tidy %>% 
  filter(dka == "Yes",
         type == "2") %>% 
  tbl_summary(by = sglt2,
              include = c(age, sex, bicarb, cc_los),
              label = list(age ~ "Age, years",
                           sex ~ "Sex",
                           bicarb ~ "Bicarbonate, mmol/L",
                           cc_los ~ "Length of stay, days"),
              type = all_continuous() ~ "continuous2",
              statistic = list(age ~ c(
                "{median} ({p25} - {p75})",
                "{min} - {max}"),
                bicarb ~ c(
                  "{median} ({p25} - {p75})",
                  "{min} - {max}"),
                cc_los ~ c(
                  "{median} ({p25} - {p75})",
                  "{min} - {max}")
              )) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Type 2 diabetics in DKA and SGLT2i status") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**SGLT2i use**")

tbl_dka
tbl_dka_gt <- as_gt(tbl_dka)
# Need this bit to point Rstudio to where chrome is
Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_dka_gt, filename = here("output/baseline_sglt2.png"))

