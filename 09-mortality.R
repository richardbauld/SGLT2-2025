# This file creates a table of SGLT2 vs non-SGLT2 patients across DM types
# It needs 01-tidy to be run first

# If 01-tidy not already run, run this
library(here)
source(here("scripts/01-tidy.R"))

# Specific packages for this script
library(gtsummary)
library(gt)

theme_gtsummary_journal(journal = "jama")


## Mortality table ----
tbl_mort <-
data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  mutate(type_group = case_when(type == "1" ~ "Type 1",
                                type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                                type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i")) %>% 
  tbl_summary(by = type_group,
              include = c(unit_outcome, hospital_outcome),
              label = list(unit_outcome ~ "Critical care outcome",
                           hospital_outcome ~ "Hospital outcome"),
              type = all_continuous() ~ "continuous2",
              ) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Critical care and hospital mortality") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Diabetes type**")

tbl_mort

tbl_mort_gt <- as_gt(tbl_mort)
# Need this bit to point Rstudio to where chrome is
Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_mort_gt, filename = here("output/mortality.png"))


## Mortality difference ----
### Organise data ----
tbl_mort_data <- data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  mutate(type_group = case_when(type == "1" ~ "Type 1",
                                type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                                type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i"))
### Type 1 table ----
tbl_1 <- tbl_mort_data %>% 
  filter(type_group == "Type 1") %>% 
  tbl_summary(by = c(hospital_outcome),
              include = c(age, sex, bicarb),
              label = list(age ~ "Age (years)",
                           sex ~ "Sex",
                           bicarb ~ "Bicarbonate, mmol/L"),
              type = all_continuous() ~ "continuous2",
              statistic = list(age ~ c(
                "{median} ({p25} - {p75})",
                "{min} - {max}"),
                bicarb ~ c(
                  "{median} ({p25} - {p75})",
                  "{min} - {max}"))
  ) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Characteristics of deaths and survivors") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Type 1**")

### Type 2 without SGLT2i ----
tbl_2_no <- tbl_mort_data %>% 
  filter(type_group == "Type 2 no SGLT2i") %>% 
  tbl_summary(by = c(hospital_outcome),
              include = c(age, sex, bicarb),
              label = list(age ~ "Age (years)",
                           sex ~ "Sex",
                           bicarb ~ "Bicarbonate, mmol/L"),
              type = all_continuous() ~ "continuous2",
              statistic = list(age ~ c(
                "{median} ({p25} - {p75})",
                "{min} - {max}"),
                bicarb ~ c(
                  "{median} ({p25} - {p75})",
                  "{min} - {max}"))
  ) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Characteristics of deaths and survivors") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Type 2 (no SGLT2i)**")

### Type 2 with SGLT2i ----
tbl_2_yes <- tbl_mort_data %>% 
  filter(type_group == "Type 2 SGLT2i") %>% 
  tbl_summary(by = c(hospital_outcome),
              include = c(age, sex, bicarb),
              label = list(age ~ "Age (years)",
                           sex ~ "Sex",
                           bicarb ~ "Bicarbonate, mmol/L"),
              type = all_continuous() ~ "continuous2",
              statistic = list(age ~ c(
                "{median} ({p25} - {p75})",
                "{min} - {max}"),
                bicarb ~ c(
                  "{median} ({p25} - {p75})",
                  "{min} - {max}"))
  ) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Characteristics of deaths and survivors") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Type 2 (SGLT2i)**")

tbl_2_yes

### Merge all tables
tbl_merge_mort <-
  tbl_merge(tbls = list(tbl_1, tbl_2_no, tbl_2_yes),
  tab_spanner = c("**Type 1**", "**Type 2 (no SGLT2i)**", "**Type 2 (SGLT2i)**"))

tbl_merge_mort

tbl_merge_mort_gt <- as_gt(tbl_merge_mort)
# Need this bit to point Rstudio to where chrome is
Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_merge_mort_gt, filename = here("output/mortality_character.png"))

## Diagnosis for deaths ----
top_diagnoses_deaths <- data_tidy %>%
  filter(dka == "Yes",
         type %in% c("1", "2"),
         hospital_outcome == "Died") %>% 
  mutate(type_group = case_when(type == "1" ~ "Type 1",
                                type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                                type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i")) %>% 
  group_by(type_group, trigger) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(type_group) %>%
  mutate(percent = (count / sum(count)) * 100) %>%
  arrange(type_group, desc(count)) %>% 
  group_by(type_group) %>%
  slice_head(n = 5) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>% 
  mutate(trigger = recode(trigger, "CAP" = "Community Acquired Pneumonia",
                          "SGLT2" = "SGLT2i induced"))

top_diagnoses_deaths <- top_diagnoses_deaths %>%
  mutate(formatted = paste0(count, " (", round(percent, 1), ")"))


# Create wide format
top_diagnoses_death_wide <- top_diagnoses_deaths %>%
  select(type_group, rank, trigger, formatted) %>%
  pivot_wider(
    names_from = type_group,
    values_from = c(trigger, formatted),
    names_glue = "{type_group}_{.value}"
  ) %>%
  arrange(rank)



# Tables for output ----
## Diagnoses table ----
tbl_diagnoses_deaths <- 
  top_diagnoses_death_wide %>%
  gt(rowname_col = "rank") %>%
  tab_stubhead(label = "Ranking") %>% 
  cols_label(
    `Type 1_trigger` = "Diagnosis",
    `Type 1_formatted` = "n (%)",
    `Type 2 no SGLT2i_trigger` = "Diagnosis",
    `Type 2 no SGLT2i_formatted` = "n (%)",
    `Type 2 SGLT2i_trigger` = "Diagnosis",
    `Type 2 SGLT2i_formatted` = "n (%)"
  ) %>%
  tab_header(
    title = "Top 5 triggers for DKA resulting in death in hospital by diabetes type") %>% 
  tab_spanner(label = "Type 1 Diabetes", columns = starts_with("Type 1")) %>%
  tab_spanner(label = "Type 2 (no SGLT2i)", columns = starts_with("Type 2 no SGLT2i")) %>%
  tab_spanner(label = "Type 2 (SGLT2i)", columns = starts_with("Type 2 SGLT2i")) %>%
  cols_label(rank = "Rank") %>% 
  tab_options(
    table.font.size = px(14),
    column_labels.font.size = px(14),
    table.border.top.width = px(0),
    table.border.bottom.width = px(1),
    table.border.top.color = "black",
    table.border.bottom.color = "black",
    data_row.padding = px(8)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", color = "black"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(weight = "normal"),
    locations = cells_body(columns = everything())
  ) %>% 
  tab_style(
    style = cell_text(weight = "bold", size = px(14), align = "left"),
    locations = list(cells_title(), cells_stubhead())
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold", size = px(14), align = "center")
    ),
    locations = cells_column_spanners(spanners = c("Type 1 Diabetes", 
                                                   "Type 2 (no SGLT2i)", 
                                                   "Type 2 (SGLT2i)"))
  )

tbl_diagnoses_deaths

Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_diagnoses_deaths, filename = here("output/diagnoses_deaths.png"))


## Mortality with type 2 combined ----

  data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  mutate(type_group = case_when(type == "1" ~ "Type 1",
                                type == "2" ~ "Type 2",
                                )) %>% 
  tbl_summary(by = type_group,
              include = c(unit_outcome, hospital_outcome),
              label = list(unit_outcome ~ "Critical care outcome",
                           hospital_outcome ~ "Hospital outcome"),
              type = all_continuous() ~ "continuous2",
  ) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Critical care and hospital mortality") %>% 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Diabetes type**")

tbl_mort_gt <- as_gt(tbl_mort)
# Need this bit to point Rstudio to where chrome is
Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_mort_gt, filename = here("output/mortality.png"))


check_no_insulin <- data_tidy %>% 
  filter(dka == "Yes") %>% 
  filter(trigger == "Missed insulin") %>% 
  group_by(hospital_outcome) %>% 
  summarise(n = n())

overall_mortality <- data_tidy %>% 
  filter(dka == "Yes") %>% 
  group_by(hospital_outcome) %>% 
  summarise(n = n())
