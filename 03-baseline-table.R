# This file creates a table of SGLT2 vs non-SGLT2 patients across DM types
# It needs 01-tidy to be run first

# If 01-tidy not already run, run this
library(here)
source(here("scripts/01-tidy.R"))

# Specific packages for this script
library(gt)

## Baseline age ----
age_baseline <- merged_tidy %>% 
  filter(dka == "Yes",
         is.na(readmission)) %>% 
  group_by(sglt2) %>% 
  summarise(n = n(),
            mean = mean(age),
            sd = sd(age),
            median = median(age),
            Q1 = quantile(age, 0.25),
            Q3 = quantile(age, 0.75),
            min = min(age),
            max = max(age),
            min_max = NA,
            IQR = NA) %>% 
  pivot_longer(cols = c(n, mean, median, min_max, IQR), names_to = "label", values_to = "val") %>% 
  mutate(
    sd = ifelse(label == "mean", sd, NA),
    max = ifelse(label == "min_max", max, NA),
    min = ifelse(label == "min_max", min, NA),
    Q1 = ifelse(label == "IQR", Q1, NA),
    Q3 = ifelse(label == "IQR", Q3, NA),
    label = recode(
      label,
      "mean" = "Mean (SD)",
      "min_max" = "Min - Max",
      "median" = "Median")) %>% 
  pivot_wider(id_cols = label, names_from = "sglt2", values_from = c(val, sd, min, max, Q1, Q3)) %>% 
  mutate(category = "Age (Years)")

## Column label n = ----
no_label <- age_baseline %>% 
  pull(val_No) %>% 
  .[1]

yes_label <- age_baseline %>% 
  pull(val_Yes) %>% 
  .[1]

## Demographic table ----
age_baseline %>% 
  gt(rowname_col = "label",
     groupname_col = "category") %>% 
  tab_header(
    title = "Baseline characteristics of patients admitted with Diabetic Ketoacidosis",
    subtitle = "NHS Fife Critical Care 01/02/2023-31/01/2024"
  ) %>%
  tab_spanner(
    label = "SGLT2i use",
    columns = starts_with("val_")) %>% 
  fmt_percent(
    columns = starts_with("pct_"),
    decimals = 1) %>% 
  fmt_integer(columns = starts_with(c("val_", "min_", "max_", "Q1_", "Q3_")), rows = label %in% c("n", "Median", "Min-Max", "Female", "Male", "IQR")) %>% 
  fmt_number(
    columns = starts_with("val_"),
    rows = label == "Mean (SD)",
    decimals = 1) %>% 
  fmt_number(
    columns = starts_with("sd_"),
    rows = label == "Mean (SD)",
    decimals = 2) %>% 
  # Each group needs one of these
  cols_merge(
    columns = c("val_No", "sd_No", "min_No", "max_No", "Q1_No", "Q3_No"),
    pattern = "<<{1}>><< ({2})>><<{3} - {4}>><<{5} - {6}>>") %>% 
  cols_merge(
    columns = c("val_Yes", "sd_Yes", "min_Yes", "max_Yes", "Q1_Yes", "Q3_Yes"),
    pattern = "<<{1}>><< ({2})>><<{3} - {4}>><<{5} - {6}>>") %>% 
  tab_stub_indent(
    rows = everything(),
    indent = 5) %>%  
  opt_align_table_header(align = "left") %>% 
  cols_width(
    starts_with("val_") ~ px(140),
    1 ~ px(250)) %>% 
  cols_label("val_No" = md("No (n = {no_label}"), 
             "val_Yes" = md("Yes (n = {yes_label}")) 
