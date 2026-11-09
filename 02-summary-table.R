# This file creates a table of SGLT2 vs non-SGLT2 patients across DM types
# It needs 01-tidy to be run first

# If 01-tidy not already run, run this
library(here)
source(here("scripts/01-tidy.R"))

library(gt)

## Create merged dataframe for the table
merged_summary <- merged_tidy %>% 
  filter(dka == "Yes",
         is.na(readmission)) %>% 
  mutate(type = recode(type, "LADA" = "Other", "Steroid" = "Other")) %>% 
  select(type,sglt2) %>% 
  group_by(type, sglt2) %>% 
  summarise(count = n()) %>% 
  group_by(type) %>% 
  mutate(pct = count / sum(count))

### Summary of SGLT2 and non SGLT2 DKA across DM type ----
merged_summary %>% 
  mutate(type = recode(type, "1" = "Type 1", "2" = "Type 2"),
         sglt2 = recode(sglt2, "No" = "No SGLT2i", "Yes" = "Prescribed SGLT2i")) %>% 
  gt(rowname_col = "sglt2",
     groupname_col = "type") %>% 
  tab_header(
    title = "SGLT2i associated Diabetic Ketoacidosis",
    subtitle = "NHS Fife Critical Care 01/02/2023-31/01/2024"
  ) %>%
  #tab_options(table.font.size = 12) %>% 
  fmt_percent(
    columns = starts_with("pct"),
    decimals = 1) %>% 
  fmt_integer(columns = starts_with("count")) %>% 
  # Each year needs one of these
  cols_merge(
    columns = c("count", "pct"),
    pattern = "<<{1}>><< ({2})>>") %>% 
  tab_stub_indent(
    rows = everything(),
    indent = 5) %>%  
  opt_align_table_header(align = "left") %>% 
  cols_width(
    starts_with("val_") ~ px(120),
    1 ~ px(120)) %>% 
  cols_label(count = "n (%)") #%>% 
#cols_move_to_end(columns = contains("Total"))



unit_summary <- merged_tidy %>% 
  filter(dka == "Yes",
         is.na(readmission)) %>% 
  mutate(type = recode(type, "LADA" = "Other", "Steroid" = "Other")) %>% 
  select(type,sglt2, unit) %>% 
  group_by(type, sglt2, unit) %>% 
  summarise(count = n()) %>% 
  group_by(type) %>% 
  mutate(pct = count / sum(count)) %>% 
  pivot_wider(names_from = "unit", values_from = c(count, pct))


unit_summary %>% 
  mutate(type = recode(type, "1" = "Type 1", "2" = "Type 2")) %>% 
  gt(rowname_col = "sglt2",
     groupname_col = "type") %>% 
  tab_header(
    title = "SGLT2i associated Diabetic Ketoacidosis",
    subtitle = "NHS Fife Critical Care 01/02/2023-31/01/2024"
  ) %>%
  #tab_options(table.font.size = 12) %>% 
  fmt_percent(
    columns = starts_with("pct_"),
    decimals = 1) %>% 
  fmt_integer(columns = starts_with("count_")) %>% 
  # Each year needs one of these
  cols_merge(
    columns = c("count_ICU", "pct_ICU"),
    pattern = "<<{1}>><< ({2})>>") %>% 
  cols_merge(
    columns = c("count_MHDU", "pct_MHDU"),
    pattern = "<<{1}>><< ({2})>>") %>% 
  cols_merge(
    columns = c("count_SHDU", "pct_SHDU"),
    pattern = "<<{1}>><< ({2})>>") %>% 
  tab_stub_indent(
    rows = everything(),
    indent = 5) %>%  
  opt_align_table_header(align = "left") %>% 
 # cols_width(
 #   starts_with("val_") ~ px(120),
 #   1 ~ px(120)) #%>% 
  cols_label("count_ICU" = "ICU", "count_MHDU" = "MHDU", "count_SHDU" = "SHDU") #%>% 
#cols_move_to_end(columns = contains("Total"))

## Trial with gtsummary ----
merged_tidy %>% 
  filter(dka == "Yes",
         is.na(readmission)) %>%
  tbl_summary()

