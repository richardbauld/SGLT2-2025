# If 01-tidy not already run, run this
library(here)
source(here("scripts/01-tidy.R"))

library(gtsummary)
library(gt)

acp_export_icu <- read_xlsx(here("data/acp_export_icu.xlsx"))
acp_export_mhdu <- read_xlsx(here("data/acp_export_mhdu.xlsx"))
acp_export_shdu <- read_xlsx(here("data/acp_export_shdu.xlsx"))


acp_merge <- bind_rows(acp_export_mhdu, acp_export_shdu, acp_export_icu)

acp_merge <- acp_merge %>% 
  clean_names() %>% 
  mutate(date = dmy(admission_unit_admission_date),
         demographic_chi_number = str_pad(demographic_chi_number, width = 10, side = "left", pad = "0"),
         acp_respiratory_support_mode = replace_na(acp_respiratory_support_mode, "none"),) 
  #       year = as.numeric(substr(admission_id_number, 1, 4)))
  
# Going to need to extract maximum support provided, so need to order the levels
support_levels <- c("none", "cannula", "high flow", "mask", "hood", "ett", "trache")


data_merge <- data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  select(chi_number, unit, year, sex, age, unit_outcome, hospital_outcome, sglt2, date, type, cc_los) %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(acp_merge, by = c("chi_number" = "demographic_chi_number", "date" = "date")) %>% 
  mutate(
    # This splits the resp support into a usable list
    # Split the string into a list of support types
    support_list = str_split(acp_respiratory_support_mode, ",\\s*"),
    
    # Convert to factor with ordered levels
    support_list = map(support_list, ~ factor(.x, levels = support_levels, ordered = TRUE)),
    
    # Get the max support level for each patient
    max_support = map_chr(support_list, ~ as.character(max(.x)),)

  ) %>%
  mutate(
    # This splits the HF into usable list
    # Split and trim whitespace
    high_flow_list = str_split(acp_respiratory_support_high_flow_nasal_oxygen_administered, ",\\s*"),
    high_flow_list = map(high_flow_list, ~ str_trim(.x)),
    
    # Check for "yes"
    high_flow_used = map_chr(high_flow_list, ~ if ("Yes" %in% .x) "Yes" else "No"),
    max_support = if_else(max_support == "cannula" & high_flow_used == "Yes", "high flow", max_support),
    
    max_support = factor(max_support, levels = support_levels, ordered = TRUE)
  ) %>%
  mutate(
    # Basic vasoactive split
    basic_vaso_list = str_split(acp_cardio_single_iv_vasoactive_drugs_or_iv_antiarrhythmics, ",\\s*"),
    basic_vaso_list = map(basic_vaso_list, ~ str_trim(.x)),
    basic_vaso_used = map_chr(basic_vaso_list, ~ if ("Yes" %in% .x) "Yes" else "No")
  ) %>% 
  mutate(
    # Advanced vasoactive split
    advanced_vaso_list = str_split(acp_cardio_multi_iv_vasoactive_drugs_antiarrhythmics, ",\\s*"),
    advanced_vaso_list = map(advanced_vaso_list, ~ str_trim(.x)),
    advanced_vaso_used = map_chr(advanced_vaso_list, ~ if ("Yes" %in% .x) "Yes" else "No")
  ) %>% 
  mutate(basic_vaso_only = case_when(basic_vaso_used == "Yes" & advanced_vaso_used == "No" ~ "Yes", TRUE ~ "No"),
         rrt = case_when(acp_summary_renal_days >= 1 ~ "Yes", TRUE ~ "No"),
         type_group = case_when(type == "1" ~ "Type 1",
                                       type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                                       type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i"),
         max_support = recode(max_support, "none" = "None", "cannula" = "Nasal cannula",
                              "high flow" = "High Flow", "mask" = "CPAP", "hood" = "CPAP", "ett" = "Intubated",
                              "trache" = "Tracheostomy"),
         resp_level = case_when(max_support == "None" ~ "None",
                                max_support == "Nasal cannula" ~ "Oxygen",
                                max_support == "High Flow" | max_support == "CPAP" ~ "Non-invasive",
                                max_support == "Intubated" | max_support == "Tracheostomy" ~ "Invasive ventilation"),
         resp_level = factor(resp_level, levels = c("None", "Oxygen", "Non-invasive", "Invasive ventilation"), ordered = TRUE)
        
         )

levels(data_merge$max_support)

data_check <- data_merge %>% 
  filter(max_support == "High Flow")

tbl_icu <-
  data_merge %>% 
  tbl_summary(by = type_group,
              include = c(resp_level, basic_vaso_only, advanced_vaso_used, rrt),
              label = list(resp_level ~ "Maximum respiratory support",
                           basic_vaso_only ~ "Single IV vasoactive used",
                           advanced_vaso_used ~ "Multiple IV vasoactives used",
                           rrt ~ "Renal replacement therapy"
                           ),
              type = all_continuous() ~ "continuous2",
              
              ) %>% 
  add_p(test.args = all_tests("fisher.test") ~ list(workspace = 2e6), include = c("resp_level", "basic_vaso_only", "advanced_vaso_used", "rrt")) %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Organ support provided") %>% 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**Diabetes type**")

tbl_icu

tbl_icu_gt <- as_gt(tbl_icu)
# Need this bit to point Rstudio to where chrome is
Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_icu_gt, filename = here("output/organ_support.png"))
