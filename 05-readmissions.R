# If 01-tidy not already run, run this
library(here)
source(here("scripts/01-tidy.R"))

library(gtsummary)
library(gt)
library(outliers)

## Readmissions ----
readmission <- data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>%
  mutate(type_group = case_when(type == "1" ~ "Type 1",
                              type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                              type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i")) %>% 
  group_by(type_group, chi_number) %>%
  count() %>% 
  filter(n > 1)

readmission_check <- readmission %>% 
  filter(type_group == "Type 2 SGLT2i")

## Readmission gt table ----
tbl_read <-
  readmission %>% 
  tbl_summary(by = type_group,
              include = c(n),
              label = list(n ~ "Readmissions"),
              type = all_continuous() ~ "continuous2",
              statistic = list(n ~ c(
                "{median} ({p25} - {p75})",
                "{min} - {max}"))
  ) %>% 
  add_p() %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> Readmissions for DKA, by type")
tbl_read

tbl_read_gt <- as_gt(tbl_read)
# Need this bit to point Rstudio to where chrome is
Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_read_gt, filename = here("output/readmissions.png"))


## Readmissions by year ----

readmission_year <- data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>%
  mutate(type_group = case_when(type == "1" ~ "Type 1",
                                type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                                type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i")) %>% 
  group_by(type_group, year, chi_number) %>%
  count() %>% 
  filter(n > 1) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  summarise(total_readmissions = sum(n))

## Do a z score for readmissions per year to identify if there is an outlier
dka_z <- readmission_year %>%
  mutate(
    med = median(total_readmissions),
    mad = median(abs(total_readmissions - med)),
    mod_z = 0.6745 * (total_readmissions - med) / mad,
    is_outlier = abs(mod_z) > 3.5
  )

# Grubbs test (outliers package)
grubbs.test(readmission_year$total_readmissions, type = 10)

tbl_read_year <-
  readmission_year %>% 
  gt() %>%
 # tab_stubhead(label = "Ranking") %>% 
  cols_label(
    `year` = "Year",
    `total_readmissions` = "Readmissions"
  ) %>%
  tab_header(
    title = "Total DKA readmissions per year") %>% 
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
  )

tbl_read_year

Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_read_year, filename = here("output/readmissions_year.png"))


  