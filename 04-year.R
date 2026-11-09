# If 01-tidy not already run, run this
library(here)
source(here("scripts/01-tidy.R"))

library(gtsummary)
library(ggpmisc)
library(gt)

year_trend <- data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  mutate(bar_type = case_when(type == "1" ~ "Type 1",
                              type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                              type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i")) %>% 
  group_by(year, bar_type) %>% 
  summarise(count = n())
  

## Year table ----

year_table <- data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  mutate(bar_type = case_when(type == "1" ~ "Type 1",
                              type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                              type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i"))


tbl_year_trend <-
year_table %>% 
  tbl_summary(by = year,
              include = c(bar_type),
              label = list(bar_type ~ "Type of diabetes"),
              type = all_continuous() ~ "continuous2",
              ) %>% 
  modify_header(label = "**Variable**", all_stat_cols() ~ "**{level}**<br>N = {n} ({style_percent(p)}%)") %>% 
  modify_caption("<div style='text-align: left; font-weight: bold'> DKA admissions, per year, by diabetes diagnosis")

tbl_year_trend

tbl_year_trend_gt <- as_gt(tbl_year_trend)
# Need this bit to point Rstudio to where chrome is
Sys.setenv(CHROMOTE_CHROME = "C:/Users/bauldr/AppData/Local/Google/Chrome/Application/chrome.exe")
gtsave(tbl_year_trend_gt, filename = here("output/admissions_year_trend.png"))

#year_trend %>% 
#  ggplot(aes(x = year, y = count, fill = bar_type)) +
#  geom_bar(position = "stack", stat = "identity") +
#  labs(x = "Year", y = "Patients") +
#  scale_y_continuous(expand = c(0, 0), limits = c(0, 200)) +
#  labs(
#    title = "DKA Admissions between 2020 and 2024",
#    x = "Year",
 #   y = "Admissions",
#    fill = "Diabetes group"
#  ) +
#  theme_minimal()

## Unique patients only ----
  
patient_year_trend <- data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  distinct(chi_number, .keep_all = TRUE) %>% 
  mutate(bar_type = case_when(type == "1" ~ "Type 1",
                              type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                              type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i")) %>% 
  group_by(year, bar_type) %>% 
  summarise(count = n())

patient_year_trend %>% 
  ggplot(aes(x = year, y = count, fill = bar_type)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x = "Year", y = "Patients") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 125)) +
  labs(
    title = "DKA unique patients between 2020 and 2024",
    x = "Year",
    y = "Admissions",
    fill = "Diabetes group"
  ) +
  theme_minimal()

## Unique patients only but with % chart ----

patient_year_trend_pct <- data_tidy %>% 
  filter(dka == "Yes",
         type %in% c("1", "2")) %>% 
  distinct(chi_number, .keep_all = TRUE) %>% 
  mutate(bar_type = case_when(type == "1" ~ "Type 1",
                              type == "2" & sglt2 == "Yes" ~ "Type 2 SGLT2i",
                              type == "2" & sglt2 == "No" ~ "Type 2 no SGLT2i")) %>%
  count(year, bar_type) %>%
  group_by(year) %>%
  mutate(percent = n / sum(n))

patient_year_trend_pct %>% 
  ggplot(aes(x = year, y = percent, fill = bar_type)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Year", y = "Patients") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "DKA unique patients, percent total patients, between 2020 and 2024",
    x = "Year",
    y = "Admissions",
    fill = "Diabetes group"
  ) +
  theme_minimal()

## Plot for trends ----
### Admissions ----
year_trend %>% 
#filter(!(year == 2020 & bar_type== "Type 1")) %>% 
ggplot(aes(x = year, y = count, group = bar_type, color = bar_type)) +
  geom_line(linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
  geom_point() +
  stat_poly_eq(
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = "~~~")),
    formula = y ~ x, 
    parse = TRUE,
    geom = "text_npc",
    # this changes the position of each label, x & y co-ordinates
    # with 1,2,3 within the brackets
    npcx = c(1, 0.9, 1),
    npcy = c(0.8, 0.3, 0.1)
  ) +
  labs(title = "DKA Admissions (including readmissions) over time by Diabetes Type",
       x = "Year",
       y = "Number of Admissions",
       color = "Diabetes Type") +
  theme_classic()

ggsave(here("output/admissions_year_plot.png"), width = 10, height = 6, units = "in", dpi = 300, limitsize = FALSE)

### Unique patients ----
patient_year_big_type_trend <- data_tidy %>% 
  filter(dka == "Yes",
         type ==  "2") %>% 
  distinct(chi_number, .keep_all = TRUE) %>% 
  mutate(bar_type = "Type 2 Combined") %>% 
  group_by(year, bar_type) %>% 
  summarise(count = n())

ggplot(patient_year_trend, aes(x = year, y = count, group = bar_type, color = bar_type)) +
  geom_line(linewidth = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
  geom_point() +
  geom_line(data = patient_year_big_type_trend, linewidth = 0.8) +
  geom_smooth(data = patient_year_big_type_trend, method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.5) +
  geom_point(data = patient_year_big_type_trend) +
  stat_poly_eq(
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = "~~~")),
    formula = y ~ x, 
    parse = TRUE,
    geom = "text_npc",
    npcx = c(1, 1, 1),
    npcy = c(0.85, 0.5, 0.1)
  ) +
  stat_poly_eq(data = patient_year_big_type_trend,
    aes(label = paste(after_stat(rr.label), after_stat(p.value.label), sep = "~~~")),
    formula = y ~ x, 
    parse = TRUE,
    label.x = 0.8, label.y = 0.7
    
  ) +
  labs(title = "Unique patient DKA Admissions over time by diabetes type",
       x = "Year",
       y = "Number of Admissions",
       color = "Diabetes Type") +
  theme_classic()

ggsave(here("output/patients_year_plot.png"), width = 10, height = 6, units = "in", dpi = 300, limitsize = FALSE)


