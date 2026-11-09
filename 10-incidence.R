library(here)
source(here("01-tidy.R"))

library(gtsummary)
library(ggpmisc)
library(gt)


glif_patients <- read_xlsx(here("data/gliflozins_patients.xlsx"))


phs_2019 <- read_csv(here("data/PHS Prescribing/dec-2019.csv"))
phs_2020 <- read_csv(here("data/PHS Prescribing/dec-2020.csv"))
phs_2021 <- read_csv(here("data/PHS Prescribing/dec-2021.csv"))
phs_2022 <- read_csv(here("data/PHS Prescribing/dec-2022.csv"))
phs_2023 <- read_csv(here("data/PHS Prescribing/dec-2023.csv"))
phs_2024 <- read_csv(here("data/PHS Prescribing/dec-2024.csv"))



# Make mergable
phs_2019 <- phs_2019 %>% 
  clean_names() %>% 
  rename(hbt = hbt2014)

# Clean all datasets
prescribing_list <- list(df1 = phs_2019, df2 = phs_2020, df3 = phs_2021, df4 = phs_2022,
                         df5 = phs_2023, df6 = phs_2024)

clean_dfs <- function(clean) {
  target_drugs <- c("DAPAGLIFLOZIN", "FORXIGA", "XIGDUO", "INVOKANA", "EMPAGLIFLOZIN",
                    "JARDIANCE", "SYNJARDY", "STEGLATRO", "GLYXAMBI")
  clean %>% 
    clean_names() %>% 
    filter(hbt == "S08000029",
           str_detect(bnf_item_description, str_c(target_drugs, collapse = "|"))) %>% 
    select(paid_date_month, bnf_item_description, number_of_paid_items)
}

# merge all the open data into a single frame
processed_combined <- prescribing_list %>% 
  imap_dfr(~ clean_dfs(.x) %>% mutate(source = .y))

# summarise the prescriptions and change month into year
year_summary <- processed_combined %>% 
  group_by(paid_date_month) %>% 
  summarise(prescriptions = sum(number_of_paid_items)) %>% 
  mutate(year = floor(paid_date_month/ 100))

# Add the open data to the imported dataset
glif_patients <- glif_patients %>% 
  left_join(year_summary, by = c("year" = "year"))




final_summary <- all_data %>%
  group_by(date) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")


all_dka <- data_tidy %>% 
  filter(dka == "Yes") %>% 
  group_by(year) %>% 
  summarise(all_dka_count = n()) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year))

sglt2_dka <- data_tidy %>% 
  filter(sglt2 == "Yes") %>% 
  group_by(year) %>% 
  summarise(sglt2_dka_count = n()) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year))

glif_patients <- glif_patients %>% 
  mutate(year = as.numeric(year))

sglt2_incidence <- sglt2_dka %>% 
  left_join(glif_patients, by = c("year" = "year")) %>% 
  left_join(all_dka, by = c("year" = "year")) %>% 
  mutate(incidence = sglt2_dka_count / patients * 1000,
         corr_incidence = sglt2_dka_count / prescriptions * 1000,
         pop_incidence = all_dka_count/population * 10000)

ggplot(sglt2_incidence, aes(x = year, group = 1)) +
  geom_line(aes(y = corr_incidence, colour = "SGLT2i prescriptions - per 1000 at risk")) +
  geom_point(aes(y = corr_incidence, colour = "SGLT2i prescriptions - per 1000 at risk")) +
  geom_line(aes(y = pop_incidence, colour = "Fife diabetes population -  per 10000 at risk")) +
  geom_point(aes(y = pop_incidence, colour = "Fife diabetes population -  per 10000 at risk")) +
  geom_smooth(
    data = sglt2_incidence,
    aes(x = year, y = corr_incidence, colour = "SGLT2i prescriptions - per 1000 at risk"),
    method = "lm",
    se = FALSE,
    linetype = "dashed",
    size = 0.5
  ) +
  geom_smooth(
    data = sglt2_incidence,
    aes(x = year, y = pop_incidence, colour = "Fife diabetes population -  per 10000 at risk"),
    method = "lm",
    se = FALSE,
    linetype = "dashed",
    size = 0.5
  ) +
  stat_poly_eq(
    aes(x = year, y = corr_incidence, color = "SGLT2i prescriptions - per 1000 at risk", label = paste(after_stat(rr.label), after_stat(p.value.label), sep = "~~~")),
    formula = y ~ x, 
    parse = TRUE,
    geom = "text_npc",
   npcx = c(1),
    npcy = c(0.15)
  ) +
  stat_poly_eq(
    aes(x = year, y = pop_incidence, color = "Fife diabetes population -  per 10000 at risk", label = paste(after_stat(rr.label), after_stat(p.value.label), sep = "~~~")),
    formula = y ~ x, 
    parse = TRUE,
    geom = "text_npc",
    npcx = c(1),
    npcy = c(0.85)
  ) +
  labs(title = "Incidence of DKA admissions",
       x = "Year",
       y = "Incidence rate",
       color = "Population") +
  theme_classic()

ggsave(here("output/incidence.png"), width = 10, height = 6, units = "in", dpi = 300, limitsize = FALSE)


patient_years <- sglt2_incidence %>% 
  summarise(total_patients = sum(sglt2_dka_count),
         pat_years = sum(prescriptions),
         per_pat_year = total_patients / pat_years * 1000)

## Approx dka incidence by type ----
t1_dka <- data_tidy %>% 
  filter(dka == "Yes",
         type == "1") %>% 
  group_by(year) %>% 
  summarise(t1_dka_count = n()) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year))

t2_dka <- data_tidy %>% 
  filter(dka == "Yes",
         type == "2") %>% 
  group_by(year) %>% 
  summarise(t2_dka_count = n()) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year))

type_dka <- sglt2_incidence %>% 
  left_join(t1_dka, by = c("year" = "year")) %>%
  left_join(t2_dka, by = c("year" = "year")) %>% 
  mutate(t1_pop = 0.103 * population,
         t2_pop = 0.88 * population,
         t1_dka_incidence = t1_dka_count / t1_pop * 1000,
         t2_dka_incidence = t2_dka_count / t2_pop * 1000)

