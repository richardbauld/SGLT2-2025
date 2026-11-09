library(tidyverse)
library(here)
library(janitor)
library(readxl)



data_raw <- read_xlsx(here("data/merged-units-2019-2024.xlsx"),
                      col_types = c("numeric", "text", "date", 
                                    "text", "numeric", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "text", "text", "text", 
                                    "numeric", "text", "numeric", "text", 
                                    "text", "text", "text"))

data_tidy <- data_raw %>% 
  clean_names() %>% 
  filter(unit != "ED") %>% 
  mutate(date = date_admitted_to_this_unit) %>% 
  separate(date_admitted_to_this_unit, into = c("year", NA, NA), sep = "-") %>% 
  mutate(hospital_outcome = recode(hospital_outcome, "1. Lived" = "Discharged", "2. Died" = "Died", "Alive" = "Discharged"),
         unit_outcome = recode(unit_outcome, "1. Improved" = "Discharged", "4. Died" = "Died", "3. Worse" = "Discharged", "2. No change" = "Discharged",
                               "Improved" = "Discharged", "No Change" = "Discharged", "Worse" = "Discharged", "1. No change" = "Discharged"),
         sex = recode(sex, "F" = "Female", "M" = "Male")) %>% 
  select(-key, -apache_iii_reason_for_admission, -searchable_chi)

data_tidy %>% count()

total_dka <- data_tidy %>% 
  filter(dka == "Yes") %>% 
  summarise(n = n())

unique_patients <- data_tidy %>% 
  filter(dka == "Yes") %>% 
  distinct(chi_number) %>% 
  summarise(n = n())

distinct_type <- data_tidy %>% 
  filter(dka == "Yes") %>% 
  distinct(chi_number, .keep_all = TRUE) %>% 
  group_by(type) %>% 
  summarise(n = n())

type1_sglt2 <- data_tidy %>% 
  filter(dka == "Yes",
         type == "1",
         sglt2 == "Yes") %>%
  count()

# Clean environment ----

# Get the name of the data frame you want to keep
my_data_frame_name <- c("data_tidy")

# Remove all objects except the data frame you want to keep
rm(list = setdiff(ls(), my_data_frame_name))


readmission_check <- data_tidy %>% 
  filter(chi_number == "2804798747")
