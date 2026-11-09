# This file creates a table of SGLT2 vs non-SGLT2 patients across DM types
# It needs 01-tidy to be run first

# If 01-tidy not already run, run this
library(here)
source(here("scripts/01-tidy.R"))

rx_2020 <- read_csv(here("data/pitc202012.csv"))
hb_2024 <- read_csv(here("data/hb_pitc2024_07_12-1.csv"))

disp_2020 <- read_csv(here("data/disp_pitc202012_updated.csv"))
hb_lookup <- read_csv(here("data/hb_lookup.csv"))

hb_lookup <- hb_lookup %>% 
  filter(str_starts(HBDateEnacted, "2018"))

fife_hb <- hb_lookup %>% 
  filter(HBName == "NHS Fife") %>% 
  pull(HB)

distinct(rx_2020, HBT)



glif_2020 <- rx_2020 %>% 
  filter(HBT == fife_hb,
    str_detect(BNFItemDescription, "GLIF")) 


glif_disp_2020 <- disp_2020 %>% 
  filter(HBT == fife_hb,
         str_detect(BNFItemDescription, "GLIF")) 

hb_disp_2024 <- hb_2024 %>% 
  filter(
         str_detect(BNFItemDescription, "GLIF"),
         PaidDateMonth == "202412") 
