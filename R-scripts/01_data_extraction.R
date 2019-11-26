

### data extraction

library(tidyverse)
library(readxl)
library(janitor)


dataset1_raw <- read_excel("data-raw/4-DHA_percent_metanalysis.xlsx") %>% 
	clean_names() %>% 
	rename(trophic_position = id) %>% 
	rename(class = phyto_class) %>% 
	mutate(ecosystem = str_to_lower(ecosystem)) %>%
	rename(epa = percent_20_5n3_epa) %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	mutate(group = ifelse(trophic_position == "Algae", "primary_producer", "consumer")) %>% 
	select(ecosystem, group, epa, dha) %>% 
	mutate(source_dataset = 1)

dataset2_raw <- read_excel("data-raw/2-Columbo-et-al_Marine_and_Terrestrial_PUFA_data_set.xlsx") %>% 
	clean_names() %>% 
	mutate(group = ifelse(functional_group == "Phytoplankton", "primary_producer", "consumer")) %>% 
	mutate(ecosystem = str_to_lower(habitat)) %>% 
	select(ecosystem, group, epa, dha) %>% 
	mutate(source_dataset = 2)
	









