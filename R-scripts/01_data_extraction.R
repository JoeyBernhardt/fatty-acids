

### data extraction

library(tidyverse)
library(readxl)
library(janitor)
library(cowplot)

theme_set(theme_cowplot())





dataset1a_raw <- read_excel("data-raw/1a-Wang_et_al_data.xlsx") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	mutate(ecosystem = str_replace(ecosystem, "freswhater", "freshwater")) %>% 
	rename(group = trophic_position) %>% 
	select(ecosystem, group, epa, dha) %>% 
	mutate(source_dataset = "1a")

dataset1b_raw <- read_excel("data-raw/1b-Adtnl-Raw-Data-from-Wang-et-al-2016.xlsx") %>% 
	clean_names() %>%
	mutate(ecosystem = "freshwater") %>% 
	mutate(group = "consumer") %>% 
	select(ecosystem, group, epa, dha) %>% 
	mutate(source_dataset = "1b")


	dataset2_raw <- read_excel("data-raw/2-Columbo-et-al_Marine_and_Terrestrial_PUFA_data_set.xlsx") %>% 
	clean_names() %>% 
	mutate(group = ifelse(functional_group == "Phytoplankton", "primary_producer", "consumer")) %>%  
	mutate(ecosystem = str_to_lower(habitat)) %>% 
	select(ecosystem, group, epa, dha, genus, species) %>% 
	mutate(source_dataset = "2")


dataset4_raw <- read_excel("data-raw/4-DHA_percent_metanalysis.xlsx") %>% 
	clean_names() %>% 
	rename(trophic_position = id) %>% 
	rename(class = phyto_class) %>% 
	mutate(ecosystem = str_to_lower(ecosystem)) %>%
	rename(epa = percent_20_5n3_epa) %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	mutate(group = ifelse(trophic_position == "Algae", "primary_producer", "consumer")) %>% 
	select(ecosystem, group, epa, dha, taxa) %>% 
	mutate(source_dataset = "4") %>% 
	mutate(dha = str_replace(dha, "n/a", "NA")) %>% 
	mutate(dha = str_replace_na(dha, replacement = NA)) %>% 
	mutate(dha = as.numeric(dha))

dataset5_raw <- read_csv("data-raw/5-Oikos-Twining-2016-FA-Review-Dryad-Data.csv") %>% 
	clean_names() %>% 
	rename(epa = x20_5n3) %>% 
	rename(dha = x22_6n3) %>% 
	rename(ala = "x18_3n3") %>% 
	mutate(dha = str_replace(dha, "n/a", replacement = "NA")) %>% 
	mutate(dha = as.numeric(dha)) %>% 
	rename(ecosystem = source_ecosystem) %>% 
	rename(class = phyto_class) %>% 
	mutate(group = "primary_producer") %>% 
	mutate(ecosystem = str_to_lower(ecosystem)) %>% 
	mutate(source_dataset = "5")


dataset6_raw <- read_excel("data-raw/6-Strandberg_et_al_Lake_Ontario_fish_data.xlsx") %>% 
	clean_names() %>% 
	rename(epa = epa_9) %>% 
	rename(dha = dha_10) %>% 
	mutate(group = "consumer") %>% 
	mutate(ecosystem = "freshwater") %>%
	rename(species = x4) %>% 
	rename(genus = scientific_name) %>% 
	select(ecosystem, group, epa, dha, common_name, species, genus) %>% 
	mutate(source_dataset = "6") %>% 
	filter(epa != "%") %>% 
	mutate(epa = as.numeric(epa)) %>% 
	mutate(dha = as.numeric(dha))


dataset7_raw <- read_csv("data-raw/7-PLoS-One-Galloway-and-Winder-2015-S1_Dataset.csv") %>% 
	clean_names() %>% 
	filter(data == "prop") %>% 
	rename(epa = c20_5w3) %>% 
	rename(dha = c22_6w3) %>% 
	mutate(group = "primary_producer") %>% 
	mutate(ecosystem = "marine_or_freshwater") %>% 
	select(genus, species, epa, dha, group, ecosystem) %>% 
	mutate(source_dataset = "7")

dataset15_raw <- read_excel("data-raw/15-Popova-et-al.xlsx") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>%
	separate(dha, into = c("dha", "crap"), sep = 4, remove = FALSE) %>% 
	separate(epa, into = c("epa", "crap"), sep = 4, remove = FALSE) %>% 
	select(species, dha, epa, order, ecosystem, habitat) %>% 
	mutate(group = "consumer") %>% 
	mutate(source_dataset = "15") %>% 
	mutate(dha = as.numeric(dha)) %>% 
	mutate(epa = as.numeric(epa))
	
dataset16_raw <- read_excel("data-raw/16-Guil-Guerrero-J-Plants-terrestrial.xlsx") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>%
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "16")

dataset19_raw <- read_excel("data-raw/19-inverts-data-kristin.xlsx") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(taxa, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "19")

dataset22_raw <- read_excel("data-raw/22-Meyer_Abiotic_D2019.xlsx") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_level) %>% 
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "22")

dataset20_raw <- read_excel("data-raw/20-amphibian-Fritz-et-al-2019.xlsx") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "20")

dataset21_raw <- read_excel("data-raw/21-Makhutova-2017.xlsx") %>% 
	clean_names() %>%
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "21")

dataset17_raw <- read_excel("data-raw/17-NL-terrestrial-plant-compilation.xlsx") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(genus, species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "17")

dataset23_raw <- read_csv("data-raw/23-DGEBUADZE-et-al.-2017-Tadpole-Fatty-content.csv") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "23")

dataset24_raw <- read_csv("data-raw/24-Whiles-2010.csv") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "24")

dataset25_raw <- read_csv("data-raw/25-Cartland-Shaw-1998.csv") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "25")

dataset26_raw <- read_csv("data-raw/26-Zalewski-2007.csv") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "26")

dataset27_raw <- read_csv("data-raw/27-Kakela-Hyvarinen-1996.csv") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "27")

dataset28_raw <- read_csv("data-raw/28-Kakela-Hyvarinen-1996b.csv") %>% 
	clean_names() %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(group = trophic_position) %>% 
	select(species, dha, epa, ecosystem, group) %>% 
	mutate(source_dataset = "28") %>% 
	mutate(dha = ifelse(dha == "trace", 0, dha)) %>% 
	mutate(epa = ifelse(epa == "trace", 0, epa)) %>% 
	filter(!is.na(dha)) %>% 
	mutate(dha = as.numeric(dha)) %>% 
	mutate(epa = as.numeric(epa))


all_data <- bind_rows(dataset2_raw, dataset4_raw, dataset5_raw, dataset1a_raw, dataset1b_raw,
					  dataset6_raw, dataset7_raw, dataset15_raw,
					  dataset16_raw, dataset19_raw, dataset22_raw,
					  dataset20_raw, dataset17_raw, dataset21_raw, dataset23_raw,
					  dataset24_raw, dataset25_raw, dataset26_raw, dataset27_raw, dataset28_raw)


all_data %>% 
	filter(source_dataset == "4") %>% View

View(unique(all_data$source_dataset)) ### missing 3, 8, 9, 10, 11, 12, 13, 14, 18 

write_csv(all_data, "data-processed/all_data.csv")

all_data <- read_csv("data-processed/all_data.csv")

unique(all_data$source_dataset)

#### get higher order taxonomy

library(taxize)

all_data <- read_csv("data-processed/all_data.csv") %>% 
	mutate(group = ifelse(group == "primary producer", "primary_producer", group)) %>% 
	mutate(genus_species = paste(genus, species, sep = " ")) %>% 
	select(genus_species, everything())

taxa <- data.frame(taxa = paste(all_data$genus, all_data$species)) ## create dataframe of names to check

syns <- unique(taxa)
tsn_search <- get_tsn(as.character(syns$taxa), accepted = FALSE) ## find tsn for each unique taxa


all_data <- read_csv("data-processed/all_data.csv") %>% 
	mutate(group = ifelse(group == "primary producer", "primary_producer", group)) %>% 
	mutate(genus_species = paste(genus, species, sep = " ")) %>% 
	gather(key = nutrient, value = concentration, epa, dha) %>% 
	filter(!is.na(concentration)) %>% 
	mutate(genus_species = ifelse(source_dataset == 4, taxa, genus_species))

all_data %>% 
	filter(source_dataset == 4) %>% View
	filter(is.na(species)) %>% View

specieslist <- unique(all_data$genus_species)

hot <- classification(specieslist, db = 'itis')
hot[1]

str(hot)

hot %>% 
	map(c("name")) 

map_dfr(hot, extract, c("name", "rank"))

library(myTAI)
taxonomy(organism = "Nitzschia paleacea" , db = "ncbi", output = "classification" )

outputlst <- apply(specieslist, 1, function(x) taxonomy(organism = x , db = "ncbi", output = "classification"))

outputlst <- hot

# Parse out the taxonomy levels that you require
taxdata <- data.frame()
for(x in 1:length(outputlst)){
	tryCatch({
		kingdom=filter(outputlst[[x]], rank =="kingdom")$name
		phylum=filter(outputlst[[x]], rank =="phylum")$name
		class=filter(outputlst[[x]], rank =="class")$name
		order=filter(outputlst[[x]], rank =="order")$name
		family=filter(outputlst[[x]], rank =="family")$name
		genus=filter(outputlst[[x]], rank =="genus")$name
		species=filter(outputlst[[x]], rank =="species")$name
		
		row <- data.frame(cbind(kingdom = kingdom, phylum=phylum,class=class,order=order,family=family,genus=genus, species = species))
		taxdata <- bind_rows(taxdata, row)    
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}









all_data %>% 
	filter(!is.na(ecosystem)) %>% 
	ungroup() %>% 
	mutate(ecosystem = ifelse(ecosystem == "littoral", "freshwater", ecosystem)) %>% 
	filter(nutrient == "dha") %>% 
	filter(ecosystem == "marine_or_freshwater") %>% 
	ggplot(aes(x = concentration, fill = ecosystem)) + geom_histogram() +
	facet_grid(group ~ ecosystem, scales = "free_y") + ylab("Number of species") + xlab("DHA %")

ggsave("figures/all-dha-ecosystems.png", width = 12, height = 6)

	all_data %>% 
	
	filter(ecosystem == "freshwater", group == "consumer") %>% View



tsn_search


