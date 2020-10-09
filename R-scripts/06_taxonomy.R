

### check for duplicates, get higher order taxonomy, get original references
## take the mean, get the n 
## group by 
## where multiple tissues were reported for a single species, we chose the muscle tissue. where tissue was not reported 
## we assumed the study sampled
### send lily the unique id column




library(tidyverse)
library(taxize)
library(readxl)
library(janitor)
library(ggridges)


fa <- read_excel("data-processed/FA-Evol-Dataset.xlsx") %>% 
	clean_names() %>% 
	mutate(fa_id = rownames(.))


producers <- fa %>% 
	filter(trophic_position == "Producer") %>% 
	filter(!grepl("iphyton", species)) %>% 
	mutate(species = str_replace_all(species, "[^a-zA-Z]", "_")) %>%
	separate(species, into = c("genus", "species1"), remove = FALSE, sep = "_") %>% 
	filter(genus != "NA") %>% 
	mutate(genus = ifelse(fa_id == 3974, "Fontinalis", genus)) 

write_csv(producers, "data-processed/producers.csv")
unique(producers$genus)
specieslist <- unique(producers$genus)[1:100]

hot <- classification(specieslist, db = 'itis')
myr <- classification("Myriogramme", db = "itis")
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



# second chunk ------------------------------------------------------------

specieslist2 <- unique(producers$genus)[101:200]
specieslist2[grepl("Emeli", specieslist2)]

hot2 <- classification(specieslist2, db = 'itis')
outputlst <- hot2

# Parse out the taxonomy levels that you require
taxdata2 <- data.frame()
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
		taxdata2 <- bind_rows(taxdata2, row)    
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# third chunk ------------------------------------------------------------

specieslist3 <- unique(producers$genus)[201:300]
specieslist3[grepl("Emeli", specieslist3)]

hot3 <- classification(specieslist3, db = 'itis')
outputlst <- hot3

# Parse out the taxonomy levels that you require
taxdata3 <- data.frame()
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
		taxdata3 <- bind_rows(taxdata3, row)    
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# fourth chunk ------------------------------------------------------------
specieslist4 <- unique(producers$genus)[301:400]

hot4 <- classification(specieslist4, db = 'itis')
outputlst <- hot4

# Parse out the taxonomy levels that you require
taxdata4 <- data.frame()
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
		taxdata4 <- bind_rows(taxdata4, row)    
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



# fifth chunk ------------------------------------------------------------
specieslist5 <- unique(producers$genus)[401:500]

hot5 <- classification(specieslist5, db = 'itis')
outputlst <- hot5

# Parse out the taxonomy levels that you require
taxdata5 <- data.frame()
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
		taxdata5 <- bind_rows(taxdata5, row)    
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}





# sixth chunk ------------------------------------------------------------
specieslist6 <- unique(producers$genus)[501:length(unique(producers$genus))]

hot6 <- classification(specieslist6, db = 'itis')
outputlst <- hot6

# Parse out the taxonomy levels that you require
taxdata6 <- data.frame()
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
		taxdata6 <- bind_rows(taxdata6, row)    
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


all_tax <- bind_rows(taxdata, taxdata2, taxdata3, taxdata4, taxdata5, taxdata6) %>% 
	filter(!genus %in%  c("Odontella", "Halopteris"))


### ok now fixing the entries that returned as animals, clearly wrong

hot7 <- classification(c("Odontella aurita", "Halopteris scoparia", "Odontella weissflogii"), db = 'itis')

outputlst <- hot7

# Parse out the taxonomy levels that you require
taxdata7 <- data.frame()
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
		taxdata7 <- bind_rows(taxdata7, row)    
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

all_taxb <- bind_rows(all_tax, taxdata7)

all_tax2 <- left_join(producers, all_taxb, by = "genus") %>% 
	mutate(dha = as.numeric(dha)) %>% 
	filter(!is.na(dha)) 

unique(all_tax2$order.y)

tax_no_match <- all_tax2 %>% 
	filter(is.na(class.y))


### ok need to figure out what to do with tax_no_match
hot8 <- classification(c("Karenia"), db = 'itis')

library(cowplot)
all_tax2 %>% 
	# filter(class.y == "Alphaproteobacteria") %>% View
	ggplot(aes(x = dha, fill = ecosystem)) + geom_density() +
	facet_wrap(~ class.y, scales= "free_y")
ggsave("figures/producer-dha.png", width = 14, height = 10)

length(unique(all_tax2$class.y))

library(patchwork)
all3 <- all_tax2 %>% 
	mutate(epa = as.numeric(epa)) %>% 
	mutate(ala = as.numeric(ala)) %>% 
	group_by(ecosystem, class.y, genus) %>% 
	summarise_each(funs(mean), dha, epa, ala)

dha_plot <- all3 %>% 
	ggplot(aes(x = dha, fill = ecosystem)) + geom_density(color = "white") +
	scale_fill_viridis_d(begin = 0.4, end = 0.9) + ylab("Density") +
	xlab("DHA (% of FA)") +
	facet_wrap( ~ ecosystem, ncol = 1, nrow = 3) +
 theme(legend.position = "none")

ala_plot <- all3 %>% 
	ggplot(aes(x = ala, fill = ecosystem)) + geom_density(color = "white") +
	scale_fill_viridis_d(begin = 0.4, end = 0.9) + ylab("Density") +
	facet_wrap( ~ ecosystem, ncol = 1, nrow = 3) +
	xlab("ALA (% of FA)") + theme(legend.position = "none")



epa_plot <- all3 %>% 
	ggplot(aes(x = epa, fill = ecosystem)) + geom_density(color = "white") +
	scale_fill_viridis_d(begin = 0.4, end = 0.9) + ylab("Density") +
	facet_wrap( ~ ecosystem, ncol = 1, nrow = 3) +
	xlab("EPA (% of FA)") + theme(legend.position = "none")

com_plot <- ala_plot + epa_plot + dha_plot
ggsave(plot = com_plot, file = "figures/combined_pp_plot.png", width = 8, height = 6)




ala2 <- all3 %>% 
	ggplot(aes(x = ala, fill = ecosystem)) + geom_histogram(color = "white") +
	scale_fill_viridis_d(begin = 0.4, end = 0.9) + ylab("Number of genera") +
	facet_wrap( ~ ecosystem, ncol = 1, nrow = 3) +
	xlab("ALA (% of FA)") + theme(legend.position = "none")

epa2 <- all3 %>% 
	ggplot(aes(x = epa, fill = ecosystem)) + geom_histogram(color = "white") +
	scale_fill_viridis_d(begin = 0.4, end = 0.9) + ylab("Number of genera") +
	facet_wrap( ~ ecosystem, ncol = 1, nrow = 3) +
	xlab("EPA (% of FA)") + theme(legend.position = "none")

dha2 <- all3 %>% 
	ggplot(aes(x = dha, fill = ecosystem)) + geom_histogram(color = "white") +
	scale_fill_viridis_d(begin = 0.4, end = 0.9) + ylab("Number of genera") +
	facet_wrap( ~ ecosystem, ncol = 1, nrow = 3) +
	xlab("DHA (% of FA)") + theme(legend.position = "none")

com_plot2 <- ala2 + epa2 + dha2
ggsave(plot = com_plot2, file = "figures/combined_pp_histogram.png", width = 8, height = 6)


length(unique(all3$ala))



### ok so there are 59 species among the producers for which we don't have the scientific name
no_genus <- fa %>% 
	filter(trophic_position == "Producer") %>% 
	filter(!grepl("iphyton", species)) %>% 
	mutate(species = str_replace_all(species, "[^a-zA-Z]", "_")) %>%
	separate(species, into = c("genus", "species1"), remove = FALSE, sep = "_") %>% 
	filter(is.na(genus))


#### ok let's find the species that are in the producers dataset that are not in the all3 (taxized dataset)

unique(producers$genus)
unique(all3$genus)

 
	


#geom_density ridges

p1 <- 
	ggplot(prod, aes(x= ala, y=ecosystem, fill=ecosystem))+
	geom_density_ridges()+
	theme_ridges()+
	scale_fill_manual(values=custom.color)+
	xlim(0, 50)+
	theme(legend.position="none")



# trying with the gnr resolve ---------------------------------------------
library(tidyverse)
library(taxize)
specieslist <- unique(producers$genus)

result.itis <- specieslist %>%
	gnr_resolve(data_source_ids = c(3), 
				with_canonical_ranks=T)


itis_tax_output <- classification(result.itis$matched_name2, db = 'itis')
outputlst <- itis_tax_output

taxdata7 <- data.frame()
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
		taxdata7 <- bind_rows(taxdata7, row)    
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

taxdata7 %>% View

p2 <- producers %>% 
	select(fa_id, ecosystem, trophic_position, genus, species, dha, epa, ala) %>% 
	left_join(., result.itis, by = c("genus" = "user_supplied_name")) 

unmatched <- p2 %>% 
	filter(is.na(matched_name2))

producers %>% 
	filter(fa_id %in% unmatched$fa_id) %>% View ### ok these are mostly algae it seems

gnr_datasources() %>% View
unmatched_species <- unique(unmatched$genus)

result.ncbi <- unmatched_species %>%
	gnr_resolve(data_source_ids = c(9), 
				with_canonical_ranks=T)

p3 <- p2 %>% 
	filter(fa_id %in% unmatched$fa_id) %>% 
	select(-submitted_name, -data_source_title, -score, - matched_name2)

p4 <- p3 %>% 
	left_join(., result.worms, by = c("genus" = "user_supplied_name"))

still_unmatched <- p4 %>% 
	filter(is.na(matched_name2))

write_csv(still_unmatched, "data-processed/unmatched-genera.csv")

still_unmatched_species <- unique(still_unmatched$genus)

result.col <- still_unmatched_species %>% 
	gnr_resolve(data_source_ids = c(1), 
				with_canonical_ranks=T)

p5 <- still_unmatched %>% 
	select(-submitted_name, -data_source_title, -score, - matched_name2) %>% 
	left_join(., result.col,  by = c("genus" = "user_supplied_name")) %>% 
	filter(!is.na(submitted_name))

actually_unmatched <- still_unmatched %>% 
	select(-submitted_name, -data_source_title, -score, - matched_name2) %>% 
	left_join(., result.col,  by = c("genus" = "user_supplied_name")) %>% 
	filter(is.na(submitted_name))

write_csv(actually_unmatched, "data-processed/actually-unmatched-genera.csv")


### ok now let's gather them all 

edited_unmatched <- read_csv("data-processed/actually-unmatched-genera-edited.csv")
g2 <- unique(edited_unmatched$googled_genus)

result.col2 <- g2 %>% 
	gnr_resolve(data_source_ids = c(1), 
				with_canonical_ranks=T)

edited2 <- edited_unmatched %>% 
	select(-submitted_name, -data_source_title, -score, - matched_name2) %>% 
	left_join(result.col2, by = c("googled_genus" = "user_supplied_name"))


### ok now let's gather them all, gather the genus and fa_id column, then merge back with the producers data

e2 <- edited2 %>% 
	select(fa_id, matched_name2) %>% 
	filter(!is.na(matched_name2)) %>% 
	distinct() %>% 
	mutate(fa_id = as.character(fa_id))

p4b <- p4 %>% 
	select(fa_id, matched_name2) %>% 
	filter(!is.na(matched_name2)) %>% 
	distinct() %>% 
	mutate(fa_id = as.character(fa_id))

p2b <- p2 %>% 
	select(fa_id, matched_name2) %>% 
	filter(!is.na(matched_name2)) %>% 
	distinct() %>% 
	mutate(fa_id = as.character(fa_id))

all_genera <- bind_rows(e2, p4b, p2b) %>% 
	distinct()


producers2 <- producers %>% 
	left_join(all_genera)

producers3 <- producers2 %>% 
	mutate(genus_matched = ifelse(is.na(matched_name2), genus, matched_name2))

prod_tax <- taxdata7 %>% 
	filter(kingdom != "Animalia")

producers4 <- producers3 %>% 
	left_join(., prod_tax, by = "genus") %>% 
	rename(class = class.y,
		   order = order.y,
		   family = family.y,
		   phylum = phylum.y) %>% 
	select(-contains(".x"))
write_csv(producers4, "data-processed/producers-genus-cleaned.csv")

producers4_cleaned <- read_excel("data-processed/producers-genus-cleaned-edited.xlsx")
producers4_cleaned <- read_csv("data-processed/producers-genus-cleaned-edited.csv") %>% 
	select(-original_study, -data_from)

# update sep 30 with cleaned up data from lily ----------------------------

new_prod <- read_excel("data-processed/producers-genus-cleaned_edrefs.xlsx") %>% 
	select(fa_id, original_study, data_from)

prod4_c2 <- producers4_cleaned %>% 
	filter(fa_id %in% c(new_prod$fa_id)) %>%
	left_join(., new_prod) %>% 
	distinct()


prod5 <- prod4_c2 %>% 
	# producers4_cleaned %>% 
	filter(identified_to_genus != "no") %>%
	# mutate(matched_name2 = ifelse(is.na(matched_name2), hand_edited_genus, matched_name2)) %>%
	# filter(matched_name2 != genus_matched) %>%
	gather(key = fa, value = concentration, epa, dha, ala) %>% 
	filter(!is.na(concentration)) %>% 
	mutate(concentration = as.numeric(concentration)) %>%
	group_by(kingdom, order, family, class, genus_matched, fa, ecosystem) %>% 
	summarise_each(funs(mean), concentration) %>% 
	filter(!is.na(genus_matched))

library(ggplot2)
library(ggridges)

length(unique(prod5$genus_matched))

write_csv(prod5, "data-processed/prod5-update-sep2020.csv")


## Diploschistes is a lichen, Peltigera, Anaptychia, Umbilicaria
## Ctenidium is a moss, Fontinalis, Dichodontium, Tortella, Pogonatum


prod6 <- prod5 %>% 
	rename(mean_concentration = concentration) %>% 
	mutate(moss = ifelse(genus_matched %in% c("Ctenidium", "Fontinalis", "Dichodontium", "Tortella", "Pogonatum"), "yes", "no")) %>% 
	mutate(lichen = ifelse(genus_matched %in% c("Diploschistes", "Peltigera", "Anaptychia", "Umbilicaria"), "yes", "no")) 



mosses <- prod6 %>% 
	filter(moss == "yes") %>% 
	filter(fa != "ala")
	

lichens <- prod6 %>% 
	filter(lichen == "yes") %>% 
	filter(fa != "ala")
	
	prd_plot <- prod6 %>% 
		mutate(fa = factor(fa, levels = c("ala", "epa", "dha"))) %>% 
	ggplot(aes(x= mean_concentration, y=ecosystem, fill=ecosystem))+
		geom_density_ridges(color = "white") +
	theme_ridges() +
	# geom_point(aes(x = concentration, y = 3.1), data = mosses, shape = 6, size = 2, color = "coral") +
	# 	geom_point(aes(x = concentration, y = 3.1), data = lichens, shape = 6, size = 2, color =  "coral") +
	xlim(0, 75)+
	scale_fill_manual(values = c( "aquamarine3", "cornflowerblue", "darkolivegreen4")) +
	theme(legend.position="none") + facet_wrap( ~ fa) +
		theme(strip.text.x = element_text(size=0)) + 
		ylab("") + xlab("")
ggsave("figures/producers-genus.pdf", width = 6, height = 3)
write_csv(prod6, "data-processed/fa-producers-finest-taxonomic-resolution.csv")

consumers <- read_excel("data-processed/FA-Evol-Dataset_ConsumerEdit3.xlsx", sheet = "Tiss Avg REd")

con2 <- consumers %>% 
	gather(key = fa, value = concentration, ala, epa, dha) %>% 
	mutate(concentration = as.numeric(concentration)) %>%
	filter(!is.na(concentration)) %>% 
	mutate(Class = as.character(Class)) %>% 
	mutate(Class = factor(Class, levels = c("Polychaeta", "Clitellata", "Cephalopoda", "Gastropoda",
											"Bivalvia", "Malacostraca", "Hexanauplia", "Branchiopoda", "Insecta", "Echinoidea",
											"Holothuroidea", "Ophiurodea", "Asteroidea", "Chondrichthyes", "Actinopterygii",
											"Amphibia", "Mammalia", "Aves"))) %>%
	mutate(Class = fct_rev(Class)) %>% 
	filter(trophic_position != "Producer") %>% 
	filter(!is.na(Class))

View(con2)

library(taxize)
con3 <- con2 %>%
	separate(col = species, into = c("genus", "species1"), remove = FALSE, sep = " ")

#### find consumer names
gnr_datasources() %>% View
result.cons <- unique(con3$genus) %>%
	gnr_resolve(data_source_ids = c(1), 
				with_canonical_ranks=T)

result.cons11 <- unique(con3$genus) %>%
	gnr_resolve(data_source_ids = c(11), 
				with_canonical_ranks=T)

con4 <- con3 %>%
	left_join(result.cons11, by = c("genus" = "user_supplied_name"))


	
	con5 <- con4 %>% 
	mutate(finest_taxon = ifelse(is.na(matched_name2), species, matched_name2)) %>% 
	mutate(finest_taxon = ifelse(is.na(finest_taxon), Family, finest_taxon)) %>%
	select(Phylum, Class, Order, Family, genus, finest_taxon, everything()) %>% 
	mutate(Class_car = as.character(Class)) %>% 
	mutate(finest_taxon = ifelse(is.na(finest_taxon), Order, finest_taxon)) %>% 
	mutate(finest_taxon = ifelse(is.na(finest_taxon), Class_car, finest_taxon)) %>% 
	select(finest_taxon, matched_name2, species, everything()) %>% 
	mutate(finest_taxon = ifelse(genus == "Grazers", Order, finest_taxon)) %>% 
	mutate(genus = ifelse(genus == "Grazers", NA, genus)) %>% 
	group_by(Phylum, Class, Order, Family, genus, ecosystem, finest_taxon, fa) %>% 
	summarise(mean_concentration = mean(concentration)) %>% 
	mutate(Kingdom = "Animalia") %>% 
	select(Kingdom, Phylum, Class, Order, Family, genus, everything()) %>% 
		ungroup() %>% 
		rename(genus_or_finest_taxon = genus)
	
	con5a <- con4 %>% 
		mutate(finest_taxon = ifelse(is.na(matched_name2), species, matched_name2)) %>% 
		mutate(finest_taxon = ifelse(is.na(finest_taxon), Family, finest_taxon)) %>%
		select(Phylum, Class, Order, Family, genus, finest_taxon, everything()) %>% 
		mutate(Class_car = as.character(Class)) %>% 
		mutate(finest_taxon = ifelse(is.na(finest_taxon), Order, finest_taxon)) %>% 
		mutate(finest_taxon = ifelse(is.na(finest_taxon), Class_car, finest_taxon)) %>% 
		select(finest_taxon, matched_name2, species, everything()) %>% 
		mutate(finest_taxon = ifelse(genus == "Grazers", Order, finest_taxon)) %>% 
		mutate(genus = ifelse(genus == "Grazers", NA, genus)) 
	
	
	con5b <- con4 %>% 
		mutate(finest_taxon = ifelse(is.na(matched_name2), species, matched_name2)) %>% 
		mutate(finest_taxon = ifelse(is.na(finest_taxon), Family, finest_taxon)) %>%
		select(Phylum, Class, Order, Family, genus, finest_taxon, everything()) %>% 
		mutate(Class_car = as.character(Class)) %>% 
		mutate(finest_taxon = ifelse(is.na(finest_taxon), Order, finest_taxon)) %>% 
		mutate(finest_taxon = ifelse(is.na(finest_taxon), Class_car, finest_taxon)) %>% 
		select(finest_taxon, matched_name2, species, everything()) %>% 
		mutate(finest_taxon = ifelse(genus == "Grazers", Order, finest_taxon)) %>% 
		mutate(genus = ifelse(genus == "Grazers", NA, genus)) %>% 
		group_by(finest_taxon, fa) %>% 
		summarise(mean_concentration = mean(concentration))
	
	con5c <- left_join(con5b, con5a) %>% 
		mutate(Kingdom = "Animalia") %>% 
		ungroup() %>% 
		rename(genus_or_finest_taxon = genus) %>% 
		mutate(Class = as.character(Class)) %>% 
		select(Kingdom, Phylum, Class, Order, Family, finest_taxon, genus_or_finest_taxon, fa, concentration, ecosystem) %>% 
		mutate(finest_taxon = ifelse(is.na(finest_taxon), Family, finest_taxon)) %>%
		mutate(finest_taxon = ifelse(is.na(finest_taxon), Order, finest_taxon)) %>% 
		mutate(finest_taxon = ifelse(is.na(finest_taxon), Class, finest_taxon)) %>% 
		mutate(finest_taxon = ifelse(finest_taxon == "Oligocheata", "Oligochaeta", finest_taxon)) %>% 
		mutate(genus_or_finest_taxon = ifelse(genus_or_finest_taxon == "Oligocheata", "Oligochaeta", genus_or_finest_taxon)) 
		
		
	
	write_csv(con5c, "data-processed/fa-consumers-finest-taxonomic-resolution.csv")



View(con5)

con_plot <- con5 %>% 
	filter(Class != "Echinoidea") %>% 
	mutate(fa = factor(fa, levels = c("ala", "epa", "dha"))) %>% 
	ggplot(aes(x = mean_concentration, y = Class, fill = ecosystem)) +
	geom_density_ridges(color = "white") +
	theme_ridges() +
		scale_fill_manual(values = c( "aquamarine3", "cornflowerblue", "darkolivegreen4")) +
		theme(legend.position="none") + facet_wrap( ~ fa) +
		theme(strip.text.x = element_text(size=0)) + 
	theme(text = element_text(size=20)) +
		ylab("") + xlab("") +
	xlim(0, 75)
ggsave("figures/consumers-genus.pdf", width = 6, height = 3)

library(patchwork)

p <- prd_plot / con_plot +
	plot_layout(heights = c(1, 5)) 
ggsave('figures/figure2-cons-genus-sep2020.pdf', p, width = 10, height = 12)



# filling in missing higher order taxonomy for producers ------------------

prod7 <- read_csv("data-processed/fa-producers-finest-taxonomic-resolution.csv")


### argania, aizoon, Candidatus
no_king <- prod7 %>% 
	filter(is.na(kingdom))


itis_tax_output2 <- classification(no_king$genus_matched, db = 'itis')
outputlst <- itis_tax_output2

taxdata7 <- data.frame()
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
		taxdata7 <- bind_rows(taxdata7, row)    
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}




king_new <- no_king %>% 
	select(-kingdom, -order, -class, -family) %>% 
	left_join(., taxdata7, by = c("genus_matched" = "genus"))  %>% 
	distinct() %>% 
	filter(kingdom != "Animalia")

king_animals <- no_king %>% 
	select(-kingdom, -order, -class, -family) %>% 
	left_join(., taxdata7, by = c("genus_matched" = "genus"))  %>% 
	distinct() %>% 
	filter(kingdom == "Animalia")

king_animals2 <- king_animals %>% 
	mutate(genus_matched = ifelse(genus_matched == "Pteridophora", "Pterygophora", genus_matched)) %>% 
	mutate(genus_matched = ifelse(genus_matched == "Phaenomonas", "Phaeomonas", genus_matched)) 

write_csv(king_animals2, "data-processed/producer-kingdom-messed-up.csv")

king_animals_corr <- read_csv("data-processed/producer-kingdom-messed-up-edited.csv")

king_new2 <- bind_rows(king_new, king_animals_corr)

king <- prod7 %>% 
	filter(!is.na(kingdom)) %>% 
	bind_rows(king_new2) %>% 
	select(-phylum, -species) %>% 
	rename(genus = genus_matched) %>% 
	select(kingdom, class, order, family, genus, everything())

write_csv(king, "data-processed/fa-producers-finest-taxonomic-resolution-october.csv")


prd_plot <- king %>% 
	mutate(fa = factor(fa, levels = c("ala", "epa", "dha"))) %>% 
	ggplot(aes(x= mean_concentration, y=ecosystem, fill=ecosystem))+
	geom_density_ridges(color = "white") +
	theme_ridges() +
	xlim(0, 75)+
	scale_fill_manual(values = c( "aquamarine3", "cornflowerblue", "darkolivegreen4")) +
	theme(legend.position="none") + facet_wrap( ~ fa) +
	theme(strip.text.x = element_text(size=0)) + 
	ylab("") + xlab("")

con_plot <- con5 %>% 
	filter(Class != "Echinoidea") %>% 
	mutate(fa = factor(fa, levels = c("ala", "epa", "dha"))) %>% 
	ggplot(aes(x = mean_concentration, y = Class, fill = ecosystem)) +
	geom_density_ridges(color = "white") +
	theme_ridges() +
	scale_fill_manual(values = c( "aquamarine3", "cornflowerblue", "darkolivegreen4")) +
	theme(legend.position="none") + facet_wrap( ~ fa) +
	theme(strip.text.x = element_text(size=0)) + 
	theme(text = element_text(size=20)) +
	ylab("") + xlab("") +
	xlim(0, 75)

library(patchwork)

p <- prd_plot / con_plot +
	plot_layout(heights = c(1, 5)) 
ggsave('figures/figure2-cons-genus-oct2020.pdf', p, width = 10, height = 12)
