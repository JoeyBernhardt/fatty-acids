

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
