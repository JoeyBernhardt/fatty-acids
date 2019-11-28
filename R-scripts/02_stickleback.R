
library(tidyverse)
library(janitor)

stuart <- read_csv("data-raw/Stewart-et-al-03-dietCntnt.csv") %>% 
	clean_names()


st <- stuart %>% 
	gather(key = species, value = amount, 2:58) %>% 
	filter(!is.na(amount))

st %>% 
	filter(!species %in% c("trichoptera_1", "trichoptera")) %>% 
	# filter(fish_id_univ == "TheL13.026") %>%View
	group_by(fish_id_univ) %>%
	mutate(sum = sum(amount)) %>% View
	summarise(sum = sum(amount)) %>% View
	filter(sum == 100) %>% View
	ggplot(aes(x = species, y = amount)) + geom_point() 
	
	unique(st$fish_id_univ)

st %>% 
	filter(amount > 100) %>% View


st %>% 
	filter(!species %in% c("trichoptera_1", "trichoptera")) %>%  
	separate(fish_id_univ, into = c("Site", "Habitat"), sep = 3, remove = FALSE) %>% 
	separate(Habitat, into = c("Habitat", "individual"), sep = 1) %>% 
	filter(Habitat != "M") %>% 
	distinct(fish_id_univ) %>% 
	tally()
	mutate(habitat = ifelse(grepl("L", fish_id_univ), "Lake", "Stream")) %>% 
	group_by(habitat, species) %>% 
	filter(habitat == "Lake") %>%
	filter(grepl("KenL", fish_id_univ)) %>% 
	summarise(mean = mean(amount)) %>% 
	ggplot(aes(x = reorder(species, mean), y = mean, fill = habitat)) + geom_histogram(stat= "identity") +
	theme(axis.text.x = element_text(angle = 90)) +
	facet_wrap( ~ fish_id_univ)


ggsave("figures/diet-items-stream.png", width = 15, height = 8)
