

library(tidyverse)
library(janitor)
library(cowplot)

theme_set(theme_cowplot())


bird <- read_csv("data-raw/Bird-Diet-Data.csv") %>% 
	clean_names() %>% 
	mutate(habitat = ifelse(epa_perc > 10, "aquatic", "terrestrial")) %>% 
	filter(!is.na(habitat)) %>% 
	mutate(group = 1)


bird %>% 
	ggplot(aes(x = reorder(taxa, env_count_mean), y = env_count_mean)) + geom_bar(stat = "identity")
ggsave("figures/bird-count-abundance.png", width = 10, height = 6)

bird %>% 
	ggplot(aes(x = reorder(taxa, -env_biomass_mean), y = env_biomass_mean, fill = habitat)) + geom_bar(stat = "identity") +
	geom_point(aes(x = reorder(taxa, -env_biomass_mean), y = epa_perc, color = habitat), data = bird, size = 4) +
	geom_point(aes(x = reorder(taxa, -env_biomass_mean), y = epa_perc), data = bird, size = 4, color = "white", shape = 1) +
	geom_line(aes(x = reorder(taxa, -env_biomass_mean), y = epa_perc, group = group), 
			  data = bird, size = 1, color = "darkgrey", linetype = "dashed") +
	ylab("Diet biomass in the envt (bars) \n EPA percent (points)") +
	xlab("") +
	theme(axis.text.x = element_text(angle = 90)) +
	scale_color_manual(values = c("blue4", "seagreen4")) +
	scale_fill_manual(values = c("blue4", "seagreen4"))
ggsave("figures/bird-count-biomass-epa.png", width = 6, height = 5)


bird %>% 
	ggplot(aes(x = reorder(taxa, -env_biomass_mean), y = diet_biomass_perc, fill = habitat)) + geom_bar(stat = "identity") +
	# geom_point(aes(x = reorder(taxa, -env_biomass_mean), y = epa_perc, color = habitat), data = bird, size = 3) +
	# geom_line(aes(x = reorder(taxa, -env_biomass_mean), y = epa_perc, group = group), 
	# 		  data = bird, size = 1, color = "darkgrey", linetype = "dashed") +
	ylab("Percentage of diet") +
	xlab("") +
	theme(axis.title.x=element_blank(),
		  axis.text.x=element_blank(),
		  axis.ticks.x=element_blank()) +
	scale_fill_manual(values = c("blue4", "seagreen4"))
ggsave("figures/bird-percent-in-diet.png", width = 6, height = 3)



plot_grid(plota, plotb, labels = c('A', 'B'), nrow = 2, ncol = 1, rel_heights = c(0.5, 1))	

### count vs. abundance

bird %>% 
	ggplot(aes(x = env_count_mean, y = env_biomass_mean)) + geom_point() 
ggsave("figures/bird-count-vs-biomass.png", width = 10, height = 6)


