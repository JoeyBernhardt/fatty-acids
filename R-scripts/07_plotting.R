

#### This script is used to generate Figure 2 

library(tidyverse)
library(ggridges)
library(patchwork)

producers <- read_csv("data-processed/fa-producers-finest-taxonomic-resolution-october.csv")
consumers <- read_csv("data-processed/fa-consumers-finest-taxonomic-resolution.csv") %>% 
	mutate(Class = factor(Class, levels = c("Polychaeta", "Clitellata", "Cephalopoda", "Gastropoda",
											"Bivalvia", "Malacostraca", "Hexanauplia", "Branchiopoda", "Insecta", "Echinoidea",
											"Holothuroidea", "Ophiurodea", "Asteroidea", "Chondrichthyes", "Actinopterygii",
											"Amphibia", "Mammalia", "Aves"))) %>%
	mutate(Class = fct_rev(Class)) 

con_plot <- consumers %>% 
	filter(Class != "Echinoidea") %>% 
	mutate(fa = factor(fa, levels = c("ala", "epa", "dha"))) %>% 
	ggplot(aes(x = concentration, y = Class, fill = ecosystem)) +
	geom_density_ridges(color = "white") +
	theme_ridges() +
	scale_fill_manual(values = c( "aquamarine3", "cornflowerblue", "darkolivegreen4")) +
	theme(legend.position="none") + facet_wrap( ~ fa) +
	theme(strip.text.x = element_text(size=0)) + 
	theme(text = element_text(size=20)) +
	ylab("") + xlab("") +
	xlim(0, 75)


prd_plot <- producers %>% 
	mutate(fa = factor(fa, levels = c("ala", "epa", "dha"))) %>% 
	ggplot(aes(x= mean_concentration, y=ecosystem, fill=ecosystem))+
	geom_density_ridges(color = "white") +
	theme_ridges() +
	xlim(0, 75)+
	scale_fill_manual(values = c( "aquamarine3", "cornflowerblue", "darkolivegreen4")) +
	theme(legend.position="none") + facet_wrap( ~ fa) +
	theme(strip.text.x = element_text(size=0)) + 
	ylab("") + xlab("")


p <- prd_plot / con_plot +
	plot_layout(heights = c(1, 5)) 
ggsave('figures/figure2-cons-genus-sep2020.pdf', p, width = 10, height = 12)
