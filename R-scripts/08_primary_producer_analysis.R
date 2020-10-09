


#### This script contains code used to answer the question: Do DHA, EPA and ALA concentrations in primary producers vary by ecosystem type?
#### last updated by JB October 8 2020, adapted from code written by Ryan Shipley (any mistakes are JB's fault!)

library(betareg)
library(tidyverse)
library(plotrix)
library(cowplot)
theme_set(theme_cowplot())


### The producer data here contain one observation per genus, per ecosystem, which is a genus-level average across studies, treatments etc.
producers <- read_csv("data-processed/fa-producers-finest-taxonomic-resolution-october.csv") %>% 
	filter(!is.na(mean_concentration))
View(producers)


# DHA ---------------------------------------------------------------------


### Are DHA concentrations different among ecosystem types?

dha <- producers %>%
	filter(fa == "dha") %>% 
	mutate(mean_concentration = mean_concentration / 100)


dha_n <- length(unique(dha$mean_concentration))

dha2 <- dha %>% 
	mutate(transformed_concentration = ((mean_concentration*(dha_n - 1)) + 0.5) / dha_n)


mod <- betareg(formula = transformed_concentration ~ ecosystem, data = dha2, link = "logit")
summary(mod)


dha2 %>% 
	group_by(ecosystem) %>% 
	summarise_each(funs(mean, std.error), mean_concentration) %>% 
	ggplot(aes(x = ecosystem, y = mean)) +
	geom_pointrange(aes(x = ecosystem, ymin = mean - std.error, ymax = mean + std.error))


# EPA ---------------------------------------------------------------------



### Are epa concentrations different among ecosystem types?

epa <- producers %>%
	filter(fa == "epa") %>% 
	mutate(mean_concentration = mean_concentration / 100)


epa_n <- length(unique(epa$mean_concentration))

epa2 <- epa %>% 
	mutate(transformed_concentration = ((mean_concentration*(epa_n - 1)) + 0.5) / epa_n)


mod <- betareg(formula = transformed_concentration ~ ecosystem, data = epa2, link = "logit")
summary(mod)


epa2 %>% 
	group_by(ecosystem) %>% 
	summarise_each(funs(mean, std.error), mean_concentration) %>% 
	ggplot(aes(x = ecosystem, y = mean)) +
	geom_pointrange(aes(x = ecosystem, ymin = mean - std.error, ymax = mean + std.error))



# ALA ---------------------------------------------------------------------



### Are ala concentrations different among ecosystem types?

ala <- producers %>%
	filter(fa == "ala") %>% 
	mutate(mean_concentration = mean_concentration / 100)

View(ala)

ala_n <- length(unique(ala$mean_concentration))

ala2 <- ala %>% 
	mutate(transformed_concentration = ((mean_concentration*(ala_n - 1)) + 0.5) / ala_n)


mod <- betareg(formula = transformed_concentration ~ ecosystem, data = ala2, link = "logit")
summary(mod)


ala2 %>% 
	group_by(ecosystem) %>% 
	summarise_each(funs(mean, std.error), mean_concentration) %>% 
	ggplot(aes(x = ecosystem, y = mean)) +
	geom_pointrange(aes(x = ecosystem, ymin = mean - std.error, ymax = mean + std.error))
