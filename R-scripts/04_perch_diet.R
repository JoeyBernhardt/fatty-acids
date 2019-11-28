

library(tidyverse)
library(janitor)
library(cowplot)
library(wesanderson)


d <- read_excel("data-raw/ARA_EPA_DHA_perch_resources.xlsx") %>% 
	clean_names() %>% 
	rename(epa = percent_20_5n3_epa) %>% 
	rename(dha = percent_22_6n3_dha) %>% 
	mutate(resource_type = as.factor(resource_type))

diet <- read_csv("data-processed/perch-diet-contribution.csv") %>% 
	rename(resource_type = prey_type) %>% 
	rename(contribution_se = std.error)

str(d)

d_sum <- d %>% 
	group_by(resource_type, order) %>% 
	summarise_each(funs(mean, std.error), dha)


	ggplot(aes(x = order, y = mean, fill = resource_type)) + geom_bar(stat = "identity") +ylab("Mean percent DHA")

perch <- bind_rows(diet, d_sum)

perch2 <- left_join(d_sum, diet, by = "resource_type") %>% 
	unite(resource_type, order, col = "prey_item", remove = FALSE) %>% 
	mutate(number = case_when(resource_type == "Macroinvertebrate" ~ 1,
							  resource_type == "Cladocera" ~ 2,
							  resource_type == "Copepoda" ~ 3,
							  resource_type == "Fish" ~ 4)) %>% 
	mutate(number = as.numeric(number))


perch2$resource_type <- perch2$resource_type %>%  
	fct_relevel("Macroinvertebrate", "Cladocera", "Copepoda", "Fish") 
levels(perch2$resource_type)	

panela <- perch2 %>% 
 ggplot(aes(x = reorder(prey_item, -number), y = mean, fill = resource_type)) + 
	geom_bar(stat = "identity") +ylab("DHA (%)") +
	xlab("Prey type") +
	coord_flip() +
	theme(legend.position = "none") + 
	scale_fill_viridis_d()
	

panelb <- perch2 %>% 
	ggplot(aes(x = reorder(resource_type, -number), y = contribution, color = habitat, shape = habitat)) +
geom_point() +
geom_errorbar(aes(x = resource_type, y = contribution, ymin = contribution - contribution_se,
				  ymax = contribution + contribution_se, color = habitat), width = 0.1) +
	coord_flip() +
	xlab("") +ylab("Diet contribution (%)") +
	scale_color_manual(values = c("cadetblue", "orange"))



# EPA now -----------------------------------------------------------------

e_sum <- d %>% 
	group_by(resource_type, order) %>% 
	summarise_each(funs(mean, std.error), epa)



perch_e <- bind_rows(diet, e_sum)

perch3 <- left_join(e_sum, diet, by = "resource_type") %>% 
	unite(resource_type, order, col = "prey_item", remove = FALSE) %>% 
	mutate(number = case_when(resource_type == "Macroinvertebrate" ~ 1,
							  resource_type == "Cladocera" ~ 2,
							  resource_type == "Copepoda" ~ 3,
							  resource_type == "Fish" ~ 4)) %>% 
	mutate(number = as.numeric(number))


perch3$resource_type <- perch3$resource_type %>%  
	fct_relevel("Macroinvertebrate", "Cladocera", "Copepoda", "Fish") 
	

panela_epa <- perch3 %>% 
	ggplot(aes(x = reorder(prey_item, -number), y = mean, fill = resource_type)) + 
	geom_bar(stat = "identity") +ylab("EPA (%)") +
	xlab("Prey type") +
	coord_flip() +
	theme(legend.position = "none") + 
	scale_fill_viridis_d()


panelb_epa <- perch3 %>% 
	ggplot(aes(x = reorder(resource_type, -number), y = contribution, color = habitat, shape = habitat)) +
	geom_point() +
	geom_errorbar(aes(x = resource_type, y = contribution, ymin = contribution - contribution_se,
					  ymax = contribution + contribution_se, color = habitat), width = 0.1) +
	coord_flip() +
	xlab("") +ylab("Diet contribution (%)") +
	scale_color_manual(values = c("green", "purple"))

plots_epa <- plot_grid(panela_epa, panelb_epa, labels = c('A', 'B'), label_size = 12, align = "h")
save_plot("figures/draft-figure2c_epa.png", plots_epa, base_width = 10)

perch_composition <- read_csv("data-raw/summary_table.csv") %>% 
	clean_names() %>% 
	select(x22_6n_3_dha, final_cluster) %>% 
	filter(final_cluster %in% c("LB", "PP")) %>% 
	mutate(final_cluster = ifelse(final_cluster == "LB", "littoral", "pelagic"))

perch_composition_tissue <- perch_composition %>% 
	ggplot(aes(x = final_cluster, y = x22_6n_3_dha, fill = final_cluster)) + geom_boxplot() +
	ylab("DHA % in perch muscle tissue") + coord_flip() + xlab("") +
	scale_fill_manual(values = c("cadetblue", "orange")) +
	theme(legend.position = "none")

lm(x22_6n_3_dha ~ final_cluster, data = perch_composition) %>% summary()


names(perch_composition)

plots_dha <- plot_grid(panela, panelb, perch_composition_tissue, labels = c('A', 'B', "C"), 
					   label_size = 12, align = "h", nrow = 1, ncol = 3)
save_plot("figures/draft-figure2c_dha.png", plots_dha, base_width = 16)
