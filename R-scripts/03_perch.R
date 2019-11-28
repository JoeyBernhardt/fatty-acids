

library(plotrix)

### Perch data from Kristin

pd <- read_excel("data-raw/perch_dietcontr_FAresources.xlsx", sheet = "SIA contrib") %>% 
	mutate(type = case_when(final.cluster == "LP" ~ "littoral_planktivorous",
							final.cluster == "LB" ~ "littoral_benthivorous",
							final.cluster == "PP" ~ "pelagic_planktivorous",
							final.cluster == "PB" ~ "pelagic_benthivorous",
							final.cluster == "Pisc" ~ "littoral_piscivorous")) %>%
	separate(type, into = c("habitat_zone", "feeding_mode"), sep = "_", remove = FALSE) %>% 
	filter(feeding_mode != "piscivorous") %>% 
	filter(!type %in% c("pelagic_benthivorous", "littoral_planktivorous")) %>% 
	rename(Macroinvertebrate = macroinv) %>% 
	rename(Cladocera = cladocera) %>% 
	select(sample, habitat, Cladocera, Copepoda, Fish, Macroinvertebrate) %>% 
	gather(key = prey_type, value = amount, 3:6) %>% 
	group_by(habitat, prey_type) %>% 
	summarise_each(funs(mean), amount) %>% 
	rename(contribution = amount) %>% 
	mutate(contribution = contribution * 100)


	
fa <- read_excel("data-raw/perch_dietcontr_FAresources.xlsx", sheet = "FA resources") %>% 
	clean_names() %>% 
	rename(dha = x22_6n_3_dha) %>%
	rename(epa = x20_5n_3_epa) %>%
	rename(ara =x20_4n_6_ara) %>% 
	mutate(resource_type = ifelse(resource_type == "fish", "Fish", resource_type)) %>% View
	group_by(resource_type) %>% 
	summarise_each(funs(mean, std.error), epa, dha, ara)

fa %>% 
	ggplot(aes(x = resource_type, y = dha_mean)) + geom_point() +
	geom_errorbar(aes(ymin = dha_mean - dha_std.error, ymax = dha_mean + dha_std.error), width = 0.1)

pd %>% 
	ggplot(aes(x = prey_type, y = mean, color = habitat)) + geom_point() +
	geom_errorbar(aes(min = mean - std.error, max = mean + std.error), width = 0.1) +
	facet_wrap( ~ habitat) +ylab("Diet contribution")

fa %>% 
	filter(epa < 20) %>% 
	group_by(resource_type) %>%
	summarise_each(funs(mean, std.error), epa) %>% 
	ggplot(aes(x = resource_type, y = mean)) + geom_point() +
	geom_errorbar(aes(min = mean - std.error, max = mean + std.error), width  =0.1) +
	ylab("EPA percent")
ggsave("figures/perch-diet-data-Kristin.png", with = 8, height = 6)

all_data <- left_join(pd, fa, by = c("prey_type" = "resource_type"))

all_data %>% 
	gather(key = data_type, value= value, 3:6) %>% 
	# filter(data_type %in% c("epa", "dha", "ara")) %>% 
	ggplot(aes(x = prey_type, y = value, fill = data_type)) + geom_bar(stat = "identity", position="dodge") +
	facet_wrap( ~ habitat)
	
