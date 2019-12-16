

library(tidyverse)
library(janitor)
library(cowplot)

theme_set(theme_cowplot())


bird <- read_csv("data-raw/Bird-Diet-Data.csv") %>% 
	clean_names()


bird %>% 
	ggplot(aes(x = reorder(taxa, env_count_mean), y = env_count_mean)) + geom_bar(stat = "identity")
ggsave("figures/bird-count-abundance.png", width = 10, height = 6)
