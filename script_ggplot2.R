## script ggplot2

# on commence par charger des données
library(tidyverse)
logs <- read_csv("../data/graylog_hypo_1d.csv")

logs %>% 
  group_by(geoip_country_name) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  slice(1:30) %>% 
  filter(geoip_country_name != "Europe") %>% 
  arrange(n) %>% 
  mutate(geoip_country_name = factor(geoip_country_name, levels = unique(geoip_country_name))) %>% 
  ggplot(aes(x = geoip_country_name, y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  xlab("Nombre de requêtes") +
  ylab("Pays d'origine") +
  theme_bw()
  
  