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
  
logs_referrers <- read_csv("../data/graylog_hypo_1d_referers.csv")  

library(rex)
rex_mode()

valid_chars <- rex(one_of(regex('a-z0-9'))) # validation des caractères autorisés dans une URL

re <- rex(
  # sous-domaine
  # group(
  #   one_or_more('http'),
  #   zero_or_more('s')
  #   ),
  capture(name = "referrer_propre",
          group(zero_or_more(valid_chars, zero_or_more('-')), one_or_more(valid_chars), one_or_more('.')), # possibilité d'un sous domaine, se finit par un point
  # nom de domaine (1er niveau)
  group(zero_or_more(valid_chars, zero_or_more('-')), one_or_more(valid_chars)),
  #TLD identifier
  group('.', valid_chars %>% at_least(2)) # extension qui finit la regex
  )
)

re_deomaine <- rex(
  # sous-domaine
  # group(
  #   one_or_more('http'),
  #   zero_or_more('s')
  #   ),
  
          group(zero_or_more(valid_chars, zero_or_more('-')), one_or_more(valid_chars), one_or_more('.')), # possibilité d'un sous domaine, se finit par un point
          # nom de domaine (1er niveau)
          capture(name = "domaine_propre",
                  group(zero_or_more(valid_chars, zero_or_more('-')), one_or_more(valid_chars))
                  ),
          #TLD identifier
          group('.', valid_chars %>% at_least(2)) # extension qui finit la regex
)

pdf(file = "./mongraphique.pdf", width = 15, height = 12)
logs_referrers %>% 
  filter(response %in% 200) %>% 
  filter(verb %in% "GET") %>% 
  # filtrer les robots ?
  filter(!referrer %in% "\"-\"") %>% 
  mutate(referrer_propre = re_matches(referrer, re_deomaine)$`domaine_propre`) %>% 
  filter(referrer_propre != "hypotheses") %>% 
  group_by(geoip_country_name, referrer_propre) %>% 
  summarise(n = n()) %>% 
  group_by(geoip_country_name) %>% 
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  filter(n[1] > 2000) %>% 
  ggplot(aes(x = referrer_propre, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ geoip_country_name, scales = "free") +
  coord_flip() 
dev.off()

pdf("horaires.pdf", 20, 15)
logs_referrers %>% 
  filter(response %in% 200) %>% 
  filter(verb %in% "GET") %>% 
  # filtrer les robots ?
  filter(!referrer %in% "\"-\"") %>% 
  group_by(geoip_country_name) %>% 
  filter(n() > 10000) %>% 
  ungroup %>% 
  ggplot(aes(x = timestamp)) +
  geom_histogram(bins = 96) +
  facet_wrap(~ geoip_country_name, scales = "free")
dev.off()
