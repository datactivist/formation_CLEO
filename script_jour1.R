# library(rio)
# logs <- import("../data/small_logs.csv")

library(readr)
logs <- read_csv("../data/small_logs.csv", 
                 col_types = cols(
                   .default = col_character(),
                   timestamp = col_datetime(format = ""),
                   bytes = col_integer(),
                   geoip_area_code = col_integer(),
                   geoip_dma_code = col_integer(),
                   geoip_latitude = col_double(),
                   geoip_longitude = col_double(),
                   httpversion = col_double(),
                   level = col_character(),
                   response = col_integer()
                 ))

write_excel_csv(logs, "./test.csv")

library(tidyverse)

sub_logs <- logs %>% 
  select(contains("geoip"))

# ceci est un commentaire

logs %>% 
  select(starts_with("geoip"))

logs %>% 
#  filter(geoip_city_name == "Marseille") %>% 
  select(timestamp, agent, bytes) %>% 
  mutate(contenu_vide = if_else(bytes == 0, "vide", "non vide", missing = "absent")) %>% 
  group_by(contenu_vide) %>% 
  summarise(n())

logs %>% 
  #  filter(geoip_city_name == "Marseille") %>% 
  select(timestamp, agent, bytes) %>% 
  mutate(contenu_vide = if_else(bytes == 0, true = "vide", false = "non vide", missing = "absent")) %>% 
  group_by(contenu_vide) %>% 
  summarise(heure = mean(timestamp),
            n = n())

library(stringr)

logs %>% 
  group_by(domain) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

library(rex)
rex_mode()
valid_chars <- rex(one_of(regex('a-z0-9\u00a1-\uffff')))

re <- rex(
  #host name
  group(zero_or_more(valid_chars, zero_or_more('-')), one_or_more(valid_chars), one_or_more('.')),
  #domain name
  capture(name = "top_domaine",
    group(zero_or_more(valid_chars, zero_or_more('-')), one_or_more(valid_chars))
    ),
  #TLD identifier
  group('.', valid_chars %>% at_least(2))
)


logs %>% 
  filter(!domain %in% c("core.openedition.org", "f-origin.hypotheses.org")) %>% 
  filter(!is.na(domain)) %>% 
  filter(!str_detect(clientip, "192.168.178")) %>% 
  filter(!str_detect(clientip, "193.48.96")) %>% 
  mutate(top_domaine = re_matches(domain, re)$top_domaine) %>% 
  group_by(top_domaine) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
  
# passer en format long et vice versa

logs %>% 
  mutate(ID = row_number()) %>% 
  gather(clé, valeur, -ID) %>% 
  spread(clé, valeur) %>% 
  mutate(timestamp = lubridate::as_datetime(as.integer(timestamp)))
