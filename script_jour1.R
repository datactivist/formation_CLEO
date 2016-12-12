library(rio)
logs <- import("../data/small_logs.csv")

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

logs %>% 
  filter(!domain %in% c("core.openedition.org", "f-origin.hypotheses.org")) %>% 
  filter(!is.na(domain)) %>% 
  filter(!str_detect(clientip, "192.168.178")) %>% 
  filter(!str_detect(clientip, "193.48.96")) 
