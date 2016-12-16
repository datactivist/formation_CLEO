library(tidyverse)
carnets <- read_tsv("../data/coordonnees-carnets.csv",
                    col_types = cols(
                      .default = col_character(),
                      `Date de mise en ligne` = col_date(format = ""),
                      `Membre responsable` = col_integer(),
                      `Membre responsable_1` = col_integer(),
                      `Membre responsable_2` = col_integer(),
                      `Membre responsable_3` = col_integer(),
                      `Membre responsable_4` = col_integer(),
                      `Membre responsable_5` = col_integer()
                    ))

library(banR)

carnets <- carnets %>% 
  filter(!is.na(`Code INSEE Ville Adresse`)) %>% 
  ban_geocode(Adresse, code_insee = "`Code INSEE Ville Adresse`")

library(sp)

carnets <- carnets %>% 
  filter(!is.na(latitude)) %>% 
  filter(latitude > 0) %>% 
  filter(result_score > 0.5)

coordinates(carnets) <- c("longitude", "latitude")
proj4string(carnets) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")


library(mapview)
mapview(carnets)

library(tmap)
library(tmaptools)
library(OpenStreetMap)

# fonds_osm <- read_osm(carnets)

library(spdplyr)
carnets <- carnets %>% 
  mutate(Langue = stringr::str_sub(Langue, 1, 2))

# tm_shape(fonds_osm) +
#  tm_raster() +
carte <- tm_shape(carnets) +
  tm_bubbles(col = "Langue", size = 0.3, alpha = 0.5, palette = "Set1") +
  tm_legend(legend.outside = TRUE, legend.outside.position = "left")

tmap_mode(mode = "view")

carte

tmap_mode(mode = "plot")
carte
