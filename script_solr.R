library(solrium)
library(tidyverse)

solr_connect('http://147.94.102.65:8983/solr/documents/select')

solr_facet(q = 'type:article', facet.field = "anneedatepubli")$facet_fields$anneedatepubli %>% 
  mutate(annee = as.integer(term)) %>% 
  filter(annee > 1990, annee < 2017) %>% 
  ggplot(aes(x = annee, y = as.integer(value))) +
  geom_line()

