# Script for ARIADNE data analysis
# By Petr Pajdla
# 2024-09-20 in Brno
# For atRium training school, https://www.aiscr.cz/atRium

library(glitter)
library(leaflet)
library(dplyr)


# get data ----------------------------------------------------------------

# ARAIDNE KB SPARQL ednpoint
endpoint <- "https://graphdb.ariadne.d4science.org/repositories/ariadneplus-pr01"

# init query with the endpoint, same for all consequent queries
q <- spq_init(endpoint = endpoint, request_control = spq_control_request(request_type = "body-form")) %>% 
  spq_prefix(prefixes = c(aocat = "https://www.ariadne-infrastructure.eu/resource/ao/cat/1.1/",
                          aat = "http://vocab.getty.edu/aat/"))

# query
q_coins <- q %>% 
  # artefacts only
  spq_add("?ae aocat:has_ARIADNE_subject ?s") %>%
  spq_add("?s rdfs:label 'Artefact'@en") %>% 
  # match only coins
  spq_add("?ae aocat:has_derived_subject aat:300037222") %>% 
  # spatial coverage
  spq_add("?ae aocat:has_spatial_coverage ?sc") %>% 
  spq_add("?sc aocat:has_place_name ?pn", .required = FALSE) %>% 
  spq_add("?sc aocat:has_latitude ?lat") %>%
  spq_add("?sc aocat:has_longitude ?lon") %>% 
  # temporal coverage
  spq_add("?ae aocat:has_temporal_coverage ?tc") %>% 
  spq_add("?tc aocat:from ?from") %>%
  spq_add("?tc aocat:until ?to") %>% 
  spq_head(200)

r_coins <- q_coins %>% 
  spq_perform()

r_coins %>% View()

# plot
sf_coins <- r_coins %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = "epsg:4326") %>% 
  mutate(across(c("from", "to"), as.numeric)) %>% 
  group_by(ae) %>% 
  summarise(from = min(from), to = max(to))

sf_coins
# sf_coins %>% 
#   leaflet() %>% 
#   addTiles() %>% 
#   addMarkers()


