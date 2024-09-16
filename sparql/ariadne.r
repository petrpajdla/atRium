# Script with some example queries to ARIADNE KB using glitter package
# By Petr Pajdla, Brno 2024-09-15
# For 'atRium' training school, https://www.aiscr.cz/atRium

# Packages 
# You will need to install `glitter` package from GitHub
# remotes::install_github("lvaudor/glitter")

library(glitter)
library(dplyr)
library(ggplot2)
library(sf)

# ARAIDNE KB SPARQL ednpoint
endpoint <- "https://graphdb.ariadne.d4science.org/repositories/ariadneplus-pr01"

# init query with the endpoint, same for all consequent queries
q <- spq_init(endpoint = endpoint, request_control = spq_control_request(request_type = "body-form")) %>% 
  spq_prefix(prefixes = c(aocat = "https://www.ariadne-infrastructure.eu/resource/ao/cat/1.1/",
                          aat = "http://vocab.getty.edu/aat/"))


# 1. GettyAAT subjects -------------------------------------------------------

# What are the 'derived subjects' (GettyAAT subjects/keywords) for artefacts in 
# ARIADNE KB? How mayn occurences of different artefacts are there?

q_aat <- q %>% 
  # artefacts only
  spq_add("?ae aocat:has_ARIADNE_subject ?s") %>%
  spq_add("?s rdfs:label 'Artefact'@en") %>% 
  # derived / GettyAAT subjects
  spq_add("?ae aocat:has_derived_subject ?ds") %>% 
  spq_add("?ds rdfs:label ?ds_label") %>% 
  # count on derived subjects and their labels
  spq_count(ds, ds_label) %>% 
  spq_arrange(desc(n))

q_aat

r_aat <- q_aat %>% 
  spq_perform()

# Tasks:
# - Create a bar plot showing the distribution of GettyAAT subjects.
# - Modify the query and check what GettyAAT subjects are available for 
# Sites/Monuments and think about why.
# - For a small subset (limit your query) check the relationship of the 
# GettyAAT subjects and native subjects.


# 2. Time spans of needles ------------------------------------------------

# What are the time spans for needles? Create an aoristic model.

q_needles <- q %>% 
  # artefacts only
  spq_add("?ae aocat:has_ARIADNE_subject ?s") %>%
  spq_add("?s rdfs:label 'Artefact'@en") %>% 
  # needles only (derived subject is a needle)
  spq_add("?ae aocat:has_derived_subject aat:300024789") %>% 
  # temporal coverage
  spq_add("?ae aocat:has_temporal_coverage ?tc") %>%
  spq_add("?tc aocat:from ?from") %>%
  spq_add("?tc aocat:until ?until") %>%
  spq_select(ae, from, until)

r_needles <- q_needles %>% 
  spq_perform() %>% 
  # years as numbers
  mutate(across(c("from", "until"), as.integer))

# basic aoristic analysis
# package aoristAAR is used here, because it is faster to install, 
# alternative could be package kairos, but it takes longer to build
# remotes::install_github("ISAAKiel/aoristAAR")
needles_aoristic1 <- r_needles %>% 
  aoristAAR::aorist(from = "from", to = "until", stepwidth = 100, method = "weight")

needles_aoristic1

# plot
needles_aoristic1 %>% 
  ggplot() +
  aes(x = date, y = sum) +
  geom_line()

# Tasks:
# - Identify and remove outliers in dates.
# - Create an aoristic sum for needles without outliers.
# - Plot it using ggplot2.


# 3. Distribution of needles ----------------------------------------------

# What is the spatial distribution of needles? Create a simple map.

# geographic extent
artefacts <- spq_init(endpoint = endpoint, request_control = spq_control_request(request_type = "body-form")) %>% 
  spq_prefix(prefixes = c(aocat = "https://www.ariadne-infrastructure.eu/resource/ao/cat/1.1/")) %>% 
  # artefacts only
  spq_add("?artefact aocat:has_ARIADNE_subject ?s") %>%
  spq_add("?artefact rdfs:label ?label") %>% 
  spq_add("?s rdfs:label 'Artefact'@en") %>% 
  # swords only
  spq_add("?artefact aocat:has_derived_subject ?ds") %>% 
  spq_add("?ds rdfs:label 'swords'@en") %>% 
  # temporal coverage
  # spq_add("?artefact aocat:has_temporal_coverage ?tc") %>% 
  # spq_add("?tc aocat:from ?from") %>% 
  # spq_add("?tc aocat:until ?until") %>% 
  spq_select(ds_label) %>% 
  spq_head(500)

artefacts %>% 
  spq_perform() %>% View

PREFIX aocat: <https://www.ariadne-infrastructure.eu/resource/ao/cat/1.1/>
  PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
SELECT * WHERE {
  
  ?r aocat:has_spatial_coverage ?sc .
  ?sc rdf:type aocat:AO_Spatial_Region_BBox .
  ?sc aocat:has_bounding_box_min_lat "41.2353929"^^xsd:decimal .
  ?sc aocat:has_bounding_box_min_lon ?bminlon .
  ?sc aocat:has_bounding_box_max_lat ?bmaxlat.
  ?sc aocat:has_bounding_box_max_lon ?bmaxlon .
  
}


