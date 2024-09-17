# Script with some example queries to ARIADNE KB using glitter package
# By Petr Pajdla, Brno 2024-09-15
# For 'atRium' training school, https://www.aiscr.cz/atRium

# Packages 
# You will need to install `glitter` package from GitHub
# remotes::install_github("lvaudor/glitter")

library(glitter)
library(dplyr)
library(ggplot2)

# ARAIDNE KB SPARQL ednpoint
endpoint <- "https://graphdb.ariadne.d4science.org/repositories/ariadneplus-pr01"

# init query with the endpoint, same for all consequent queries
q <- spq_init(endpoint = endpoint, request_control = spq_control_request(request_type = "body-form")) %>% 
  spq_prefix(prefixes = c(aocat = "https://www.ariadne-infrastructure.eu/resource/ao/cat/1.1/",
                          aat = "http://vocab.getty.edu/aat/"))


# 1. GettyAAT subjects -------------------------------------------------------

# What are the 'derived subjects' (GettyAAT subjects/keywords) for artefacts in 
# ARIADNE KB? How many occurences of different artefacts are there?

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

q_needles_time <- q %>% 
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

r_needles_time <- q_needles_time %>% 
  spq_perform() %>% 
  # years as numbers
  mutate(across(c("from", "until"), as.integer))

# basic aoristic analysis
# package aoristAAR is used here, because it is faster to install, 
# alternative could be package kairos, but it takes longer to build
# remotes::install_github("ISAAKiel/aoristAAR")
needles_aoristic1 <- r_needles_time %>% 
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

q_needles_space <- q %>% 
  # artefacts only
  spq_add("?ae aocat:has_ARIADNE_subject ?s") %>%
  spq_add("?s rdfs:label 'Artefact'@en") %>% 
  # needles only (derived subject is a needle)
  spq_add("?ae aocat:has_derived_subject aat:300024789") %>% 
  # spatial coverage - points
  spq_add("?ae aocat:has_spatial_coverage ?sc") %>% 
  spq_add("?sc aocat:has_latitude ?lat") %>% 
  spq_add("?sc aocat:has_longitude ?lon") %>% 
  spq_select(ae, lat, lon)

# result as spatial features
r_needles_space <- q_needles_space %>% 
  spq_perform() %>% 
  mutate(across(c("lat", "lon"), as.numeric)) %>% 
  sf::st_as_sf(coords = c("lon", "lat"))

# simple leaflet map
r_needles_space %>% 
  leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addCircleMarkers()

# Tasks:
# - Modify the query for swords instead of needles.
# - Combine the queries to get time spans for needles as well.


# 4. Exercise ----------------------------------------------------------------

# Try to answer questions done in ARIADNE Portal using SPARQL queries to 
# ARIADNE KB, skip those that you do not know how to do.
# 1. How many **resources** are there in the ARIADNE Portal?
# 2. How many records of **individual artefacts** are there?
# 3. **Who** contributes the most of the records on artefacts?
# 4. How many **swords** are there?
# 5. How many **Bronze Age swords** are there?
# 6. Are there any records from Brno (Czech Republic)?
# 7. How many records are there from Africa?

