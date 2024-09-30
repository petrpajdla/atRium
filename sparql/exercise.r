# Script on queryingdata from ARIADNE KB and analysing the...?
# By Petr Pajdla, Brno 2024-09-18
# For 'atRium' training school, https://www.aiscr.cz/atRium

# Packages 
# You will need to install `glitter` package from GitHub
# remotes::install_github("lvaudor/glitter")
# Note to myself: Funny thing, if this is ever repeated, mind that
# GitHub allows only certain amount of traffic from a certain
# IP address, so tell people to install in advance, use VPN etc.

library(glitter)
library(dplyr)
library(ggplot2)

# ARAIDNE KB SPARQL ednpoint
endpoint <- "https://graphdb.ariadne.d4science.org/repositories/ariadneplus-pr01"

# init query with the endpoint, same for all consequent queries
q <- spq_init(endpoint = endpoint, request_control = spq_control_request(request_type = "body-form")) %>% 
  spq_prefix(prefixes = c(aocat = "https://www.ariadne-infrastructure.eu/resource/ao/cat/1.1/",
                          aat = "http://vocab.getty.edu/aat/"))

# artefacts 

find_aat_subject <- function(x, pattern) {
  if (is.na(x)) {
    # ARAIDNE KB SPARQL ednpoint
    endpoint <- "https://graphdb.ariadne.d4science.org/repositories/ariadneplus-pr01"
    
    # init query with the endpoint, same for all consequent queries
    q <- spq_init(endpoint = endpoint, request_control = spq_control_request(request_type = "body-form")) %>% 
      spq_prefix(prefixes = c(aocat = "https://www.ariadne-infrastructure.eu/resource/ao/cat/1.1/",
                              aat = "http://vocab.getty.edu/aat/"))
    
    # query to ARIADNE KB
    q_aat <- q %>% 
      spq_prefix(prefixes = c(dc = "http://purl.org/dc/elements/1.1/")) %>% 
      # artefacts only
      spq_add("?ae aocat:has_ARIADNE_subject ?s") %>%
      spq_add("?s rdfs:label 'Artefact'@en") %>% 
      # derived / GettyAAT subjects
      spq_add("?ae aocat:has_derived_subject ?ds") %>% 
      spq_add("?ds rdfs:label ?ds_label") %>% 
      spq_add("?ds dc:identifier ?id") %>% 
      # count on derived subjects
      spq_count(id, ds_label) %>% 
      spq_arrange(desc(n))
    
    # perform query
    x <- q_aat %>% 
      spq_perform()
  }
  
  # search string
  x[grep("pot", x$ds_label), ]
}


q_aat <- q %>% 
  spq_prefix(prefixes = c(dc = "http://purl.org/dc/elements/1.1/")) %>% 
  # artefacts only
  spq_add("?ae aocat:has_ARIADNE_subject ?s") %>%
  spq_add("?s rdfs:label 'Artefact'@en") %>% 
  # derived / GettyAAT subjects
  spq_add("?ae aocat:has_derived_subject ?ds") %>% 
  spq_add("?ds rdfs:label ?ds_label") %>% 
  spq_add("?ds dc:identifier ?id") %>% 
  # count on derived subjects and their labels
  spq_count(id, ds_label) %>% 
  spq_arrange(desc(n))

q_aat

r_aat <- q_aat %>% 
  spq_perform()

grep("pot", r_aat$ds_label)

r_aat[c(24, 78),]




q_needles_time <- q %>% 
  # artefacts only
  spq_add("?ae aocat:has_ARIADNE_subject ?s") %>%
  spq_add("?s rdfs:label 'Artefact'@en") %>% 
  # needles only (derived subject is a needle)
  # spq_add("?ae aocat:has_derived_subject aat:300024789") %>% 
  # temporal coverage
  spq_add("?ae aocat:has_temporal_coverage ?tc") %>%
  spq_add("?tc aocat:from ?from") %>%
  spq_add("?tc aocat:until ?until") %>%
  spq_select(ae, from, until)

r_needles_time <- q_needles_time %>% 
  spq_perform() %>% 
  # years as numbers
  mutate(across(c("from", "until"), as.integer))




