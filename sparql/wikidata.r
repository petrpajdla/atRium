# Script with some example queries to Wikidata using glitter package
# By Petr Pajdla, Brno 2024-08-29
# For 'atRium' training school, https://www.aiscr.cz/atRium

library(glitter)

# Find all archaeological artefacts made of bronze ------------------------

# First we need to identify what properties and entities we are looking for 
# in Wikidata, package WikidataR will help us with this task.

# entities
WikidataR::find_item("archaeological artefact")
WikidataR::find_item("bronze")

# properties 
WikidataR::find_property("instance of")
WikidataR::find_property("made of")

# Next we build up a query using glitter package. 
# It is always a good idea to carefully examine the SPARQL query created by 
# glitter to identify any problems or things you do not understand!
# Note: The dollar sign ($) at the end of the language tag ensures that 
# only labels with @en language tag are returned, and not @en-gb @en-us etc.

# build a query
q1 <- spq_init() %>% 
  spq_add("?a wdt:P31 wd:Q220659") %>% 
  spq_add("?a wdt:P186 wd:Q34095") %>% 
  spq_label(a, .languages = "en$")

# explore the query
q1

# perform the query
res1 <- q1 %>% 
  spq_perform()

# explore the results
res1 %>% View()


# Count how many artefacts are made of each material ----------------------

# In this query we are grouping by labels, although we are not defining 
# the 'mat_label' variable anywhere, it is created for us by glitter in the
# call to 'spq_label()' function. Then we group on these labels and count
# how many occurrences of each material label are there and group the 
# results in a descending order.

# build a query
q2 <- spq_init() %>% 
  spq_add("?a wdt:P31 wd:Q220659") %>% 
  spq_add("?a wdt:P186 ?mat") %>% 
  spq_label(mat, .languages = "en$") %>% 
  spq_group_by(mat_label) %>% 
  spq_count() %>% 
  spq_arrange(desc(n))

# explore the query
q2

# perform the query
res2 <- q2 %>% 
  spq_perform()

# explore the results
res2


# Add coordinates and images where possible -------------------------------

# In this query we are asking again for artefacts made of bronze, but now 
# we are adding two optional triple matches ('.required = FALSE'), one 
# for coordinates and another one for an image URLs.
# Hits where there are coordinates are then transformed to simple features and 
# plotted using leaflet package.
# Images (if present) are added in popup messages with some basic HTML code.

WikidataR::find_property("coordinate")

# edit
q3 <- spq_init() %>% 
  spq_add("?a wdt:P31 wd:Q220659") %>% 
  spq_add("?a wdt:P186 wd:Q34095") %>% 
  spq_add("?a wdt:P625 ?coords", .required = FALSE) %>% 
  spq_add("?a wdt:P18 ?img", .required = FALSE) %>% 
  spq_label(a, .languages = "en$")

# explore
q3

# perform
res3 <- q3 %>% 
  spq_perform()

# explore the results
res3 %>% View()

# transform to simple features
res3sf <- res3 %>% 
  dplyr::filter(!is.na(coords)) %>% 
  sf::st_as_sf(wkt = "coords") %>% 
  dplyr::mutate(
    popup = dplyr::if_else(
      is.na(img), 
      paste0("<a href='", a,"'><h2>", a_label, "</h2></a>"), 
      paste0("<a href='", a,"'><h2>", a_label, "</h2></a><img src='", img, "' width='200' height='200'>")))

# plot using leaflet
res3sf %>% 
  leaflet::leaflet() %>% 
  leaflet::addTiles() %>% 
  leaflet::addMarkers(popup = res3sf$popup)
