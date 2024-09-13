# Script with some example queries to Wikidata using glitter package
# By Petr Pajdla, Brno 2024-08-29
# For 'atRium' training school, https://www.aiscr.cz/atRium

library(glitter)
library(ggplot2)
library(dplyr)

# Find all archaeological artefacts made of bronze ------------------------

# In this query the question is how many and what are the archaeological 
# artefacts in Wikidata made of bronze. The thing is that in my (imaginary)
# research project I am looking for artefacts made of bronze from different
# places and periods because I want to make a comparative study on the 
# composition and technology of bronze mettalurgy in different parts of the 
# world.

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
# i am prefixing object names here with: 
# q_ for queries
# r_ for results
q_bronzes <- spq_init() %>% 
  spq_add("?a wdt:P31 wd:Q220659") %>% 
  spq_add("?a wdt:P186 wd:Q34095") %>% 
  spq_label(a, .languages = "en$")

# explore the query
q_bronzes

# perform the query
r_bronzes <- q_bronzes %>% 
  spq_perform()

# explore the results
r_bronzes
r_bronzes %>% View()

nrow(r_bronzes)


# Count how many artefacts are made of each material ----------------------

# We are also interested in materials that other artefacts are made of 
# to see the bronze artefacts in context. For instance we want to check 
# if there are other copper alloys represented in the data set...

# In this query we are grouping by labels, although we are not defining 
# the 'mat_label' variable anywhere, it is created for us by glitter in the
# call to 'spq_label()' function. Then we group on these labels and count
# how many occurrences of each material label are there and group the 
# results in a descending order.

# build a query
q_materials <- spq_init() %>% 
  spq_add("?a wdt:P31 wd:Q220659") %>% 
  spq_add("?a wdt:P186 ?mat") %>% 
  spq_label(mat, .languages = "en$") %>% 
  spq_group_by(mat_label) %>% 
  spq_count() %>% 
  spq_arrange(desc(n))

# explore the query
q_materials

# perform the query
r_materials <- q_materials %>% 
  spq_perform()

# explore the results
r_materials
r_materials %>% View()

# create a plot
r_materials %>% 
  # lets filter only materials with 200 or more artefacts
  filter(n > 200) %>% 
  ggplot() +
  aes(x = mat_label, y = n) +
  geom_col(fill = "white", color = "black") +
  labs(x = "Material", y = "Count") +
  coord_flip()


# Add coordinates and images where possible -------------------------------

# Finally lets check how many of the bronze artefacts have coordinates and
# images to have some notion of what are we looking at and from where.

# In this query we are asking again for artefacts made of bronze, but now 
# we are adding two optional triple matches ('.required = FALSE'), one 
# for coordinates and another one for an image URLs.
# Hits where there are coordinates are then transformed to simple features and 
# plotted using leaflet package.
# Images (if present) are added in popup messages with some basic HTML code.

WikidataR::find_property("coordinate")

# edit
q_coords <- spq_init() %>% 
  spq_add("?a wdt:P31 wd:Q220659") %>% 
  spq_add("?a wdt:P186 wd:Q34095") %>% 
  spq_add("?a wdt:P625 ?coords", .required = FALSE) %>% 
  spq_add("?a wdt:P18 ?img", .required = FALSE) %>% 
  spq_label(a, .languages = "en$")

# explore
q_coords

# perform
r_coords <- q_coords %>% 
  spq_perform()

# explore the results
r_coords %>% View()

# how many images have coordinates and images?
r_coords <- r_coords %>% 
  mutate(has_img = !is.na(img),
         has_coords = !is.na(coords))

table(r_coords$has_img)
table(r_coords$has_coords)

# barplot of artefacts with images
r_coords %>% 
  ggplot() +
  aes(has_img) +
  geom_bar(fill = "white", color = "black") +
  labs(x = "Artefact has image", y = "Count")

# barplot of artefacts with coordinates
r_coords %>% 
  ggplot() +
  aes(has_coords) +
  geom_bar(fill = "white", color = "black") +
  labs(x = "Artefact has image", y = "Count")

# transform to simple features to draw a map
# simple features (package sf) are a standard way of representing spatial data
r_coords_sf <- r_coords %>% 
  # filter out rows without coordinates
  dplyr::filter(!is.na(coords)) %>%
  # transform from WKT (well-known text) format to simple features
  sf::st_as_sf(wkt = "coords") %>% 
  dplyr::mutate(
    # if - else statements to create HTML code for popups, 
    # if there is no image link, create a popup with a label linking to WD page,
    # else create a popup with a label and an image
    popup = dplyr::if_else(
      is.na(img), 
      paste0("<a href='", a,"'><h2>", a_label, "</h2></a>"), 
      paste0("<a href='", a,"'><h2>", a_label, "</h2></a><img src='", img, "' width='200' height='200'>")))

# plot using leaflet (a package for interactive maps)
r_coords_sf %>% 
  # initialize leaflet 
  leaflet::leaflet() %>% 
  # add OSM map background tiles
  leaflet::addTiles() %>% 
  # add marks for the finds
  leaflet::addMarkers(popup = r_coords_sf$popup)

