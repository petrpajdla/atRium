# Exercise on point pattern analysis with Michael Kempf
# Script by Petr Pajdla
# 2024-09-19
# For atRium training school, https://www.aiscr.cz/atRium

# install spatstat with install.packages("spatstat") etc. for missing pckgs

library(ggplot2)
library(sf)
library(terra)
library(spatstat)

# load the data, change the path, if your project structure is different
# sites <- read_sf(here::here("data/raw/"))
sites <- read.csv(here::here("data/raw_data/sites.csv"), header = TRUE)

# let's extract only the complete cases
# we need to remove sites that have missing values in one of the coordinates
sites <- sites[complete.cases(sites$LAT, sites$LON), ]

# sites as a spatstat object
# vect is a function from terra package, 
# it creates an object specific to terra package from the sf object
sites <- vect(st_as_sf(sites, coords = c("LON", "LAT"), crs = 4326))

# geoboundaries data, queried from https://geoboundaries.org
area <- vect(here::here("data/raw_data/area/geoBoundaries-JOR-ADM0.shp"))

# plot
plot(area)
plot(sites, add = TRUE, col = "red")
plot(bbox, add = TRUE)
plot(bbox2, add = T)
# alternative approach with ggplot2 
# ggplot2 plots simple features/sf, we need to transform 
# terra/vect -> sf
ggplot() +
  geom_sf(data = st_as_sf(area), color = NA, fill = "lightblue") +
  geom_sf(data = st_as_sf(sites), shape = 4) +
  theme_gray()

# project sites
sites_proj <- project(sites, "epsg:3857")

# limiting the area to the extent of points
# i use sf, because terra is shit
ext <- sites_proj %>% 
  st_as_sf() %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_buffer(2e3) %>% 
  st_bbox()

# plot
ggplot() +
  geom_sf(data = st_as_sf(area), color = NA, fill = "lightblue") +
  geom_sf(data = ext) +
  geom_sf(data = st_as_sf(sites), shape = 4) +
  theme_gray()

# point pattern
sites_ppp <- spatstat.geom::as.ppp(st_as_sf(sites_proj))
marks(sites_ppp) <- NULL    

# window
window <- owin(xrange = c(ext[1], ext[3]), yrange = c(ext[2], ext[4]))

Window(sites_ppp) <- window

