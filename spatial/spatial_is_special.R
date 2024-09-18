### Wednesday practical. Intro to spatial data in R ###
# Giacomo Bilotti

# if you did not install the packages yet or you do not know if they are already installed
# run the following line 
# source("install_packages.R")

## Set directory
# use the setwd() function
# setwd("your/directory/")

# specify where raw data are stored. It is not necessary but it saves time and 
# enhances reproducibility: if the source folder changes, you only need to change 
# this line and not all the ones where you mention it (in this case already 5-10 times)
sourcedir <- file.path("data", "raw_data")

# Create the target directory, i.e. where results will be stored. 
targetdir <- file.path("data", "derived_data")
if(dir.exists(targetdir) == FALSE){
  dir.create(targetdir)
}

## Load libraries
library(sf)
library(terra)
library(tmap)
library(dplyr)

## Load sites: csv

sites <- read.csv(
  file = file.path(sourcedir, "sites.csv")
)

# Look at the data
print(head(sites))

# You can plot them very easily in base R

plot(x = sites$LON, # Longitude (E-W)
     y = sites$LAT) # Latitude (N-S)

## Load area: shp 
# To load a shapefile we need to use the sf package
area <- read_sf(
  dsn = file.path(sourcedir, "area", "geoBoundaries-JOR-ADM0.shp")
)

# Plot the area
plot(area)
# Something weird?
# try this
plot(area$geometry)
# Better?

# This is because sf are "Simple Features" objects, basically data frames with 
# a geometry feature. If a column has some information you can plot it directly. 
plot(area[,"shapeName"])
# In this case there is only one geometry but later we will see the difference.

# Now try to plot the sites AND Jordan on the same map.

# Did you manage? Actually, it is fine if you did not as the sites are not an sf 
# object yet. However, if you do this you might have succeeded:

plot(area$geometry)
points(x = sites$LON, y = sites$LAT)

# Now let's convert the sites into an sf object
# what function should we use? 

sites <- st_as_sf(
  x = sites,
  coords = c("LON", "LAT"), 
  crs = 4326, ## Specifying the correct CRS is essential 
  na.fail = FALSE ## If NA are present and you do not set this as FALSE, it will throw an error 
)

# Now plot them together

plot(area$geometry)
plot(sites[,"Type"], add = TRUE)

## Save as gpkg
# gpkgs are much better than shp for 3 reasons: 
# 1- they can handle large datasets
# 2- they are more "future proof" 
# 3- there is no risk of "loosing" pieces, all the info are in the same file

# save area
st_write(
  obj = area,
  dsn = file.path(targetdir, "jordan.gpkg"),
  layer = paste(sample(c(letters, LETTERS, 0:9), 4, replace = TRUE), collapse = "")
)
# save sites
st_write(
  obj = sites,
  dsn = file.path(targetdir, "jordan.gpkg"),
  layer = paste(sample(c(letters, LETTERS, 0:9), 5, replace = TRUE), collapse = "")
)
# Another advantage of gpkgs is that you can have multiple layers in a single file

# remove files
rm(sites, area)
# free memory
gc()

##### Re-load area/sites: gpkg #####
# this time we do it from the newly created gpkg

# the function to use for this is called st_read. try to load a vector
# st_read(dsn  = file.path(targetdir, "jordan.gpkg"))
# You need to specify a "dsn" (path) and if you want/need a "layer"
# make changes accordingly
st_read(
  dsn = "your/path",
  layer = "your_layer"
)

# Maybe you managed. In any case, you can check if a gpkg has multiple layers, 
# their names and type using the st_layer function like this:
st_layers(file.path(targetdir, "jordan.gpkg"))
# Sometimes it is not you that define the layer name and may not know what layers
# are stored into a gpkg, thus it is a good way to verify. Opening it in QGIS 
# is also an option for quick visualisation.
laynames <- st_layers(file.path(targetdir, "jordan.gpkg"))
# once you know the names, it is easier to call the correct layer

# Sites
sites <- st_read(
  dsn  = file.path(targetdir, "jordan.gpkg"),
  layer = laynames$name[laynames$geomtype == "Point"]
  # if you do not specify a layer it will load the first one
)
# Area
area <- st_read(
  dsn  = file.path(targetdir, "jordan.gpkg"),
  layer = laynames$name[laynames$geomtype == "Polygon"] 
)

#### Load DEM: raster ####

dem <- rast(
  x = file.path(sourcedir, "DEM_large.tif")
)

#### Look at the structure of the data ####

# Look at the structure of the sites
sites
# it is fairly similar to the data frame after loading the csv file.  
# the main difference is the "geom" field.
# you can check it using the class function
class(sites)
# look at the number of points, the fields and the entries
# To get a better understanding of the data structure you can use the str function
str(sites)
# nrows and ncols
dim(sites)
# look at some entries
head(sites)
# Data have been previously cleaned, divided by broader typologies and chronology
# Site are divided as follows:
unique(sites$Type)
# and have the following chronology: 
unique(sites$Age.s.)

# the function that can be applied to an sf are
methods(class = "sf")

## terra
# Raster
str(dem)
class(dem)
# number of rows, columns and layers
dim(dem)
# the number of cells (pixels)
ncell(dem)
# spatial resolution
res(dem) # you need to know the unit (comes with CRS)
# spatial extent
ext(dem) 

# Vector
head(terra::vect(sites))
# the function that can be applied to an terra rast/vect are
methods(class = "SpatRaster")
methods(class = "SpatVector")

#### Subset spatial data ####

## Now try to subset the sites, extract only certain categories (you can choose)
# You can use the filter/slice/subset (from dplyr) or base R `[` operator.
# If you want to subset columns instead of rows

# your code
#
#
# after you finished try to get only the type field (no geom) 
# You have 3 ways:
sites$Type
sites[["Type"]]
pull(sites, "Type")

# the nice part of dplyr is the fact that you can chain multiple commands with pipes:

sites |>
  filter(Type == "Sherd scatter") |>
  select("Site.No") |>
  slice(1:5)

#### Easy plot ####
## Now try to plot a subset of sites on the DEM. Do it as nicely as possible
# e.g., change the shape/size/col of sites according to type/chrono, zoom in to a certain area etc. 
# This is a very simple yet effective way. 
# If you are bored you can play with the legend() and other graphic parameters/functions

plot(dem, range = c(-500, 1300), ext = vect(sites)) # force it to sites extent
points(sites, pch = 19)

#### Spatial operations ####
## Now let;s group spatial data
# add a numeric value so we can sum it
sites$num <- 1
sites |>
  group_by(Type) |>
  summarize(num = sum(num, na.rm = TRUE))

## Create 2 different vectors with a common fields to be joined

sites_type <- sites[,c("Site.No", "Type")]
sites_chrono <- data.frame(sites[,c("Site.No", "Age.s."),])[,1:2]

# now try to join
# use the dplyr function left_join for this
# you need to specify 2 objects to join and how (by = ....)

left_join(obj_1, obj_2, by = "")

## unite cols
tidyr::unite(data = sites, col = "type_chrono", Type, Age.s., sep = ": ")
# its opposite function is seprate()

#### Vector operations ####
## Let's do some vector operations now

# set.seed(0)
# Define area
# we can create a smaller area based on the DEM extension

# Let's run the commands one after just to look at the output 
ext(dem) 
st_bbox(dem) # same but better format to create and sf object 
st_as_sfc(st_bbox(dem))
# check if it already has a crs
st_crs(st_as_sfc(st_bbox(dem)))

## Now create an "area" object yourself

# Random sample 100 points with the st_sample area
ran_pts <- st_sample(area, 1000)

## Load country borders
vector_filepath = system.file("shapes/world.gpkg", package = "spData")
new_vector = read_sf(vector_filepath)

# check which points are contained in a country 
# you can use the st_contains funciton
# do not forget that if you do not know how to put the argument you can always do:
?st_contains
# This time you have the solution
st_contains(new_vector[new_vector$name_long == "Israel",], ran_pts)
st_contains(new_vector[new_vector$name_long == "Israel",], ran_pts)[[1]]

# intersect points to Jordan area
# use the st_intersection function and create an object named jo_pts (or whatever you like):

# jo_pts <- st_intersection()

# Now try to compute Euclidean distance between our sites. 
# Use a subset of sites based on a specific period or type

st_distance(sites[sites$Type == "Sherd scatter",])

# Join points (random ones) with country information
st_join(st_as_sf(ran_pts), new_vector["name_long"])

#### Plot some operations ####
## Now try to plot some of the things you created 


#### Spatial operations 2 ####

# Simplify vectors
# for example
st_simplify(area, dTolerance = 50000)
# now do it for Western Asia

# try to play around with the tolerance and quicky visualise the results
# plot(simplified)

#### Buffering ####  
# you can do it with points or polygons. Do it for both and visualise it. 
# better if you subset data! if you created a subset of sites you can reuse them 
# Buffer some points
# Paleolithic sites
st_buffer(sites[grepl("palaeolit", sites$Age.s., ignore.case = TRUE),"Type"], dist = 1000)

# Buffer a polygon (a state or something like this)
# DIY

#### Plot buffers ####
## Now plot some stuff (e.g., buffered sites, sites and local DEM)

#### Spatial operation 3 ####
# Compute centroids
# use the st_centroid function

# Shift
# Better to work with projected data when doing this type of operations
proj_pts <- st_transform(sites, "EPSG:32637") # use a subset of sites (less points)

shifted_pts <- st_geometry(proj_pts) + c(20000, 20000)

# union
st_buffer(proj_pts, dist = 1000) |>
  st_union() 

## Now find 2 sites that are next to each other and do some operations with their buffers
# This depends on the data you decided to use so you need to have a look at them and try to subset them
# Hint: if you use the st_distance on the points you can find out which ones are close to each other. 
# You can also use extremely large buffers to make sure your points intersect

## try to do intersections, differences and unions

st_buffer(proj_pts[12,], dist = 100000) |>
  st_intersection(st_buffer(proj_pts[13,], dist = 100000)) 

st_buffer(proj_pts[12,], dist = 100000) |>
  st_sym_difference(st_buffer(proj_pts[13,], dist = 100000)) |>
  plot(col = "blue")

## We can also convert points into other features (like )
st_union(proj_pts[10:16,]) |>
  st_cast("MULTILINESTRING") |>
  plot(lwd = 2)


#### Raster operations #### 
## Now let's work with rasters

# subset data 

# row 1, column 1
dem[1,1]
# cell ID 1 (the same one)
dem[1]
# row 32
dem[32,]
# col 32
dem[,32]
# you can change a cell value like this
elev <- dem
elev[2000:6000, 4000:5000] <- NA 

# get all values like this:
# is the raster is big it takes a lot of time, consider not doing it
# values(dem)
# dem[]

# Map algebra
elev <- dem
elev + elev
elev^2
log(elev)
elev > 5

## try some yourself: create new rasters and then plot it
## careful, it may be too heavy. If it is you can create a dummy raster to play with

r <- terra::rast(nrows = 10, ncols = 10)
values(r) <- matrix(runif(100), ncol = 10)

## plot some of these things

## do some basic stats with rasters
# Histogram
terra::hist(dem)
# Boxplot
terra::boxplot(dem)
# # You can extract the values like this
# stats::quantile(values(dem), na.rm = TRUE)

# aggregate, aka reduce resolution
elev <- aggregate(dem, fact = 4, cores = parallel::detectCores()/2) # fact is number of cells in each direction
# Reclassify to make it non-continuous (low, medium, high) you can choose your own values
rcl <- matrix(c(-428, 550, 1, 550, 855, 2, 855, 3084, 3), ncol = 3, byrow = TRUE)
recl <- classify(elev, rcl = rcl)
plot(recl)

## Now let's have some vector-raster interactions

# Extract elevation for specific location and store it in a column
sf_pts <- sites 
sf_pts[["height"]] <- extract(dem, vect(sf_pts))[,2]
# same as this:
sf_pts <- cbind(sf_pts, extract(dem, vect(sf_pts)))
# remove NAs (basically points with no coords have no elev)
sf_pts <- sf_pts[!is.na(sf_pts$height),]

# Aggregate value by country (and exclude the empty countries)
# This is technically a vector operation but was only possible after extraction from DEM
height_agg <- aggregate(x = sf_pts, by = new_vector, FUN = mean) # they are all in Jordan but if you used the random points not!
height_agg <- height_agg[!is.na(height_agg$height),] |>
  st_crop(area) # Crop to extent
# actually, the simplified shapefile of the world is not precise at the border and some sites are in Israel

## Try to plot You can see where in each country sites have a higher elevation
## you can also look at the elevation pattern of the sites

hist(sf_pts$height) # it is kind of a normal distribution
quantile(sf_pts$height)

# Do it by period! the grepl function is a really powerfool tool, do not forget about it ;)
hist(sf_pts$height[grepl("roman", sf_pts$Age.s., ignore.case = TRUE)])

## How to: extract coastline from a DEM
coastline <- elev
# Get only sea area (NA values)
# the ifelse() functions allows you to set a condition to your input data.
# if it is matched, it returns the "yes" value, otherwise the "no".
values(coastline) <- ifelse(
  test = is.na(values(elev)), # if values are NA
  yes = 1,  # make them = 1
  no = NA # else make them NA
)

# compute distance
coast_dist <- distance(aggregate(coastline, fact = 4), unit = "km")
coast_dist[coast_dist == 0] <- NA # make sea as NA
# save it if you want
# writeRaster(coast_dist, file.path(targetdir, "sea_dist.tif"))

## If internet is not super fast it will take some time. On monday it took about 0.5 mins
dem_1 <- geodata::elevation_3s(lon = 35, lat = 35, path = tempdir())
dem_2 <- geodata::elevation_3s(lon = 32, lat = 35, path = tempdir())

# merge data
dem_merged <- merge(dem_1, dem_2)

# Mask using Jordan extent 
dem_jo <- dem_merged |>
  crop(vect(new_vector[new_vector$name_long == "Jordan",])) |>
  mask(vect(new_vector[new_vector$name_long == "Jordan",]))

# vectorize coastline
poly_coastline <- as.polygons(coastline) |>
  st_as_sf() |>
  st_cast("POLYGON") # separate med from Read sea

# plot it
plot(st_cast(poly_coastline, "POLYGON")[2,])

# you can do the same for "land" instead of sea. You only need to set NA to sea and 1 (or any other value) to land
# Try it. The code would be mostly the same

coastline <- elev
# vectorize coastline
coastline[!is.na(coastline)] <- 1
poly_coastline <- as.polygons(coastline) |>
  st_as_sf() |>
  st_cast("POLYGON") # there are some islands

#### Intro to CRS ####
# we have not cared much about it as all data had the same CRS so far. However, it is worth knowing some practical basics
# Get crs
st_crs(sites)
# If missing (but you know it) you can specify it:
# st_set_crs(sites, "EPSG:4326") # set CRS, AUTHORITY:CODE (also code only works but this is safer)
# To check if it is Geographic coords or not you can do this:
st_is_longlat(sites)
# for rasters:
# crs(dem) <- "EPSG:4326" # set CRS (already correct)

dem_crop <- crop(dem, vect(sites))
dem_agg <- aggregate(dem, 4)

# sf 
st_transform(sites, "EPSG:32637")
# terra
project(dem_crop, "EPSG:32637")
project(vect(sites), "EPSG:32637")
# you can also project based on the CRS of another vect/rast
st_transform(sites, st_crs(new_vector))

## Plot 
par(mfrow = c(2,1))
plot(dem_crop)
points(sites)
plot(project(dem_crop, "EPSG:32637"))
points(st_transform(sites, "EPSG:32637"))

#### tmap ####
## Now let's try to do some nicer plotting using tmap

library(tmap)

tm_shape(new_vector[new_vector$subregion == "Western Asia",]) +
  tm_fill() 
tm_shape(new_vector[new_vector$subregion == "Western Asia",]) +
  tm_borders()
tm_shape(new_vector[new_vector$subregion == "Western Asia",]) +
  tm_polygons()
tm_shape(new_vector) +
  tm_polygons() + 
  tm_shape(new_vector[new_vector$subregion == "Western Asia",], is.master = TRUE) +
  tm_polygons(col = "gdpPercap", style = "log10", palette = "Spectral")

# you can check palettes/cols here
# tmaptools::palette_explorer()

tm_shape(dem_agg) +
  tm_raster(legend.hist = TRUE) +
  tm_legend(outside = TRUE)

tm_shape(dem_agg) +
  tm_raster(palette = "-Spectral", breaks = c(seq(-500,2500, by = 250)), 
            title = "Elevation (m asl)", midpoint = 750) +
  tm_shape(area) +
  tm_borders(col = "black") +
  tm_layout(bg.color = "lightblue", legend.outside = TRUE, inner.margins = 0,
            title = "A better looking map") +
  tm_grid(n.x = 3, n.y = 4, alpha = .5) +
  tm_compass(type = "4star", position = c("left", "top"), size = 3) +
  tm_scale_bar(text.size = 1, position = c("right", "BOTTOM"), lwd = 0.2,
               width = .3) 
# you can also store tmap objects and replot them later as any other R object you used so far
# then you can arrange multiple tmap plots with htis function. Try it!
# tmap_arrange()

## Create an inset map
# we need a base map and the main map and then we can combine them
# the individual maps are created normally
jor_region <- st_bbox(sites) |> 
  st_as_sfc()

main_map <- tm_shape(dem_crop) +
  tm_raster(palette = "-Spectral", style = "cont", midpoint = 550, title = "Elev (m asl)") +
  tm_shape(sites) +
  tm_symbols(size = .2, shape = 24, border.col = "navy", col = "white", alpha = 0.2) +
  tm_layout(legend.position = c("right", "top"), inner.margins = 0) +
  tm_scale_bar(position = c("LEFT", "BOTTOM"), lwd = 0.2, width = .25) +
  tm_compass(position = c("LEFT", "TOP"), size = 1.5, lwd = .2)

inset_map <- tm_shape(st_union(new_vector[new_vector$name_long == "Syria" |
                                            new_vector$name_long == "Israel",])) +
  tm_fill() +
  tm_shape(new_vector[new_vector$subregion == "Western Asia" |
                        new_vector$name_long == "Egypt",]) +
  tm_polygons(col = "grey90") +
  tm_shape(jor_region) +
  tm_borders(col = "red", lwd = 4) +
  tm_layout(bg.color = "lightblue")

## Using cowplot package you will get a similar result
# library(cowplot)
# ggdraw() +
#   draw_plot(tmap_grob(main_map)) +
#   draw_plot(tmap_grob(inset_map),
#     height = 0.35,
#     x = 0.39,
#     y = 0.137
#   )

## Using gird package 
vp <- grid::viewport(width = .35, height = .35, x = .88, y = .22)
## you can save maps as pdf or image file with some base R functions!
# png(filename = "inset.png", width = 480*2.3, height = 480)
main_map
print(inset_map, vp = vp)
# dev.off()

## Last bit, interactive maps. Very useful to look at your data very quickly 

tmap_mode("view")
tm_shape(new_vector[new_vector$subregion == "Western Asia",]) +
  tm_polygons(col = "lifeExp", palette = "Spectral", title = "Life Expetancy")
