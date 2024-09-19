# Point pattern analysis in R
# By Michael Kempf 
# slight edits by Petr Pajdla, sorry Michael, I deleted your setwd() calls
# and changed call from Kinhom to Kest
# Brno, 2024-09-19
# For atRium training school, https://www.aiscr.cz/atRium

library(terra)
library(sf)

library(stars)
library(spatstat)
# library(spatstat.explore)
# library(spatstat.utils)

## ON USB STICK: D:\spatial_is_special\data\raw_data

# list the files
list.files(here::here("data/raw_data/"))

# load your sites (csv format) and check out the study area from the geopackage
sites <- read.csv(here::here("data/raw_data/sites.csv"), header=TRUE) ## this loads the csv file
head(sites, 6) ## this looks at the first rows of each column

# we have missing coordinates! Let us use only complete cases of Lat/Lon
sites <- sites[complete.cases(sites$LAT, sites$LON), ]   
# sites <- st_as_sf(sites, coords = c("LON", "LAT"), crs = 4326)

# and turn the dataframe into a spatial object (SF and spatvector)
sites <- vect(st_as_sf(sites, coords = c("LON", "LAT"), crs = 4326))
plot(sites, col="red", pch=4)   # plot them, have a look at the data!


## load some OSM data, we get it from here:
## +++++ General Use Citation +++++
## Please include the term 'geoBoundaries' with a link to
## https://www.geoboundaries.org

## +++++ Academic Use Citation +++++++++++
## Runfola D, Anderson A, Baier H, Crittenden M, Dowker E, Fuhrig S, et al. (2020)
## geoBoundaries: A global database of political administrative boundaries.
## PLoS ONE 15(4): e0231866. https://doi.org/10.1371/journal.pone.0231866.

## but it is provided in your summerschool data package you received
jor <- vect(here::here("data/raw_data/area/geoBoundaries-JOR-ADM0.shp"))

## check the coordinate ref. system (crs)
crs(jor) ## it is EPSG4326 code

# plot the data together in R base
plot(jor)
plot(sites, add=T, col="red")

## Q: Do we see anything?
## A: actually not, it the window of operation is far to big.
## S: we will reduce the window of operation for PPA

extent <- ext(sites)            ## we create an extent (spatial extent) of the sites
plot(extent)                    ## plot it
plot(sites, add=T, col="red")   ## we see that sites do fall on the edges of the extent, which is not good (-> edge effects)

## we will buffer the extent a bit to enlarge the window of operation
extent <- vect(extent)         ## turn the extent into a spatial object (spatvector)
crs(extent) <- "EPSG:4326"     ## assign the correct CRS
extent <- buffer(extent, 2000) ## buffer with 2000 m

## plot it again
plot(extent)
plot(sites, add=T, col="red", pch=1)

##################

# we will do spatial analysis and more precisely PPA (point pattern analysis) from the data.
# that means we will check out if there is any particular pattern in the data that we can observe and how the data is
# distributed within the window of operation and among the data itself. Eventually, we will check for so-called CSR,
# which is Complete Spatial Randomness of the data to detect clustering or regular behavior or random site distribution.
# there are multiple tools to do that and we will use the inhomogeneous K-Function (Ripley's K) for this.


## turn the sites into a ppp:
## Creates an object of class "ppp" representing a point pattern dataset in the two-dimensional plane.
## Because PPA is based on distances between the sites, we have to reproject our data into a meter-based (projected) coordinate ref. system
## We use epsg code 3857 (pseudo mercator) for this

sites <- project(sites, "epsg:3857")      ## project sites
sites <- st_as_sf(sites)                  ## turn the spatvector format into a SF-Object

sites_ppp <- spatstat.geom::as.ppp(sites) ## create a ppp-Object using the spatstat package
spatstat.geom::marks(sites_ppp) <- NULL   ## and set all additional information to NULL


## We also want to add the window of operation (extent) to the dataset
## we have to reproject the spatial object to the same crs:

extent <- project(extent, "epsg:3857")  ## reproject (you can also use: project(extent, sites)
window <- st_as_sf(extent)              ## and turn it into a SF Object


## Now we use spatstat again to assign the point pattern the correct extent of the window:

spatstat.geom::Window(sites_ppp) <- spatstat.geom::Window.im(window)

## check the format

sites_ppp  ## a point pattern: 118 points
## window: polygonal boundary

## good! Lets calculate a dirty density estimation to have a look:

data_sites_smooth <- spatstat.explore::density.ppp(sites_ppp)

## look at the results

plot(data_sites_smooth)
plot(sites_ppp, add=T, col="white")

## looks like there are clusters.... but we cant really identify them...
## As we have seen in the presentation (intro), the SIGMA/BANDWIDTH must be adjusted to
## understand the clustering behavior of the point pattern



## but are there actually clusters? Just looking at the data does not provide enough evidence to judge.
## We perform the K Function to check for CSR and identify IF there is clustering behavior or not

k_sites_est <- spatstat.explore::envelope(
  sites_ppp,
  fun = "Kest", # previously this was Kinhom, but that did not work as expected
  nsim = 99,
  correction = "best"
)

k_sites_inhom <- spatstat.explore::envelope(
  sites_ppp,
  fun = "Kinhom", # previously this was Kinhom, but that did not work as expected
  nsim = 99,
  sigma = 6000, 
  correction = "best"
)


#### now plot the results and evaluate.
par(mfrow = c(1, 2))
plot(k_sites_est,
     main = "K-est site distribution",
     sub = "nsim = 99",
     xlab = "r in m"
)

plot(k_sites_inhom,
     main = "K-inhom site distribution",
     sub = "nsim = 99",
     xlab = "r in m")

## Q: what do you see?


#### What can we do with the data now after checking for CSR and proposing clustered behavior?
#### KDE: kernel density estimation with adjusted sigma!

# we already have: sites_ppp (a spatstat spatial format)
# we already have: data_sites_smooth (a smoothed density of our sites, basically what we want but we want to control it!)
# we already have: spatstat.geom::Window(sites_ppp) (our window of operation)

## have a look again (copy from above)

plot(data_sites_smooth)
plot(spatstat.geom::Window(sites_ppp), add=T, border="white")
plot(sites_ppp, add=T, col="white")

## We explore the data a bit now using standard techniques:

## calculate the global density

global_density <- length(sites)/sum(st_area(window))

# Return global_density to our console

global_density  ## 6.932419e-09 [1/m^2]

## quadrant count

quadrat <- quadratcount(sites_ppp, nx = 10, ny = 10)

# inspect

plot(quadrat)

## run Chi Squared test for CSR:

quadrat.test(sites_ppp, nx = 10, ny = 10)

## We already plotted a density map, however, we didnt control the input variables.
## here is how we do it with custom variables, like radius of the kernel (sigma or bandwidth)

## simple KDE with bandwidth (sigma) = 200 m
## the same units as your CRS, in this case metres

plot(density.ppp(sites_ppp, sigma = 200))

# adjust the sigma!

plot(density.ppp(sites_ppp, sigma = 500))

# adjust the sigma!

plot(density.ppp(sites_ppp, sigma = 1000))

# adjust the sigma!

plot(density.ppp(sites_ppp, sigma = 2000))


## Q: what are the differences?
## Q: Which sigma would be more useful? (remember the K-inhom plot!!)

## here it is again:

par(mfrow = c(1, 2))

plot(k_sites,
     main = "K-inhom site distribution",
     sub = "nsim = 99",
     xlab = "r in m"
)

plot(density.ppp(sites_ppp, sigma = 1000))



#### If you want to continue, you can turn the result into a spatial object for visualisation (terra package)
#### turn it into a raster image using the terra package

KDE1000 <- rast(density.ppp(sites_ppp, sigma = 1000))

plot(KDE1000)

## check crs

crs(KDE1000)  ## aha! there is no CRS!

## assign CRS

crs(KDE1000) <- "EPSG:3857"

# and plot

dev.off()                                     ## turn off the R graphics
plot(KDE1000, main="site density")            ## plot the main raster
plot(vect(sites), add=T, col="red", pch=1)    ## plot the points in spatvector format and add them

#############################################################
#############################################################

#### this is part ONE, you succesfully explored your point pattern, created a Density Estimation and performed simple stats on the data

#############################################################
#############################################################

## now we continue with an underlying explanatory covariate

setwd("E:/Michael_ADATA_Festplatte/Projekte/BASEL_SNF/presentations/Brno_2024/DATA")

list.files()

dem <- rast("DEM_large.tif")

plot(dem)
plot(window, add=T)
plot(sites, add=T, col="red")

## that does not work!! (demn! <- silly joke)

crs(dem)
crs(window)    ## we can see: the coordinate systems do not match

## we must reproject the raster first

dem <- project(dem, window) # we use the window as spatial reference
crs(dem)

plot(dem)
plot(window, add=T)
plot(sites, add=T, col="red")

## we crop the DEM now to the extent of the study area:

dem <- crop(dem, extent)
dem <- mask(dem, extent)

plot(dem)
plot(window, add=T)
plot(sites, add=T, col="red")

## that worked!
## now lets check out the elevation model! here are some commands

range(dem)                           # looks at the range, min max values
hist(dem)                       # plot the data range
hist(dem, maxcell=ncell(dem))   # and use all values (can take very long if your raster is big)

barplot(dem)                       # looks at range
boxplot(dem)                       # idem
global(dem, c("mean"), na.rm=TRUE) # prints global mean of the raster




## lets start some terrain analyses
## e.g., slope and check out the results

slope <- terrain(dem, "slope")
plot(c(slope, dem))

## look at the data again

range(slope)
barplot(slope)

plot(slope)
plot(sites, add=T, col="black")




## we might be interested in how the points are actually located on our two rasters!
## let's check out the value at each point's location

P_slope <- extract(slope, sites)
hist(P_slope$slope, breaks=15)

P_elev <- extract(dem, sites)
P_elev
hist(P_elev$DEM_large, breaks=20)


## lets compare that to a random sample distribution to see if the sites behave differently!

# Sample 150 random points within the window of

points = sf::st_sample(window, size=150)   ## create 150 random points
points = vect(points)                      ## and turn them into a spatvector using terra

plot(dem)
plot(points, add=T)

P_ran <- extract(dem, points)            ## extract the dem signal at the location of each point
P_ran_slope <- extract(slope, points)    ## extract the slope signal at each location


## plot the sites info and the random points info:

par(mfrow = c(1, 2))
hist(P_ran$DEM_large, breaks=20)
hist(P_elev$DEM_large, breaks=20)

## Q: what do we see?


### use the Kolmogorov-Smirnov test to check if two samples are drawn from the same distribution

### 1) ELEVATION

ks.test(P_ran$DEM_large, P_elev$DEM_large)

par(mfrow=c(1,2))  ## this command just allows 2 plots on one page

qqnorm(P_ran$DEM_large, main="random")
qqline(P_ran$DEM_large)

qqnorm(P_elev$DEM_large, main="sites")
qqline(P_elev$DEM_large)

## The line gives a robust approach for estimating the parameters of the normal distribution
## Here: against a random distribution


### 2) Slope

ks.test(P_ran_slope$slope, P_slope$slope)   ## results are significant but at the edge....

par(mfrow=c(1,2))

qqnorm(P_ran_slope$slope, main="random")
qqline(P_ran_slope$slope)

qqnorm(P_slope$slope, main="sites")
qqline(P_slope$slope)

## export the dem to your local working directory

writeRaster(dem, "cropped_dem.tif", overwrite=TRUE)
writeRaster(slope, "cropped_slope.tif", overwrite=TRUE)




#################### next step: analyse the point distribution as a function of the covariate:

## we did this already above:

sites_ppp <- spatstat.geom::as.ppp(sites)
spatstat.geom::marks(sites_ppp) <- NULL
spatstat.geom::Window(sites_ppp) <- spatstat.geom::Window.im(window)


## now we want to add our covariate!
## first lets check again the raster

res(dem) ## [1] 104.6538 104.6538  we have a resolution of approximately 100 m

## we resample the DEM using the stars package, you can choose whatever you want, we just make it simple and
## make the resoltuion 100 m. Although: increasing the resolution is not adding information or enhancing the dem
## We simply want to have a smooth number now.

r_100 = st_as_stars(st_bbox(dem), dx = 100)
dem_100 = st_warp((st_as_stars(dem)), r_100)
dem_100 <- rast(dem_100)

plot(dem_100)
res(dem_100)


### now we use the rhohat function from the spatstat packages, which
### is an estimate of site intensity a a function of an underlying
### covariate (here= the elevation with 100x100 m)

# first, turn the raster into am im-format for spatstat ..we return to the stars format... (a pain... but spatstat does not go with terra...)

dem_100 <- st_as_stars(dem_100)
dem_im <- as.im(dem_100)
rhohat_sites_dem <- spatstat.explore::rhohat(sites_ppp, dem_im, method = "ratio")

plot(rhohat_sites_dem)


## lets do the same with slope!

## get the slope from the 100 x 100 m resolution

dem_100 <- rast(dem_100)
slope_100 <- terrain(dem_100, "slope")
plot(slope_100)


# and (annoying) turn it into a stars object for the im-format....
# (you can simplify this but now we are learning ;)

slope_100 <- st_as_stars(slope_100)
slope_im <- as.im(slope_100)
rhohat_sites_slope <- spatstat.explore::rhohat(sites_ppp, slope_im, method = "ratio")

plot(rhohat_sites_slope)



## plot both results:

par(mfrow=c(1,2))
plot(rhohat_sites_dem)
plot(rhohat_sites_slope)



## this little function now gives you an estimate to which extent your covariate is decisive
## as a underlying explanatory variable for the point pattern.


