

library(malariaAtlas)


# Types de données spatiales

##############

#la prévalence du paludisme au burkina Fasp
d <- getPR(country = "Burkina Faso", species = "Pf")
ggplot2::autoplot(d)


# Modèles de points

library(sparr)
data(pbc)
plot(unmark(pbc[which(pbc$marks == "case"), ]), main = "cases")
axis(1)
axis(2)
title(xlab = "Easting", ylab = "Northing")
plot(unmark(pbc[which(pbc$marks == "control"), ]),
     pch = 3, main = "controls")
axis(1)
axis(2)
title(xlab = "Easting", ylab = "Northing")




# devtools::install_github("Paula-Moraga/SpatialEpiApp")
library(SpatialEpiApp)
library(sf)
library(ggplot2)
library(viridis)

# map
f <- "SpatialEpiApp/data/Ohio/fe_2007_39_county/fe_2007_39_county.shp"
pathshp <- system.file(f, package = "SpatialEpiApp")
map <- st_read(pathshp, quiet = TRUE)

# data
namecsv <- "SpatialEpiApp/data/Ohio/dataohiocomplete.csv"
d <- read.csv(system.file(namecsv, package = "SpatialEpiApp"))

# data are disaggregated by gender and race
# aggregate to get population in each county and year
d <- aggregate(x = d$n, by = list(county = d$NAME, year = d$year),
               FUN = sum)
names(d) <- c("county", "year", "population")

# join map and data
mapst <- dplyr::left_join(map, d, by = c("NAME" = "county"))

# map population by year
# facet_wrap() splits data into subsets and create multiple plots
ggplot(mapst, aes(fill = log(population))) + geom_sf() + 
  facet_wrap(~ year, ncol = 7) +
  scale_fill_viridis("log(population)") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())



library(terra)
pathraster <- system.file("ex/elev.tif", package = "terra")
r <- rast(pathraster)
plot(r)


r <- rast(ncol = 10, nrow = 10,
          xmin = -150, xmax = -80, ymin = 20, ymax = 60)
r

library(terra)
# Raster (SpatRaster)
r <- rast(system.file("ex/elev.tif", package = "terra"))
# Polygons (SpatVector)
v <- vect(system.file("ex/lux.shp", package = "terra"))
points <- crds(centroids(v))
plot(r)
plot(v, add = TRUE)
points(points)



# data frame with the coordinates
points <- as.data.frame(points)
valuesatpoints <- extract(r, points)
cbind(points, valuesatpoints)



# Average raster values by polygon
v$avg <- extract(r, v, mean, na.rm = TRUE)$elevation
# Area-weighted average raster values by polygon (weights = TRUE)
v$weightedavg <- extract(r, v, mean, na.rm = TRUE,
                         weights = TRUE)





library(ggplot2)
library(tidyterra)

# Plot average raster values within polygons
ggplot(data = v) + geom_spatvector(aes(fill = avg)) +
  scale_fill_terrain_c()

# Plot area-weighted average raster values within polygons
ggplot(data = v) + geom_spatvector(aes(fill = weightedavg)) +
  scale_fill_terrain_c()




devtools::install_github("FlowmapBlue/flowmapblue.R")
library(flowmapblue)


locations <- data.frame(
  id = c(1, 2, 3),
  name = c("New York", "London", "Rio de Janeiro"),
  lat = c(40.713543, 51.507425, -22.906241),
  lon = c(-74.011219, -0.127738, -43.180244)
)

flows <- data.frame(
  origin = c(1, 2, 3, 2, 1, 3),
  dest = c(2, 1, 1, 3, 3 , 2),
  count = c(42, 51, 50, 40, 22, 42)
)
flowmapblue(locations, flows,
            clustering = TRUE, darkMode = TRUE, animation = FALSE)






library(spData)
library(sf)
library(mapview)
map <- st_read(system.file("shapes/boston_tracts.shp",
                           package = "spData"), quiet = TRUE)
map$vble <- map$MEDV
mapview(map, zcol = "vble")



# Neighbors
library(spdep)
nb <- poly2nb(map, queen = TRUE) # queen shares point or border
nbw <- nb2listw(nb, style = "W")

# Global Moran's I
gmoran <- moran.test(map$vble, nbw,
                     alternative = "greater")
gmoran