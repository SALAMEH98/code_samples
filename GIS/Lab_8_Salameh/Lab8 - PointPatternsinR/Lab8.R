
##############################
# Lab 8. Points to Predictions
##############################

# Updated May 2023
# Compiled by M. Kolak and C. Bae with source material from:
# Lansley and Cheshire (2016): https://data.cdrc.ac.uk/tutorial/an-introduction-to-spatial-data-analysis-and-visualisation-in-r
# Kingi (2017): http://hautahi.com/rmaps
# And adapted to an sf framework.

##############################
# Environment Setup
##############################

# Set Working Directory
#setwd("~/Desktop/Lab8-CodeData")

library(sf)
library(tmap)
library(leaflet)
library(raster) # Needed for grid and kernel density surface
library(adehabitatHR) # Needed for kernel density surface

#install.packages("raster")

##############################
# Load and Join Data
##############################

# Load non-spatial Data (csv)
Census.Data <- read.csv("data/practicaldata.csv")
head(Census.Data)

# Load spatial data from shapefile
Output.Areas <- st_read("data/Camden_oa11.shp")
head(Output.Areas)

# Join our census data to the spatial object
OA.Census <- merge(Output.Areas, Census.Data, by.x = "OA11CD", by.y = "OA")
head(OA.Census)

# Load the house prices csv file
houses <- read.csv("data/CamdenHouseSales15.csv")
head(houses)

##############################
# Inspect and Prepare Data
##############################

# We only need a few columns for this practical
houses <- houses[,c(1,2,8,9)]
head(houses)

# Next create a House.Points spatial dataframe in simple features format (sf) using coordinates in columns 3 and 4
House.Points <- st_as_sf(houses, coords = c("oseast1m","osnrth1m"), crs = 27700)

# Plot to ensure the points were encoded correctly
plot(House.Points)

# Other exploration of the data
summary(House.Points$Price)

##############################
# Point Data Method: Basic Visualizations
##############################

# This plots a blank base map, we have set the transparency of the borders to 0.4
tm_shape(OA.Census) + tm_borders(alpha=.4)

# Create a color-coded dot map with tm_dots
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", palette = "Reds", style = "quantile")

# Rescale the points
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", scale = 1.5, palette = "Reds", style = "quantile", title = "Price Paid (£)")

# Turn on interactive Leaflet map view
tmap_mode("view")

# View interactive map; in Export, Save as Web Page
tm_shape(OA.Census) + tm_borders(alpha=.4) +
  tm_shape(House.Points) + tm_dots(col = "Price", scale = 1.5, palette = "Reds", style = "quantile", title = "Price Paid (£)")

# Turn off interactive view
tmap_mode("plot")

##############################
# Point Data Method: Graduated Symbol Visualizations
##############################

# Creates a graduated symbol map using tm_bubbles
tm_shape(OA.Census) + tm_borders(alpha =.4) + 
  tm_shape(House.Points) + tm_bubbles(size = "Price", col = "Price", palette = "Reds", style = "quantile", legend.size.show = FALSE, title.col = "Price Paid (£)") +
  tm_layout(legend.text.size = 1.1, legend.title.size = 1.4, frame = FALSE)

##############################
# Point Data Method: Graduated Symbol with Overlay Visualizations
##############################

# Creates a graduated symbol map with overlay choropleth
tm_shape(OA.Census) + tm_fill("Unemployed", alpha = 0.8, palette = "Greys", style = "quantile", title = "% Unemployed") + 
  tm_borders(alpha=.4) + 
  tm_shape(House.Points) + tm_bubbles(size = "Price", col = "Price", palette = "PuRd", style = "quantile", legend.size.show = FALSE, title.col = "Price Paid (£)", border.col = "black", border.lwd = 0.1, border.alpha = 0.1) +
  tm_layout(legend.text.size = 0.8, legend.title.size = 1.1, frame = FALSE)

##############################
# Point Data Method: Buffer Generation
##############################

# Create 200m buffers for each house point
house_buffers <- st_buffer(House.Points, 200)

# Map in tmap
tm_shape(OA.Census) + tm_borders() +
  tm_shape(house_buffers) + tm_borders(col = "blue") +
  tm_shape(House.Points) + tm_dots(col = "red")

##############################
# Point Data Method: Buffer Count per Area
##############################

# Count buffers within each area; generates a vector of totals
count_buffers <- lengths(st_within(OA.Census, house_buffers))
head(count_buffers)

# Stick buffer totals back to the census master file
OA.Census <- cbind(OA.Census, count_buffers)
head(OA.Census)

# Map density of buffers per census area
tm_shape(OA.Census) + tm_fill(col = "count_buffers", palette = "BuGn", style = "quantile", title = "Housing Density")

##############################
# Point Data Method: Buffer Union (or Dissolve)
##############################

# Merge the buffers and view plot
union.buffers <- st_union(house_buffers)

# Map housing buffers
tm_shape(OA.Census) + tm_borders() +
  tm_shape(union.buffers) + tm_fill(col = "blue", alpha = .4) + tm_borders(col = "blue") +
  tm_shape(House.Points) + tm_dots(col = "red")

##############################
# Point Data Method: Group Attribute by Area
##############################

# Point in Polygon (PIP)
# Gives the points the attributes of the polygons that they are in
pip <- st_join(House.Points, OA.Census, join = st_within)
head(pip)

# Aggregate average house prices by the OA11CD (OA names) column
OA <- aggregate(pip$Price, by = list(pip$OA11CD), mean)
head(OA)

# Change the column names of the aggregated data
names(OA) <- c("OA11CD", "Price")

# Join the aggregated data back to the OA.Census polygon
OA.Census <- merge(OA.Census, OA, by = "OA11CD", all.x = TRUE)

# Map mean housing price per area
tm_shape(OA.Census) + tm_fill(col = "Price", style = "quantile", title = "Mean House Price (£)")

# Think about patterns you see here, compared to in the previous plots.


##############################
# Point Data Method: Group by Grid
##############################

# Convert sf objects to sp objects for raster and adehabitate packages
OA.Census.sp <- sf:::as_Spatial(OA.Census)
House.Points.sp <- sf:::as_Spatial(House.Points)

# Make a Grid:
# First, define boundaries or extent of grid using bounding box
grid.extent <- extent(bbox(OA.Census.sp))

# Next, make a raster object from the extent
grid.raster <- raster(grid.extent)

# Specify our dimensions; split extent into 30 x 30 cells
dim(grid.raster) <- c(30,30)

# Project the grid using our shapefile CRS
projection(grid.raster) <- CRS(proj4string(OA.Census.sp))

# Convert into a spatial data frame
grid.sp <- as(grid.raster, 'SpatialPolygonsDataFrame')

# Convert spatial data frame to matrix data structure
grid.matrix <- grid.sp[OA.Census.sp, ]

# Aggregate housing prices by grid matrix
OA.grid <- aggregate(x=House.Points.sp["Price"], by = grid.matrix, FUN = mean)

# If there is a null value, assign 0
OA.grid$Price[is.na(OA.grid$Price)] <- 0

# Inspect data range
summary(OA.grid@data)

# Map
tm_shape(OA.grid) + tm_fill(col = "Price", style = "quantile", n = 7, title = "Mean House Price (£)")

# Compare this to the previous map we've created by output area. Reflect on how this compares to lecture material and what it would mean to change the grid size.


##############################
# Point Data Method: Kernel Density Surface
##############################

# Runs the kernel density estimation; look up the function parameters for more options
kde.output <- kernelUD(House.Points.sp, h = "href", grid = 1000)

plot(kde.output)

# Converts to raster
kde <- raster(kde.output)

# Sets projection to British National Grid
projection(kde) <- CRS("+init=EPSG:27700")

# Maps the raster in tmap, "ud" is the density variable
tm_shape(kde) + tm_raster("ud")

# Creates a bounding box based on the extent of the Output.Areas polygon
bounding_box <- bbox(OA.Census.sp)

# Maps the raster within the bounding box
tm_shape(kde, bbox = bounding_box) + tm_raster("ud")

# Mask the raster by the output area polygon
masked_kde <- mask(kde, Output.Areas)

# Maps the masked raster, also maps white output area boundaries
tm_shape(masked_kde, bbox = bounding_box) + tm_raster("ud", style = "quantile", 
                                                      n = 100, 
                                                      legend.show = FALSE, 
                                                      palette = "YlGnBu") +
  tm_shape(Output.Areas) + tm_borders(alpha=.3, col = "white") +
  tm_layout(frame = FALSE)

# Compute homeranges for 75%, 50%, 25% of points, objects are returned as spatial polygon data frames
range95 <- getverticeshr(kde.output, percent = 95)
range75 <- getverticeshr(kde.output, percent = 75)
range50 <- getverticeshr(kde.output, percent = 50)
range25 <- getverticeshr(kde.output, percent = 25)
range05 <- getverticeshr(kde.output, percent = 05)

# Code below creates a map of several layers using tmap
kernel_map1 <- tm_shape(Output.Areas) + tm_fill(col = "#cccccc") + 
  tm_borders(alpha=.8, col = "black") +
  tm_shape(House.Points) + tm_dots(col = "blue") +
  tm_shape(range95) + tm_borders(alpha=.7, col = "#ffffb2", lwd = 2) + 
  tm_fill(alpha=.15, col = "#ffffb2") +
  tm_shape(range75) + tm_borders(alpha=.7, col = "#fecc5c", lwd = 2) + 
  tm_fill(alpha=.15, col = "#fecc5c") +
  tm_shape(range50) + tm_borders(alpha=.7, col = "#fd8d3c", lwd = 2) + 
  tm_fill(alpha=.15, col = "#fd8d3c") +
  tm_shape(range25) + tm_borders(alpha=.7, col = "#f03b20", lwd = 2) + 
  tm_fill(alpha=.15, col = "#f03b20") +
  tm_shape(range05) + tm_borders(alpha=.7, col = "#bd0026", lwd = 2) + 
  tm_fill(alpha=.15, col = "#bd0026") +
  tm_layout(frame = TRUE, title = "Housing Density in the Borough of Camden", 
            title.position = c("left", "bottom"))

# Write your kernel density surface to raster format

writeRaster(masked_kde, filename = "kernel_density.grd")

