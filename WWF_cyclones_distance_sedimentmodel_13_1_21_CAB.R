####WWF Cyclones Chapter#### 
#### Date: 5/1/21 edited by CAB 
###### By: Alyssa Giffin
###### Purpose: Distance matrix for the sediment runoff model 

library(raster)
library(igraph)
library(sf)
library(rgdal)
library(RColorBrewer)
library(rgeos)
library(fasterize)
library(dplyr)
library(ggplot2)

#projection
fiji_utm <- st_crs("+proj=utm +zone=60 +south +ellps=intl +towgs84=265.025,384.929,-194.046,0,0,0,0 +units=m +no_defs")

###Data inputs 
#land raster already in the same projection and resolution so no need to projectraster
rland <- raster("WWF Cyclones/GIS/sediment/LAND_FIJI_resample_3_12_20_Reprojected.tif")
#pour points - river opening - sediment values added to each point. Checked the resulting grid cell below and they seem ok land wise.
pour_pts <- st_read("WWF Cyclones/GIS/sediment/pour_points_11_1_21.shp")
pour_pts2 <- st_transform(pour_pts, crs=fiji_utm)
#coastline points
coastline_points <- st_read("WWF Cyclones/GIS/sediment/coastline_points_PU3_verified_21_12_20.shp")
coastline_points2 <- st_transform(coastline_points, crs=fiji_utm)
#gdistsed_all <- raster("gdist_sed_11_1_21.tif")

#reclassify values of each raster and shapefile 
rcoast <- rland
rcoast[] <-1
#npp <- length(pour_pts2)#same length as pour pts file

grid.list <- list()# list for storing dataframes

for (ipp in 1:length(pour_pts2)){
xy <- st_coordinates(pour_pts2[ipp,])
icell <- cellFromXY(rland, xy)
rcoast[rland[] > 0] <- NA
rcoast[icell] <- 2

#generate distance matrix
# Griddistance to find distance to everywhere in ocean from cells = 2 (pour points)
gdist_sed <- gridDistance(rcoast, origin = 2,  omit = NA)
#plot(gdist_sed)

grid.list[[ipp]] <- as.data.frame(gdist_sed)
}

# turn list of grid distance dataframes into an array
# NOTE:: Set the dimensions from number of rows, columns, and grid.dist dfs in list
# in example below, this is number of rows = 14, number of columns = 5, number of dfs in list = 3

grid.array <- array(as.numeric(unlist(grid.list)), dim=c(1483976, 26, 3)) #dont think these are correct. Ask CB.... 

#save as a raster - writeraster 
#writeRaster(gdist_sed, file="gdist_sed_11_1_21.tif", format = "GTiff") 

#Need to create a distance matrix that has the distances to every gridded cell from each pour point










