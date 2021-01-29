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

#clear
rm(list=ls())

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
rcoast[] <- 1

iocean <- rcoast
iocean <- which(rcoast[] == 1) #we want the cell IDs for ocean cells

#iocean <- which(rland[] == 1) #we want the cell IDs for ocean cells
#oceandf <- data.frame(xyFromCell(rland, iocean), 
 #                    cellID = iocean)

oceandf <- data.frame(xyFromCell(rcoast, iocean), 
                      cellID = iocean)

#oceandf <- data.frame(xyFromCell(rcoast, iocean), 
                      #cellID = iocean)

nocean <- length(iocean)
npp <- nrow(pour_pts2) #number of pour points
#'Preallocation' so R knows ahead of time memory size it 
# will need to store our data
distmat <- matrix(NA, nrow = npp, ncol = nocean)

for (ipp in 1:length(pour_pts2)){
  xy <- st_coordinates(pour_pts2[ipp,])
  icell <- cellFromXY(rland, xy)
  rtemp <- rcoast
  rtemp[rland[] > 0] <- NA
  rtemp[icell] <- 2
  
  #generate distance matrix
  # Griddistance to find distance to everywhere in ocean from cells = 2 (pour points)
  
  #buffer idea..
  # buffdist <- 3 #make a more sensible number, e.g. 10,000m? 
  # cropext <- extent(c(xy[1] - buffdist, xy[1] + buffdist,
  #                   xy[2] - buffdist, xy[2] + buffdist))
  # rtemp2 <- crop(rtemp, cropext)
  # plot(rtemp2)
  
  #finding cell numbers
  # rcellnums <- raster(rtemp)
  # rcellnums[iocean] <- iocean
  # rcellnums_crop <- crop(rcellnums, cropext)
  #TB continued.... 
  #then make sure you use rtemp2 below! 
  
  #This might be heinously slow because it is calculating distance to every where in 
  # the entir ocean. If it is, come back to me adn we can buffer/crop
  gdist_sed <- gridDistance(rtemp, origin = 2,  omit = NA)
  
  #plot(gdist_sed)
  #double check gdist_sed is a raster? 
  distmat[ipp, ] <- gdist_sed[iocean]/1000 #convert to km so its
  # consistent with my 2017 paper
  
  #then if we used cropping above, do this instead:
  # distmat[ipp, ] <- gdist_sed[iocean]/1000 #convert to km so its
  
  # grid.list[[ipp]] <- as.data.frame(gdist_sed)
  rm(rtemp)

}


x <- distmat #this is distmat
b <- matrix(pour_pts2$Acc_sed, nrow = 26) #this is the sediment influence for each PP
alpha <- -2.3 #from Brown et al. 2017 Table 1, north coast
#this matrix algebra gives us the cumulative sediment at every cell. 
cumulative_sed <- (t(x) ^ alpha ) %*% b
#So this is just matrix algebra shorthand for:
#multiple each row of b by all values of x in the 
# corresponding row of x
#then sum columns of x
#cumulative_sed
#once you have 'cumulative sed' then plot it:
rsed <- raster(rcoast)
rsed[iocean] <- cumulative_sed
plot(rsed)



##CB code writing and comments 

# iocean 
#so we know which columsn of 'distmat' correspond 
# to which cells!
#distmat, which has the pour points distances for every pour point and every ocean cell
# distmat is a matrix where rows are pour points, columns are ocean cells
# values in the matrix are distance of that cell to that pour point. 

#explanation of distmat and getting 'sediment' totals:
#first I make up distmat and sed influence 
#x <- matrix(runif(10), nrow = 2, ncol = 5) #say this is distmat (2 pour points)
#b <- matrix(runif(2), nrow = 26) #this is the sediment influence for each PP

#alpha <- -2.3 #from Brown et al. 2017 Table 1, north coast
#this matrix algebra gives us the cumulative sediment at every cell. 
cumulative_sed <- (t(x) ^ alpha ) %*% b
#So this is just matrix algebra shorthand for:
#multiple each row of b by all values of x in the 
# corresponding row of x
#then sum columns of x
cumulative_sed
#notice first value is same as: 
b[1]* (x[1,1]^alpha) + b[2]*(x[2,1]^alpha)

#once you have 'cumulative sed' then plot it:
rsed <- raster(rland)
rsed[iocean] <- cumulative_sed
plot(rsed)


# turn list of grid distance dataframes into an array
# NOTE:: Set the dimensions from number of rows, columns, and grid.dist dfs in list
# in example below, this is number of rows = 14, number of columns = 5, number of dfs in list = 3

#grid.array <- array(as.numeric(unlist(grid.list)), dim=c(1483976, 26, 3)) #dont think these are correct. Ask CB.... 

#save as a raster - writeraster 
#writeRaster(gdist_sed, file="gdist_sed_11_1_21.tif", format = "GTiff") 

#Need to create a distance matrix that has the distances to every gridded cell from each pour point










