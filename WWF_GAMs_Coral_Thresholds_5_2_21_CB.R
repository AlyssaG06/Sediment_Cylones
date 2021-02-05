#### WWF Cyclones Chapter #####
#### by: Alyssa 
#####Date: 5/2/21
#####Purpose: GAm's for coral sediment thresholds


#####Library####
library(dplyr)
library(tmap)
library(sf)
library(raster)
library(tidyr)

#######read in data ####
benthic_GSR <- read.csv("Data/great_sea_reef_benthic_data_WWF.csv", header = TRUE)
sediment_GSR <- raster("GIS/sediment/sediment_runoff_29_1_21.tif")
sediment_GSR <- projectRaster(sediment_GSR, crs=crs('+init=EPSG:32760'))
Benthic_GSR_pts <- st_read("GIS/Benthic_GSR/benthic_SGR_points_projected_5_2_21.shp")

###summarise####
#summarise total points per transect (for each site)
points_transect <- benthic_GSR %>% 
  group_by(.dots =c("Site","Transect_number"))%>%
  summarise(sum_points = sum(Point, na.rm = TRUE))

#summarise points per habitat per transect (this instance its 'benthic attributes') for each site
habitat_ben_at <- benthic_GSR %>% 
  group_by(.dots =c("Site","Transect_number", "Latitude", "Longitude", "Benthic_attribute" "Benthic_attribute"))%>%
  summarise(sum_points = sum(Point, na.rm = TRUE))

#summarise points per habitat per transect (this instance its 'benthic attributes') for each site
habitat_ben_cat <- benthic_GSR %>% 
  group_by(.dots =c("Site","Transect_number", "Latitude", "Longitude", "Benthic_category"))%>%
  summarise(sum_points = sum(Point, na.rm = TRUE))

#summarise points per habitat per transect  (this instance 'Growth form')
habitat_ben_grow <- benthic_GSR %>% 
  group_by(.dots =c("Site","Transect_number", "Growth_form"))%>%
  summarise(sum_points = sum(Point, na.rm = TRUE))

##creating a better data.frame (e.g hard coral, macroalage coloumns)
benthic_GSR_update<-habitat_ben_cat %>% pivot_wider(names_from = Benthic_category, 
                                                    values_from = sum_points)

benthic_GSR_update[is.na(benthic_GSR_update)] <- 0

benthic_GSR_update2 <- benthic_GSR_update%>% mutate(uncol_sed= Rubble + Sand)%>%mutate(algae = Macroalgae + `Turf algae`)

# Dplyr remove multiple columns by name:
benthic_GSR_update3 <- dplyr::select(benthic_GSR_update2, c(Transect_number, Latitude, Longitude, Site, `algae`, `Hard coral`, `uncol_sed`))

##convert benthic_GSR_update3 to spatial points (SF)


### intersect turbidity with spatial point benthic_GSR_update3 from step above



#####write the model




##CB coding - think changed above
datsites <- select(benthic_GSR, Site, Transect_number, Latitude, Longitude)%>%
distinct()
nrow(datsites)

#Now extract TSS at datsites
#Then I'd join that to datjoined <- left_join(habitat_ben_grow, datsites)


#T
dat_hard_coral <- datjoined %>% filter(Growth_form)

m1 <- gam(branching_coral ~ s(TSS) + s(site_code, bs="re"), family = poisson, method = "REML")



m1 <- gam(Bra)