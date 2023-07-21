##############################################################################
###           Create Occurrence points from land cover polygons           ####
##############################################################################

##' This script is to generate occurrence records for vegetation type based on the INEGI land cover polygons 

#Loading the necessary packages
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)

##########################################
#STEP 1: loading all the data 
##########################################

#Specifying the directories where the vegetation shapefiles and Chelsa rasters are located. 
veg.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/data/MexVeg/CountryData/Veg"
env.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/data/MexVeg/Env"

#creating lists of files for
veg.list <- file.path(veg.dir, dir(veg.dir, pattern = ".shp$"))
env.list <- file.path(env.dir, dir(env.dir, pattern = ".tif$"))

#reading in vegetation shapefiles
veg.shp <- lapply(veg.list, function(x){
  a<- raster::shapefile(x)
  return(a)
})

veg.shp <- raster::bind(veg.shp[[1]], veg.shp[[2]])

#loading the environmental data 
env <- raster::stack(env.list)

##############################################
#STEP 2: Creating occ points for each veg type
##############################################

# Rasterizing the veg polygons using Chelsa bioclim layers as a template
veg.ras <- raster::stack(unlist(lapply(veg.shp, 
                                       raster::rasterize, env[[1]])))
names(veg.ras) <- "CF"

# Now converting the veg rasters to spatial points for my occurrences
veg.occ <- raster::rasterToPoints(veg.ras, spatial = T)

# Converting the points to a spatial points dataframe with lat/long columns
veg.occ.xy <- tbl_df(cbind(veg.occ@coords, veg.occ@data)) %>% 
  rename(scientific_name = name, longitude = x, latitude = y) 

#Exporting occ data for each veg type individually
CF.occ <- veg.occ.xy %>% filter(!is.na(CF)) %>% 
  mutate(scientific_name = rep("Cloud_forest", nrow(.))) %>%
  select(scientific_name, longitude, latitude)

write.csv(CF.occ, "/home/erica/PhD/ENMRandomization/ENMReal/data/OccData/FullOccs/CarsoHuasteco/CF_Occ.csv")