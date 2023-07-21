##############################################################################
###                             PRE-PROCESS DATA                           ###
##############################################################################

##' Pre-processing spatial data to crop to extend of Carso Huasteco + Gran Sierra Plegada
##' and match resolutions/coordinate reference systems

#loading the necessary R packages
library(raster)
library(rgdal)
library(rgeos)

##################################
## 1. UPLOADING ALL THE DATA
##################################
#Sierra Madre Oriental (Carso Huasteco + Gran Sierra Plegada) shapefile
SMOr <- readOGR("/home/erica/PhD/ENMRandomization/data/MexVeg/CountryData/SMOr.shp", layer = "SMOr")

#CHELSA bioclimatic variables
env.dir <- "/home/erica/PhD/ENMRandomization/data/MexVeg/Env/raw"
chelsa <- raster::stack(file.path(env.dir, dir(env.dir, pattern = "CHELSA")))

#EarthEnv Cloud cover
env.dir <- "/home/erica/PhD/ENMRandomization/data/MexVeg/Env/raw"
cloud <- raster::stack(file.path(env.dir, dir(env.dir, pattern = "MODCF")))

#MODIS VCF 
VCF <- raster("/home/erica/PhD/ENMRandomization/data/MexVeg/CountryData/MODISVCF_2016-2018Avg.tif")

##################################
## 2. CROPPING DATA
##################################
#' Cropping spatial data to the extent of the Sierra Madre Oriental (SMOr)

#Chelsa bioclimatic variables
chelsa <- crop(chelsa, SMOr)
chelsa <- mask(chelsa, SMOr)
writeRaster(chelsa, paste("/home/erica/PhD/ENMRandomization/data/MexVeg/Env",
                          names(chelsa), sep= "/"),format = "raster", bylayer = T)
#EarthEnv Cloud cover
cloud <- crop(cloud, SMOr)
cloud <- mask(cloud, SMOr)
writeRaster(cloud, paste("/home/erica/PhD/ENMRandomization/data/MexVeg/Env",
                          names(cloud), sep= "/"),format = "raster", bylayer = T)

#MODIS VCF
#Projecting the SMOr shapefile to match MODIS VCF
SMOr<- spTransform(SMOr, CRS(projection(VCF)))

#Cropping the VCF layer to the extent of the SMOr
VCF <- crop(VCF, SMOr)
VCF<- mask(VCF, SMOr)
writeRaster(VCF, "/home/erica/PhD/ENMRandomization/data/MexVeg/CountryData/MODISVCF_cropped.grd", 
             format = "raster")