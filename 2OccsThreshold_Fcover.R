##############################################################################
###             Occurrence point forest cover thresholding                 ###
##############################################################################

##' Here we're looking to remove occurrence points falling in areas below/above a forest cover threshold prior to building our veg ENMs

#loading the necessary R packages
library(dplyr)
library(raster)
library(rgdal)
library(rgeos)

##################################
## 1. UPLOADING ALL THE DATA
##################################
#loading veg type occurrence points drawn from the INEGI land cover polygons
occ.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/data/MexVeg/OccData/Full"

#Carso Huasteco
CF.occ <- lapply(file.path(occ.dir, dir(occ.dir,pattern = "CF")), read.csv) %>% 
  dplyr::bind_rows()

#loading the averaged 2016-2018 MODIS VCF raster 
VCF <- raster("/home/erica/PhD/ENMRandomization/ENMReal/data/MexVeg/CountryData/MODISVCF_cropped.grd")

#########################################
## 2. EXTRACT RASTER VALUES AT OCC POINTS
#########################################
#' Now we will get  the raster values at each occurrence point per veg type and 
#' remove points falling below specified forest cover threshold for each veg type

#lapply allows to use extract function  to every element in the list

occs.th <- function(occs, ras.th, file.name){
  #Converting occurrences to spatial points dataframe
  occs.xy <- SpatialPointsDataFrame(occs[2:3], occs, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  occs.xy <- spTransform(occs.xy, crs(ras.th))
  ForestCov<- extract(ras.th, occs.xy[2:3]) #cols 2 & 3 of each df represent long/lat
  fcov <- cbind(occs, ForestCov) #appending forest cover values to the occs df
  fcov <- if(unique(fcov$scientific_name) == "Cloud_forest"){ #removing points by cover threshold
    subset(fcov, fcov$ForestCov >= 60) #Cloud forest must have >=60% tree cover
  } 
  fcov <- fcov[1:3]
  write.csv(fcov, file.name, row.names = F)
  return(fcov)
}

occs.th(CF.occ, VCF, "/home/erica/PhD/ENMRandomization/ENMReal/data/MexVeg/OccData/VCF/CFocc_VCF.csv")