##############################################################################
###                          ENM THRESHOLDING                             ####
##############################################################################

#' Applying MTP and 10 pct thresholds the continuous ENMs for each vegetation type 

#loading the necessary R packages
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)

#######################################
## 1. UPLOADING ENM raster predictions
#######################################

#specifying file paths for Raster predictions 

#Continuous ENMs projected to the SMOr & Gran Sierra Plegada regions
enm.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/Rasters"

#Bioclimatic variables only
enm.bio <- file.path(enm.dir, dir(enm.dir, pattern = "Bioclim_pj"))
enm.bio <- raster::stack(enm.bio)

#Bioclimatic variables + cloud cover
enm.biocloud <- file.path(enm.dir, dir(enm.dir, pattern = "BioCloud_pj"))
enm.biocloud <- raster::stack(enm.biocloud)

#Cloud cover only
enm.cloud <- file.path(enm.dir, dir(enm.dir, pattern = "_Cloud_pj"))
enm.cloud <- raster::stack(enm.cloud)

######################################
# 2. CREATING A TRHESHOLDING FUNCTION
######################################
#loading file with all calculated MTP and 10 pct thresholds per veg type/region

thresh <- as_tibble(read.csv("/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/Models/ENM_thresholds.csv"))

#Reclassifying so values under the calculated threshold for each veg type is NA
##' @param enm a raster object of enm predictions 
##' @param th.tbl a dataframe with mtp & 10pct threshold values for a region/veg type


ENM.thresh <- function(enm, preds, th.tbl, out.dir) {
  CF <- subset(enm, which(grepl("CF_", names(enm)) == TRUE))
  
  mtp <- as_tibble(th.tbl) %>% select(Veg.type, Predictors, MTP)
  
  pct10 <- as_tibble(th.tbl) %>% select(Veg.type, Predictors, pct10)
  
  #MTP thresholds
  mtp.CF <- as.numeric(th.tbl %>% 
                         filter(Veg.type == "Cloud forest" & Predictors == "Bioclim") %>% 
                         select(MTP))
  #10 Pct thresholds
  pct10.CF <- as.numeric(th.tbl %>% 
                           filter(Veg.type == "Cloud forest" & Predictors == "Bioclim") %>% 
                           select(pct10))
  
 #Thresholding the predictions
 # Mininum training presence 
  CF.mtp <- CF
  CF.mtp[CF.mtp < mtp.CF] <- NA

  # 10 percentile 
  CF.10pct <- CF
  CF.10pct[CF.10pct < pct10.CF] <- NA
  
  
  names(CF.mtp) <- paste0(names(CF.mtp), "_mtp")
  
  names(CF.10pct) <- paste0(names(CF.10pct), "_10pct")
  
raster::writeRaster(CF.mtp, filename = file.path(out.dir, names(CF.mtp)),
            format = "GTiff", overwrite = T) 
  
raster::writeRaster(CF.10pct, filename = names(file.path(out.dir,CF.10pct)),
                    format = "GTiff", overwrite = T) 
  
  return(list(CF.mtp, CF.10pct))
  
}

######################################
# 3. THRESHOLDING  ENMs
######################################

#Obtaining the thresholded rasters for the continuous ENMs 

#Bioclimatic only
Bio.th<- ENM.thresh(enm.bio, th.tbl = thresh, preds = "Bioclim", out.dir = enm.dir)

#Bioclimatic only + cloud cover
Biocloud.th <- ENM.thresh(enm.biocloud, preds = "BioCloud", th.tbl = thresh, out.dir = enm.dir)

#Cloud cover only
Cloud.th <- ENM.thresh(enm.cloud, preds = "Cloud", th.tbl = thresh, out.dir = enm.dir)