##############################################################################
###                           ENM DIFFERENCES                             ####
##############################################################################

#' Calculating differences in suitability values between bioclim-only models and: 
#' a) bioclim + cloud cover; b) cloud cover-only models.

#loading the necessary R packages
library(raster)

############################################
## 1. Uploading thresholded ENM predictions
############################################
enm.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/Rasters"

CF.mtp <- raster::stack(file.path(enm.dir, grep("mtp", dir(enm.dir), value = T)))


######################################
# 2. Calculate ENM differences
######################################

enm.dif <- function(pred1, pred2, enm, veg.type = c("CF"), out.dir){
  
  enm <- subset(enm, which(grepl(veg.type, names(enm)) == TRUE))
  
  p1 <- subset(enm, which(grepl(pred1, names(enm)) == TRUE))
  p2 <- subset(enm, which(grepl(pred2, names(enm)) == TRUE))
  
  dif <- p2 - p1
  names(dif) <- paste(veg.type, pred1,"v", pred2, sep = "_")
  
  raster::writeRaster(dif, file.path(out.dir, paste0(names(dif), ".tif")), 
                      format = "GTiff")
}
  

CF.dif <- lapply(c("BioCloud", "_Cloud"), enm.dif, pred1 = "Bioclim", enm = CF.mtp, 
                 veg.type= "CF", out.dir = enm.dir)