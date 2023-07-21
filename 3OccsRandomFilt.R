##############################################################################
###            VCF Occurrence point random filtering                       ####
##############################################################################

#' Here we will randomly remove a percentage of the forest cover thresholded 
#' occurrences to avoid issues with spatial autocorrelation in the downstream SVMs

#loading the necessary R packages
library(raster)
library(rgdal)
library(rgeos)
library(dplyr)

###################################
## 1. UPLOADING ALL THE OCC DATA
##################################
#loading veg type occurrence points drawn from the INEGI land cover polygons
occ.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/data/MexVeg/OccData/VCF"

#SMOr veg occs
veg.occ <- lapply(file.path(occ.dir,dir(occ.dir, pattern = ".csv")),read.csv)

###################################
## 2. RANDOMLY REMOVING % POINTS 
###################################

##'Function to randomly filter a percentage of occurrence points
##'@param occ dataframe of lat/long occurrence points
##'@param pct integer indicating percentage of points to be retained 


random.sample <- function(occ, pct, out.dir){
  require(dplyr)
  # set seed for reproducibility 
  set.seed(1234)
  #converting occs to tibble
  occ <- as_tibble(occ)
  #randomly filtering occurrence points
  if (length(pct) == 1){
    rand.filt <- slice_sample(occ, n=(nrow(occ)*pct))
    write.csv(rand.filt, paste(out.dir, paste0(unique(occ$scientific_name), (pct*100), ".csv"), 
                               sep = "/"), row.names = F)
  } else if (length(pct) > 1){
    rand.filt <- lapply(pct, function(x){
      rf<- slice_sample(occ, n = nrow(occ)*x)
      write.csv(rf, paste(out.dir, 
                          paste0(unique(occ$scientific_name), "_", (x*100), ".csv"),
                          sep = "/"))
      return(rf)
    }
    )}
  return(rand.filt)
    } 

#'selecting random samples for all veg types retaining 25%of all occ points.
out.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/data/MexVeg/OccData/RandFilt/test"

lapply(veg.occ, random.sample, pct = 0.25, out.dir = out.dir)
