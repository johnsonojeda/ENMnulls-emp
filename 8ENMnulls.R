##############################################################################
###                       RANDOMIZATION TESTS                             ####
##############################################################################
##' Run Bohl et al. 2019 randomization tests to evaluate if empirical models are
##' better than expected than random
##' Last updated: Dec 1,2022
##' 
#Loading the necessary packages
library(dplyr)
library(ENMeval)

##########################################
#1. SET DIRECTORIES & LOAD MODELS
##########################################
mod.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/Models"


#Cloud forest
CF <- file.path(paste(mod.dir, dir(mod.dir, pattern = "CF"), sep = "/"))
CF.mod <-lapply(grep("_args",CF, value = T, invert = T), readRDS) 
CF.modset <- lapply(grep("_args",CF, value = T), readRDS)

#Loading optimal model arguments table 
opt.args <- read.csv(paste(mod.dir, "ENM_OptArgs.csv", sep = "/"))

##########################################
#2. RUN ENMNULLS 
##########################################

#Cloud forest
CF.null <- ENMnulls(e = CF.mod[[1]], mod.settings = CF.modset[[1]],no.iter = 1000,
                    eval.stats = c("auc.val", "or.10p"), user.eval.type = "kspatial")

saveRDS(CF.null, "/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/ENMnulls/CFnull_AllPreds.rds")

mapply(function(e, m){
  null <- ENMeval::ENMnulls(e, m, no.iter, eval.stats = c("auc.val", "or.10p"), 
                            user.eval.type = "kspatial")
}, CF.mod, CF.modset)