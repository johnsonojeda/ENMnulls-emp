##############################################################################
###                   EXTRACT PERMUTATION IMPORTANCE                      ####
##############################################################################
##' Generate permutation importance tables for models built with different predictors
##' Last updated: Dec 12,2022
##' 
#Loading the necessary packages
library(dplyr)
library(ENMeval)

##########################################
#1. SET DIRECTORIES
##########################################
mod.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/Models"
out.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/VarImp"

#Loading optimal model arguments table 
opt.args <- read.csv(paste(mod.dir, "ENM_OptArgs.csv", sep = "/"))

##########################################
#2. EXTRACT VARIABLE IMPORTANCE TABLES 
##########################################
#' @param sp character. Species for which variable importance tables should be extracted
#' @param mod.dir filepath to ENMeval model objects
#' @param opt.args table with optimal model arguments 
#' @param out.dir directory where variable importance tables should be saved

var_imp <- function(sp, mod.dir, opt.args, out.dir){
  require(tidyverse)
  
  #loading model objects & selecting optimal models
  mod.path <- file.path(mod.dir, dir(mod.dir, pattern = sp))
  mod.path <- grep("args", mod.path, value = TRUE, invert = TRUE)
  
  var.imp.tbl <- lapply(mod.path, function(x){
    #specifying predictor variable set
    if(grepl("Bioclim.rds", x)){
      pred <- "Bioclim"
    }else if(grepl("Biocloud.rds", x)){
      pred <- "Biocloud"
    }else if(grepl("Cloud.rds", x)){
      pred <- "Cloud"
    }
    
    #specifying model tuning arguments of optimal model
    args <- opt.args %>% filter(scientific_name == sp, predictors == pred)
    args <- args$tune.args
    
    #loading ENMevaluation object (model)
    m <- readRDS(x)
    
    #Selecting optimal model & extracting variable importance values
    var.imp <- m@models[[args]]@results %>% data.frame(.) %>% 
      rownames_to_column("Variable")
    #changing column names
    colnames(var.imp) <- c("Variable", "permutation.importance")
    
    var.imp <- var.imp %>% 
      filter(grepl("permutation.importance", Variable))%>%
      mutate(Variable = str_split(Variable, "[.]")%>% sapply("[", 1)) %>%
      mutate(predictors = rep(pred, nrow(.))) %>% 
      
    return(var.imp)
  })
  
  var.imp.tbl <- bind_rows(var.imp.tbl)%>% 
    pivot_wider(names_from = predictors, values_from = permutation.importance)%>%
    arrange(desc(AllPreds))
  
  write.csv(var.imp.tbl, paste(out.dir, paste0(sp, "_VarImp.csv"), sep = "/"))
  
  return(var.imp.tbl)
}

# Apply function to Cloud forest and Pine-oak forest models
varImp <- var_imp("CF", mod.dir = mod.dir, opt.args = opt.args, 
       out.dir = out.dir)