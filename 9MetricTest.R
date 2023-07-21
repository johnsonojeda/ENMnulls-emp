##############################################################################
###                STATISTICALLY COMPARE MODEL ACCURACY                   ####
##############################################################################
##' Comparing model accuracy metrics for models built with different predictors
##' by extending methods developed by Bohl et al. 2019.
##' Last updated: Apr 22,2023
##' 
#Loading the necessary packages
library(dplyr)
library(ENMeval) #From nulls_anova branch

##########################################
#1. SET DIRECTORIES
##########################################
mod.dir <- "/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/Models"

#Loading optimal model arguments table 
opt.args <- read.csv(paste(mod.dir, "ENM_OptArgs.csv", sep = "/"))

#Cloud forest
CF <- file.path(paste(mod.dir, dir(mod.dir, pattern = "CF"), sep = "/"))
CF.mod <- grep("_args",CF, value = T, invert = T)
CF.mod <- lapply(c(CF.mod[2], CF.mod[3], CF.mod[4]), readRDS) 

CF.modset <- grep("_args",CF, value = T)
CF.modset <- lapply(c(CF.modset[2], CF.modset[3], CF.modset[4]), readRDS)

for(i in 1:length(CF.mod)){
       CF.mod[[i]]@partition.method <- "checkerboard2"
}

##########################################
#2. RANDOMIZATION TESTS 
##########################################

#Cloud forest
CF.anova.auc <-  ENMeval::ENMnulls_ANOVA(e.list = CF.mod,
                                         mod.settings.list = CF.modset, 
                                         eval.stats = "auc.val", 
                                         alternative = "greater",
                                         no.iter = 1000)


CF.anova.or <-  ENMeval::ENMnulls_ANOVA(e.list = CF.mod,
                                        mod.settings.list = CF.modset, 
                                        eval.stats = "or.10p", 
                                        alternative = "less",
                                        no.iter = 1000)

saveRDS(CF.anova.auc,"/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/Randomiz/CF_auc.rds")
saveRDS(CF.anova.or, "/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/Randomiz/CF_orNulls.rds" )

##########################################
### 3. EXPORTING RESULTS
##########################################

nulls_list <- c(CF.anova.auc, CF.anova.or)
out.dir2 <- "/home/erica/PhD/ENMRandomization/ENMReal/outputs/MexVeg/Randomiz/"

lapply(nulls_list, function(n){
  nulls <- n
  aov <- nulls$anova.nulls 
  m <- names(aov)
  
  #Exporting ANOVA tables
  aov <- lapply(aov, function(a){
    a <- as_tibble(a) %>% dplyr::mutate(species = rep(sp, nrow(.))) %>% 
      mutate(metric = rep(m, nrow(.)))%>% 
      dplyr::relocate(species, metric)
    
    #Exporting table of co-optimal model parameters
    if(file.exists(paste(out.dir2, "ANOVA_nulls.csv", sep = "/")) == F){
      write.csv(a, paste(out.dir2, "ANOVA_nulls.csv", sep = "/"), 
                row.names =  F)
    } else {
      write.table(a, paste(out.dir2, "ANOVA_nulls.csv", sep = "/"),
                  append = T, row.names = F, col.names = F, sep = ",")
    }
  })
  
  #Exporting post-hoc paired t-test
  pair <- nulls$pair.nulls %>% dplyr::mutate(species = rep(sp, nrow(.)))%>%
    relocate(species)
  
  if(file.exists(paste(out.dir2, "PostHoc_nulls.csv", sep = "/")) == F){
    write.csv(pair, paste(out.dir2, "PostHoc_nulls.csv", sep = "/"), 
              row.names =  F)
  } else {
    write.table(pair, paste(out.dir2, "PostHoc_nulls.csv", sep = "/"),
                append = T, row.names = F, col.names = F, sep = ",")
  }
  
  #Exporting empirical vs. null diffs
  emp.null <- ungroup(nulls$emp.nulls) %>% 
    dplyr::mutate(species = rep(sp, nrow(.)))%>%
    relocate(species)
  
  if(file.exists(paste(out.dir2, "EmpNulls.csv", sep = "/")) == F){
    write.csv(emp.null, paste(out.dir2, "EmpNulls.csv", sep = "/"), 
              row.names =  F)
  } else {
    write.table(emp.null, paste(out.dir2, "EmpNulls.csv", sep = "/"),
                append = T, row.names = F, col.names = F, sep = ",")
  }
  
  #Exporting null diff distributions
  null.diff <- nulls$nulls.diff 
  write.csv(null.diff, 
            paste(out.dir2, paste0(sp, "_NullsDiff.csv"), sep = "/"), row.names = F)
})
