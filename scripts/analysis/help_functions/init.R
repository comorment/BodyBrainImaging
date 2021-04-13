############################################################################
# Help functions
############################################################################
center_variable <- function(var){
  value <- var - mean(var, na.rm = TRUE)
  return(value)
}

############################################################################
# Load packages
############################################################################
# install.packages("ggthemes", repos="file://tsd/shared/R/cran")
# install.packages("ggforce", repos="file://tsd/shared/R/cran")
# install.packages("ggraph", repos="file://tsd/shared/R/cran")
# install.packages("qgraph", repos="file://tsd/shared/R/cran")
# install.packages("igraph", repos="file://tsd/shared/R/cran")
# install.packages("cluster", repos="file://tsd/shared/R/cran")
# install.packages("lattice", repos="file://tsd/shared/R/cran")
# install.packages("reshape", repos="file://tsd/shared/R/cran")
# install.packages("ggpubr", repos="file://tsd/shared/R/cran")
# install.packages("ggfortify", repos="file://tsd/shared/R/cran")
# install.packages("gridExtra", repos="file://tsd/shared/R/cran")
# install.packages("gtable", repos="file://tsd/shared/R/cran")
# install.packages("moments", repos="file://tsd/shared/R/cran")
# 
# # Do I still need these packages
# install.packages("curl", repos="file://tsd/shared/R/cran")
# # install.packages("tsne", repos="file://tsd/shared/R/cran")
# # install.packages("pcaMethods", repos="file://tsd/shared/R/bioconductor/")
# # install.packages("tidyverse", repos="file://tsd/shared/R/cran")
# install.packages("pheatmap", repos="file://tsd/shared/R/cran")
# install.packages("corrplot", repos="file://tsd/shared/R/cran")
# install.packages("factoextra", repos="file://tsd/shared/R/cran")
# install.packages("cowplot", repos="file://tsd/shared/R/cran")
# install.packages("dplyr", repos="file://tsd/shared/R/cran")
# install.packages("tidyr", repos="file://tsd/shared/R/cran")

# GGseg stuff
# .libPaths("/tsd/p33/data/durable/R-packages")
# install.packages("viridis", repos="file://tsd/shared/R/cran")
library(viridis)
library(ggseg, lib.loc = "/tsd/p33/data/durable/R-packages")
library(dplyr)
library(tidyr)

# library(pheatmap)
# library(gtable)
library(moments)
library(ggfortify)
library(gridExtra)
# library(factoextra)
library(corrplot)
library(ggpubr)
library(lattice)
library(reshape)
library(ggplot2)
library("ggthemes")
library("moments")
# qgraph required R 4.0 on our system
# library(ggforce)
# library(qgraph)
# library(ggraph)
# library(igraph)



############################################################################
# Set paths & ifile
############################################################################

ipath <<- "../../ukbdocs"
dpath <<- "../../Results/demographics/"
opath <<- "../../Results/output/"
ppath <<- "../../Results/pics/"

ifile <- "UKB_BodyBrain.CLEANED.csv"

############################################################################
# Create the basic linear model
############################################################################
lm_model1 <- "poly(age.c,2)+sex" 
lm_model2 <- "poly(age.c,2)*sex" 
lm_model3 <- "poly(age.c,2)*sex + Ethnicity + diabetic.2.0 + 
                   high_cholesterol.2.0+hypertension.2.0+ smoking.2.0 + alcohol.2.0"

############################################################################
# Create the basic body and brain vectors
############################################################################
anthrometricVars <- c("waist_circumference.2.0","WHR.2.0","BMI.2.0")
bodyMRIVars <- c("TTMV.2.0","VAT.2.0","ASAT.2.0","Liver_PDFF.2.0","VAT_ASAT.2.0", "MFI.2.0")

anthrometricVars.c <- anthrometricVars
for(ii in 1:length(anthrometricVars.c)){
  anthrometricVars.c[ii] <- gsub("2.0","c",anthrometricVars.c[ii])
}

bodyMRIVars.c <- bodyMRIVars
for(ii in 1:length(bodyMRIVars.c)){
  bodyMRIVars.c[ii] <- gsub("2.0","c",bodyMRIVars.c[ii])
}

brainVars <- c("MeanCorticalThickness","MeanWhiteSurfArea","CortexVol","Total.Cerebellum.Cortex","Brain.Stem",
               "CorticalWhiteMatterVol","Total.Cerebellum.White.Matter",
               "CSF","Total.Lateral.Ventricle","X3rd.Ventricle",
               "Total.Thalamus.Proper","Total.Hippocampus","Total.Amygdala",
               "Total.Accumbens.area","Total.Caudate",
               "Total.Putamen","Total.Pallidum","EstimatedTotalIntraCranialVol")

leftBrainVars <- c("lh_MeanThickness_thickness","lh_WhiteSurfArea_area",
                   "lhCortexVol","Left.Cerebellum.Cortex",
                   "lhCorticalWhiteMatterVol","Left.Cerebellum.White.Matter",
                   "Left.Lateral.Ventricle",
                   "Left.Thalamus.Proper","Left.Hippocampus","Left.Amygdala",
                   "Left.Accumbens.area","Left.Caudate",
                   "Left.Putamen","Left.Pallidum")
rightBrainVars <- c("rh_MeanThickness_thickness","rh_WhiteSurfArea_area",
                   "rhCortexVol","Right.Cerebellum.Cortex",
                   "rhCorticalWhiteMatterVol","Right.Cerebellum.White.Matter",
                   "Right.Lateral.Ventricle",
                   "Right.Thalamus.Proper","Right.Hippocampus","Right.Amygdala",
                   "Right.Accumbens.area","Right.Caudate",
                   "Right.Putamen","Right.Pallidum")


cortical_parcellation <- c("bankssts", "caudalanteriorcingulate", "caudalmiddlefrontal", "cuneus",  
                           "entorhinal", "fusiform","inferiorparietal", "inferiortemporal",        
                           "isthmuscingulate","lateraloccipital", "lateralorbitofrontal","lingual", 
                           "medialorbitofrontal", "middletemporal","parahippocampal","paracentral", 
                           "parsopercularis","parsorbitalis","parstriangularis","pericalcarine",           
                           "postcentral","posteriorcingulate","precentral","precuneus",
                           "rostralanteriorcingulate",  "rostralmiddlefrontal", "superiorfrontal",
                           "superiorparietal", "superiortemporal", "supramarginal",
                           "frontalpole", "temporalpole", "transversetemporal", "insula")


############################################################################
# Initialize the actual data
############################################################################
initialize_data <- function(ipath,ifile, 
                            brain_vars,
                            anthrometric_vars,
                            bodyMRI_vars, 
                            initial_submission = TRUE){
  
  ############################################################################
  # Load data
  ############################################################################
  df <- read.csv(paste0(ipath,"/",ifile))
  head(df)
  
  ############################################################################
  # Convert to factor
  ############################################################################
  cfactors <- c("sex","Ethnicity","diabetic.2.0","high_cholesterol.2.0",
                "hypertension.2.0","alcohol.2.0","smoking.2.0","Assessment_centre.2.0")
  for(var in cfactors){
    df[,var] <- factor(df[,var])
  }
  
  ############################################################################
  # If not initial submission, update liver PDFF measure to AMRA version
  ############################################################################
  if(!initial_submission){
    df$Liver_PDFF.2.0 <- df$Liver_PDFF_AMRA.2.0
    df$VAT_ASAT.2.0 <- df$myVat_ASAT.2.0
  }
  
  ############################################################################
  # Center variables
  ############################################################################
  df$ICV   <- center_variable(df[,"EstimatedTotalIntraCranialVol"])
  df$age.c <- center_variable(df[,"age.2.0"])

  cvars <- c(bodyMRI_vars,anthrometric_vars)
  for(var in cvars){
    ovar <- paste0(gsub("2.0","",var),"c") 
    df[,ovar] <- df[,var]
    df[,ovar] <- center_variable(df[,ovar])
  }
  # print(head(df))
  
  ############################################################################
  # Convert to ml
  ############################################################################
  tmp <- brain_vars[!brain_vars %in% c("MeanCorticalThickness","MeanWhiteSurfArea")]
  for(t in tmp){
    ovar <- paste0(t,"_ml") 
    df[,ovar] <- df[,t]/1000
  }
  df[,"MeanWhiteSurfArea_scaled"] <- df[,"MeanWhiteSurfArea"]/1000
    
  return(df)
}

