rm(list=ls())

working_directory <- "/tsd/p33/home/p33-tirilg/Documents/Projects/2019-UKbio/scripts/analysis"
setwd(working_directory)

initial_submission <- FALSE

# Load data
source("help_functions/init.R")

# Initialize funtions
source("help_functions/Effect_size_estimate.R")
source("help_functions/get_functions.R")
source("help_functions/process_data.R")
source("help_functions/linear_regression_functions.R")
source("help_functions/plot_functions.R")
source("help_functions/create_demo.R")
source("help_functions/fdr_threshold.R")
source("help_functions/CreateTables.R")
source("help_functions/my_ggseg.R")

#############################################################
# Initialize data
#############################################################
df.in <- initialize_data(ipath,ifile,  #"UKB_BodyBrain.CLEANED.csv",
                         brainVars,anthrometricVars,
                         bodyMRIVars,initial_submission)

# Do not process for ICV effects
brainVars <- brainVars[!brainVars %in% c("EstimatedTotalIntraCranialVol")]

#############################################################
# Generate demographic information & plots
#############################################################

# Full sample
#########################
create_demographics(df.in, paste0(dpath, "Demographics.csv"))

# Supplemental figures - distributions
plots_demographics(paste0(dpath,"demographics_plots.pdf"),df.in, bodyMRI = FALSE)
plots_brain_MRI(paste0(dpath,"FigS4_brain_density_plots.pdf"),df.in, sfill = "sex")

for(val in anthrometricVars){
  plots_brain_MRI_body_composition_scatter_lm(paste0(ppath,"FigureXX_body_brain_",val),df.in,
                                              val,anthrometricVars,subsample = FALSE)
}

# Create correlation plots - required R 4.0 on our system
# create_corr_plots(df.in, paste0(dpath,"corrplot_qgraph_all.pdf"), fullsample = TRUE)

# Subsample with body MRI
#########################
ind <- complete.cases(df.in[,append(anthrometricVars,bodyMRIVars)])
create_demographics(df.in[ind,], paste0(dpath, "Demographics_bodyMRI.csv"))

# Supplemental figures - distributions
plots_demographics(paste0(dpath,"demographics_plots_bodyMRI.pdf"),df.in[ind,], bodyMRI = TRUE)
plots_brain_MRI(paste0(dpath,"FigS5_brain_density_plots_bodyMRI.pdf"),df.in[ind,], sfill = "sex")

for(val in append(anthrometricVars,bodyMRIVars)){
  plots_brain_MRI_body_composition_scatter_lm(paste0(ppath,"FigureXX_body_brain_",val),df.in,
                                              val,append(anthrometricVars,bodyMRIVars),
                                              subsample = TRUE)
}

# Create correlation plots - required R 4.0 on our system
# create_corr_plots(df.in[ind,], paste0(dpath,"corrplot_qgraph_subsample.pdf"),fullsample = FALSE)

#############################################################
# Sample description - Body MRI processing 
#############################################################

# full sample
process_body_MRI(df.in, anthrometricVars, "Body_anthrometric_All1", 
                 lm_model = lm_model1, opath = opath, ppath = ppath)
process_body_MRI(df.in, anthrometricVars, "Body_anthrometric_All2", 
                 lm_model = lm_model2, opath = opath, ppath = ppath)
process_body_MRI(df.in, anthrometricVars, "Body_anthrometric_All3", 
                 lm_model = lm_model3, opath = opath, ppath = ppath)

# Sample with body MRI
process_body_MRI(df.in, append(anthrometricVars,bodyMRIVars),"Body_MRI_Subset1", 
                 lm_model = lm_model1, opath = opath, ppath = ppath)
process_body_MRI(df.in, append(anthrometricVars,bodyMRIVars),"Body_MRI_Subset2", 
                 lm_model = lm_model2, opath = opath, ppath = ppath)
process_body_MRI(df.in, append(anthrometricVars,bodyMRIVars),"Body_MRI_Subset3", 
                 lm_model = lm_model3, opath = opath, ppath = ppath)

#####################################################################################
# Sample description - Process brain measures for age and sex associations
#####################################################################################

process_brain_MRI_age_sex(df.in, brainVars, lm_model = lm_model1,
                          ofile = "_age_sex_ALL1", multi_center = TRUE)

process_brain_MRI_age_sex(df.in, brainVars, lm_model = lm_model2,
                          ofile = "_age_sex_ALL2", multi_center = TRUE)

#####################################################################################
# Process brain measures for associations with individual body composition parameters
#####################################################################################

#####################################################################################
############### Anthropometric measures##############################################
#####################################################################################
process_brain_body_MRI(df.in, brainVars, anthrometricVars.c, lm_model = lm_model2,
                       ofile = "_anthrometric_ALL1",
                       sexInteraction = FALSE, multi_center = TRUE,
                       poly_regressor = FALSE, opath = opath, ppath = ppath)
process_brain_body_MRI(df.in, brainVars, anthrometricVars.c, lm_model = lm_model2,
                       ofile = "_anthrometric_ALL2",
                       sexInteraction = FALSE, multi_center = TRUE,
                       poly_regressor = TRUE, opath = opath, ppath = ppath)
process_brain_body_MRI(df.in, brainVars, anthrometricVars.c, lm_model = lm_model3,
                       ofile = "_anthrometric_ALL3",
                       sexInteraction = FALSE, multi_center = TRUE,
                       poly_regressor = TRUE, opath = opath, ppath = ppath)

# Follow-up Left / Right model 2c
process_brain_body_MRI(df.in, leftBrainVars, anthrometricVars.c, lm_model = lm_model3,
                       ofile = "_LEFT_anthrometric_ALL3",
                       sexInteraction = FALSE, multi_center = TRUE,
                       poly_regressor = TRUE, opath = opath, ppath = ppath)

process_brain_body_MRI(df.in, rightBrainVars, anthrometricVars.c, lm_model = lm_model3,
                       ofile = "_RIGHT_anthrometric_ALL3",
                       sexInteraction = FALSE, multi_center = TRUE,
                       poly_regressor = TRUE, opath = opath, ppath = ppath)



#####################################################################################
############### Body MRI measures####################################################
#####################################################################################

process_brain_body_MRI(df.in, brainVars, append(anthrometricVars.c,bodyMRIVars.c), lm_model = lm_model2,
                       ofile = "_bodyMRI_subsample1",
                       sexInteraction = FALSE, multi_center = FALSE,
                       poly_regressor = FALSE, opath = opath, ppath = ppath)
process_brain_body_MRI(df.in, brainVars, append(anthrometricVars.c,bodyMRIVars.c), lm_model = lm_model2,
                       ofile = "_bodyMRI_subsample2",
                       sexInteraction = FALSE, multi_center = FALSE,
                       poly_regressor=TRUE, opath = opath, ppath = ppath)
process_brain_body_MRI(df.in, brainVars, append(anthrometricVars.c,bodyMRIVars.c), 
                       lm_model = lm_model3,
                       ofile = "_bodyMRI_subsample3",
                       sexInteraction = FALSE, multi_center = FALSE,
                       poly_regressor =TRUE, opath = opath, ppath = ppath)

# Left / Right model 2c
process_brain_body_MRI(df.in, leftBrainVars, append(anthrometricVars.c,bodyMRIVars.c), 
                       lm_model = lm_model3,
                       ofile = "_LEFT_bodyMRI_subsample3",
                       sexInteraction = FALSE, multi_center = FALSE,
                       poly_regressor = TRUE, opath = opath, ppath = ppath)

process_brain_body_MRI(df.in, rightBrainVars, append(anthrometricVars.c,bodyMRIVars.c), 
                       lm_model = lm_model3,
                       ofile = "_RIGHT_bodyMRI_subsample3",
                       sexInteraction = FALSE, multi_center = FALSE,
                       poly_regressor = TRUE, opath = opath, ppath = ppath)

################################################################################
# Plot density plots of cortical analyses
################################################################################

# Density plots - data now shown in manuscript. 
# Demographic related / density plots
# hemi <- c("lh","rh")
# cmeasure <- c("thickness", "area", "volume")
# for(l in hemi)
# {
#   for(cm in cmeasure){
#     plots_brain_MRI_followup(paste0(dpath,"FigSxx_brain_density_plots_",l,"_",cm,".pdf"),df.in, 
#                              "sex",l, cm, cortical_parcellation)
#   }
# }
# 
# # Brain MRI subset
# ind <- complete.cases(df.in[,append(anthrometricVars,bodyMRIVars)])   
# for(l in hemi)
# {
#   for(cm in cmeasure){
#     plots_brain_MRI_followup(paste0(dpath,"FigSxx_brain_density_plots_subset_",l,"_",cm,".pdf"),df.in[ind,], 
#                              "sex",l, cm, cortical_parcellation)
#   }
# }

################################################
# Conduct cortical analyses
################################################
# Full sample analyses - model no . 3
hemi <- c("lh","rh")
cmeasure <- c("thickness", "area", "volume")
wICV <- TRUE
for(l in hemi){
  for(cm in cmeasure){
    if(cm == "thickness"){
      wICV = FALSE
    }else{
      wICV = TRUE
    }
    # Process cortical parcellation
    process_brain_body_MRI(df.in, paste0(l,"_",cortical_parcellation,"_",cm), anthrometricVars.c, 
                           lm_model = lm_model3, ofile = paste0("_",l,"_",cm,"_ALL3"),
                           sexInteraction = FALSE, multi_center = TRUE,
                           poly_regressor = TRUE, opath = opath, ppath = ppath, wICV = wICV)
    process_brain_body_MRI(df.in, paste0(l,"_",cortical_parcellation,"_",cm), 
                           append(anthrometricVars.c,bodyMRIVars.c), 
                           lm_model = lm_model3, ofile = paste0("_",l,"_",cm,"bodyMRI_subsample_ALL3"),
                           sexInteraction = FALSE, multi_center = FALSE,
                           poly_regressor =TRUE, opath = opath, ppath = ppath, wICV = wICV)
  }
}

##########################
# Extract min / max - cortical parcellation
##########################

# Figure out min/max r value across parcellations
measures <- c("thickness","area","volume")
hemisphere <- c("lh","rh")
r_values <- NULL
for(m in measures){
  for(h in hemisphere){
    for(cm in anthrometricVars.c){
      tmp <- get_df_hemi(paste0(opath,"brain_body/RegressorOfInterest/"),m,cm,"_ALL3.csv", h)
      r_values <- append(r_values,tmp$r_full)
      r_values <- append(r_values,tmp$r_full_2)
    }
    for(cm in append(anthrometricVars.c,bodyMRIVars.c)){
      tmp <- get_df_hemi(paste0(opath,"brain_body/RegressorOfInterest/"),m,cm,"bodyMRI_subsample_ALL3.csv", h)
      r_values <- append(r_values,tmp$r_full)
      r_values <- append(r_values,tmp$r_full_2)
    } 
  }
}
print("Min/Max r-values for cortical parcellations")
print(c(min(r_values),max(r_values)) )

######################################################################################
# Plot parcellations
######################################################################################

# flims <- c(min(r_values),max(r_values)) 
# ggseg_combined(ppath, paste0(opath,"brain_body/RegressorOfInterest/"), 
#                anthrometricVars.c, "_ALL3.csv",  flims)
# 
# ggseg_combined(ppath, paste0(opath,"brain_body/RegressorOfInterest/"), 
#                append(anthrometricVars.c,bodyMRIVars.c), "bodyMRI_subsample_ALL3.csv",  flims)

#############################################################
# Plot results
# #############################################################
# plot_results(df.in,opath,ppath,anthrometricVars,bodyMRIVars,
#              anthrometricVars.c, bodyMRIVars.c, lm_model = lm_model3)

# Plot supplemental figures not created in prior functions. 
# plot_results_supplement(df.in,opath,ppath,anthrometricVars,bodyMRIVars,
#              anthrometricVars.c, bodyMRIVars.c, lm_model = lm_model3)

