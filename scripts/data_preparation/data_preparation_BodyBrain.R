rm(list = ls())

working_directory <- "/net/tsd-evs.tsd.usit.no/p33/home/p33-tirilg/Documents/Projects/2019-UKbio/scripts/data_preparation/"
setwd(working_directory)

initial_submission <- FALSE

source("help_functions.R")
source("exclude_functions.R")
source("prepare_data_functions.R")

df.body <- NULL
if(initial_submission){
  source("init_initial_submission.R")
  df.body <- prepare_body_MRI_data(df.body)
}else{
  source("init_updated_analyses.R")
  df.body <- prepare_body_MRI_data_updated(df.body)
}

df.demo <- prepare_demographic_data(df.demo, opt_out, opath,
                                    paste0(fpath,ifile_diag_cancer),      # "X20001_Diagnosis_coding.txt"
                                    paste0(fpath,ifile_diag_noncancer),   # "X20002_Excluded_diagnosis.txt"
                                    paste0(fpath,ifile_diag_psychiatric), # "X20002_Diagnosis_psychiatric.txt"
                                    paste0(opath,"output/Excluded_non_cancer_diagnosis.csv"))

df.img <- NULL
if(initial_submission){
  df.img <- prepare_imaging_data(df.area, df.thickness, df.aseg)
}else{
  df.img <- prepare_imaging_data_updated(df.area, df.thickness, df.aseg, df.volume)
}

df.euler <- prepare_euler(df.euler)

###########################
# Combine data frames
###########################
df.out <- merge(df.demo, df.img,by = "eid", all = FALSE)

Nbrain <- nrow(df.out)
print("No participants with brain MRI: ")
print(Nbrain)

df.out  <- merge(df.out, df.body, by="eid",all.x = TRUE)
NbodyBrain <- nrow(df.out)

ind <- df.out$HasBody & !is.na(df.out$HasBody)
print("Participants with body-MRI:")
print(nrow(df.out[ind,]))

#############################################################
#Remove participants that do not pass quality control for brain MRI
#############################################################

# Include euler numbers
df.out <- merge(df.out, df.euler,by = "eid", all = FALSE)

print("Remaining participants after merging with euler numbers: ")
print(nrow(df.out))

# Remove participants exceeding 3 standard deviations from the mean
df.out <- exclude_based_on_euler_number(df.out, 3, ofile_euler, oimg_euler, fpath)

print("No participants after exlucding based on Euler numbers: ")
print(nrow(df.out))

#############################################################
# Write cleaned data file to file
#############################################################
write.csv(df.out,paste0(fpath,"/","UKB_BodyBrain.CLEANED.csv"), row.names = FALSE, na="")

print("Participants in study:")
print(nrow(df.out))

ind <- df.out$HasBody & !is.na(df.out$HasBody)
print("Participants with body-MRI:")
print(nrow(df.out[ind,]))



