###################################################
# Initiate relevant paths
###################################################

fpath <- "../../ukbdocs/" 
fpath_updated <- "../../ukbdocs/updated_files/" 
opath <- "../../Results/" 

###################################################
# Initiate input files
###################################################
opt_out_list <- "UKB_optOutList_w27412_20200820.csv"
ifile_demographics <- "demographics_wBrainMRI.csv"
ifile_body1 <- "ukb40541_basked_200189_body_imaging.csv"
ifile_body2 <- "ukb42438_basket2008608_body_imaging2.csv"

ifile_diag_cancer <- "X20001_Diagnosis_coding.txt"
ifile_diag_psychiatric <- "X20002_Diagnosis_psychiatric.txt"
ifile_diag_noncancer <- "X20002_Excluded_diagnosis.txt"

###################################################
# Initiate packages required
###################################################

# install.packages("RGraphics", repos="file://tsd/shared/R/cran")
library(RGraphics)
library(ggplot2)
library(ggpubr)

###################################################
# Updated analyses 
###################################################

###################################################
# Init body MRI data frame 
###################################################

df.body1 <- read.csv(paste0(fpath_updated,ifile_body1), 
                     na.strings = c("NA", "")) # File used in manuscript
names(df.body1)
df.body2 <- read.csv(paste0(fpath_updated,ifile_body2), 
                     na.strings = c("NA", "")) # File used in manuscript
names(df.body2)

# Remove duplicated columns from the second data frame
ind <- names(df.body2) %in% names(df.body1)
is_dup <- names(df.body2)[ind]
is_dup <- is_dup[!is_dup %in% c("eid")]
df.body2 <- df.body2[,!names(df.body2) %in% is_dup]

df.body <- merge(df.body1, df.body2, by = "eid", all = TRUE)

###################################################
# Init basic demographic file
###################################################

# df.demo <- read.csv(paste0(fpath_updated,"ukb40502_basket_39315_demographics.csv"), na.strings = c("NA", ""))
# names(df.demo)
# 
# # Include only those participants that have Brain MRI
# names(df.demo)[names(df.demo) == "X12187.2.0"] <- "BrainMRI_measured"     # Indicates whether participants has brain MRI
#   
# # Remove those participants without brain MRI
# df.demo$BrainMRI_measured <- factor(df.demo$BrainMRI_measured)
# levels(df.demo$BrainMRI_measured)
# ind <- is.na(df.demo$BrainMRI_measured)
# df.demo <- df.demo[!ind,]
# 
# df.demo$BrainMRI_measured <- factor(df.demo$BrainMRI_measured)
# levels(df.demo$BrainMRI_measured)
# ind <- df.demo$BrainMRI_measured %in% c(0) # Extract those with direct BrainMRI measure
# df.demo <- df.demo[ind,]
# 
# # Dump to file subset with brain measures
# write.csv(df.demo, paste0(fpath_updated,"demographics_wBrainMRI.csv"), na = "", row.names = FALSE)

# Read demographic file with brain MRI data - work with smaller file for sake of time.
df.demo <- read.csv(paste0(fpath_updated,ifile_demographics), na.strings = c("NA", ""))
names(df.demo)

###################################################
# Init opt-out list
###################################################
# Exclude participants that have opt-out from the study
# None were excluded.
opt_out <- read.csv(paste0(fpath_updated,opt_out_list), 
                    header = FALSE)


####################
# Initiate imaging data
####################

####################
# Load area data
####################
impath <-paste0(fpath_updated,"FS_stats/regularFSstats","/")
df.lharea <- read.table(paste0(impath,"lh.area.UKBB.txt"), header = TRUE)
df.rharea <- read.table(paste0(impath,"rh.area.UKBB.txt"), header = TRUE)
df.area <- merge(df.lharea,df.rharea, by.x = "lh.aparc.area", 
                 by.y = "rh.aparc.area",all = FALSE)

####################
# Load thickness data
####################
df.lhthickness <- read.table(paste0(impath,"lh.thickness.UKBB.txt"), header = TRUE)
df.rhthickness <- read.table(paste0(impath,"rh.thickness.UKBB.txt"), header = TRUE)
df.thickness <- merge(df.lhthickness,df.rhthickness, 
                      by.x = "lh.aparc.thickness",
                      by.y = "rh.aparc.thickness",all = FALSE)


####################
# Load Volume data
####################
df.lhvolume <- read.table(paste0(impath,"lh.volume.UKBB.txt"), header = TRUE)
df.rhvolume <- read.table(paste0(impath,"rh.volume.UKBB.txt"), header = TRUE)
df.volume <- merge(df.lhvolume,df.rhvolume, 
                      by.x = "lh.aparc.volume",
                      by.y = "rh.aparc.volume",all = FALSE)

####################
# Load volumetric aseg data
####################
df.aseg <- read.table(paste0(impath,"subcorticalstats.UKBB.txt"),header = TRUE)
names(df.aseg)[names(df.aseg) == "Measure.volume"] <- "MRid"     



####################
# Load euler numbers
####################
df.euler <- read.csv(paste0(paste0(fpath_updated,"FS_stats/"),"allEuler_UKB.csv"))

# Output files
ofile_euler <- "../../Results/output/Euler_included_excluded.csv"
oimg_euler <- paste0("../../Results/pics/","Euler_included_excluded.pdf")



