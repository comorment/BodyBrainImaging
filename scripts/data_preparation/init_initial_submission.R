###################################################
# Initiate relevant paths
###################################################

fpath <- "/net/tsd-evs.tsd.usit.no/p33/home/p33-tirilg/Documents/Projects/2019-UKbio/ukbdocs/"
opath <- "/net/tsd-evs.tsd.usit.no/p33/home/p33-tirilg/Documents/Projects/2019-UKbio/Results/"

###################################################
# Initiate packages required
###################################################

# install.packages("RGraphics", repos="file://tsd/shared/R/cran")
library(RGraphics)
library(ggplot2)
library(ggpubr)

###################################################
# Initial submission
###################################################

###################################################
# Init body MRI data frame 
###################################################

# Read data from basket 2000151 - whole body datafile
df.body <- read.csv(paste0(fpath,"ukb26346.csv"), na.strings = c("NA", "")) # File used in manuscript
names(df.body)

###################################################
# Init basic demographic file
###################################################
 
# # Read data from basket 10335
# #df.demo <- read.csv(paste0(fpath,"ukb24843.csv"), na.strings = c("NA", "")) # This is the old file from before May 2019.
# df.demo <- read.csv(paste0(fpath,"ukb29266.csv"), na.strings = c("NA", "")) # This is the new file from May 2019.
# names(df.demo)
# 
# # Include only those participants that have Brain MRI
# names(df.demo)[names(df.demo) == "X12187.2.0"] <- "BrainMRI_measured"     # Indicates whether participants has brain MRI
#  
# # Remove those participants without brain MRI
# df.demo$BrainMRI_measured <- factor(df.demo$BrainMRI_measured)
# ind <- is.na(df.demo$BrainMRI_measured)
# df.demo <- df.demo[!ind,]
#  
# levels(df.demo$BrainMRI_measured)
# ind <- df.demo$BrainMRI_measured == 0 # Extract those with direct BrainMRI measure
# 
# df.demo <- df.demo[ind,]
#  
# # Dump to file subset with brain measures
# write.csv(df.demo, paste0(fpath,"demographics_wBrainMRI.csv"), na = "", row.names = FALSE)
df.demo <- read.csv(paste0(fpath,"demographics_wBrainMRI.csv"), na.strings = c("NA", ""))
names(df.demo)

###################################################
# Init opt-out list
###################################################
# Exclude participants that have opt-out from the study
# None were excluded.
opt_out <- read.csv(paste0(fpath,"w27412_20181016_optOutList.csv"), 
                    header = FALSE)


####################
# Initiate imaging data
####################

####################
# Load area data
####################
df.lharea <- read.table(paste0(fpath,"lh.area.UKBB.txt"), header = TRUE)
df.rharea <- read.table(paste0(fpath,"rh.area.UKBB.txt"), header = TRUE)
df.area <- merge(df.lharea,df.rharea, by.x = "lh.aparc.area", 
                 by.y = "rh.aparc.area",all = FALSE)

####################
# Load thickness data
####################
df.lhthickness <- read.table(paste0(fpath,"lh.thickness.UKBB.txt"), header = TRUE)
df.rhthickness <- read.table(paste0(fpath,"rh.thickness.UKBB.txt"), header = TRUE)
df.thickness <- merge(df.lhthickness,df.rhthickness, 
                      by.x = "lh.aparc.thickness",
                      by.y = "rh.aparc.thickness",all = FALSE)

####################
# Load volumetric aseg data
####################
df.aseg <- read.table(paste0(fpath,"subcorticalstats.UKBB.txt"),header = TRUE)
names(df.aseg)[names(df.aseg) == "Measure.volume"] <- "MRid"     

####################
# Load euler numbers
####################
df.euler <- read.csv(paste0(fpath,"allEuler_UKB.csv"))

# Output files
ofile_euler <- "../../Results/output/Euler_included_excluded.csv"
oimg_euler <- paste0("../../Results/pics/","Euler_included_excluded.pdf")



# For updated analyses upon review
# df.body <- read.csv(paste0(fpath,"may2020/ukb40541.csv"), na.strings = c("NA", "")) # Updated file from may 2020 used for application.



