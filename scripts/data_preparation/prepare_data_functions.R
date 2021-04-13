#################################################################
# Perform data cleaning of demographic file
#################################################################
prepare_demographic_data <- function(df, opt_out, opath, 
                                     ifile_x20001_coding, # Cancer diagnosis
                                     ifile_x20002_nonpsychiatric, 
                                     ifile_x20002_psychiatric,
                                     ofile_noncancer){
  print("No. participants prior to excluding opt-out participants: ")
  print(nrow(df))
  
  # Find location of participants that have opt-out - none were excluded
  ind <- df$eid %in% opt_out$V1
  
  print("Number of opt-out participants: ")
  print(nrow(df[ind,]))
  
  # Only include participants that have not opt-out of the study.
  df <- df[!ind,]
  
  print("No. participants after excluding opt-out participants: ")
  print(nrow(df))
  
  explore_psychiatric_illness(df, opath)
  # source("explore_psychiatric_illness.R")
  print("N Participants prior to removing based on diagnosis")
  print(nrow(df))
  df <- exclude_cancer_diagnosis(df, ifile_x20001_coding) 
  df <- exclude_noncancer_diagnosis(df,ifile_x20002_nonpsychiatric, ofile_noncancer)
  df <- exclude_psychiatric_diagnosis(df,ifile_x20002_psychiatric) 
  print("N Participants after removing based on diagnosis")
  print(nrow(df))
   
  names(df)[names(df) == "X31.0.0"]    <- "sex"   	# Sex component
  names(df)[names(df) == "X34.0.0"]    <- "birth.year" 	# Year of birth
  names(df)[names(df) == "X52.0.0"]    <- "birth.month" 	# Month of birth
  names(df)[names(df) == "X54.0.0"]    <- "Assessment_centre.0.0"   # Assessment centre
  names(df)[names(df) == "X54.1.0"]    <- "Assessment_centre.1.0"   # Assessment centre
  names(df)[names(df) == "X54.2.0"]    <- "Assessment_centre.2.0"   # Assessment centre
  
  names(df)[names(df) == "X21003.0.0"] <- "age.0.0" 	# Age when attended assessment centre
  names(df)[names(df) == "X21003.1.0"] <- "age.1.0" 	# Age when attended assessment centre
  names(df)[names(df) == "X21003.2.0"] <- "age.2.0" 	# Age when attended assessment centre
  # names(df)[names(df) == "X40000.0.0"] <- "date.of.death.0.0"	# Date of death
  # names(df)[names(df) == "X40000.1.0"] <- "date.of.death.1.0"	# Date of death
  # names(df)[names(df) == "X40000.2.0"] <- "date.of.death.2.0"	# Date of death
  # names(df)[names(df) == "X42000.0.0"] <- "date.of.myocardinal.infarction.0.0"   
  names(df)[names(df) == "X20117.0.0"] <- "alcohol_drinker.0.0" 	# Alcohol drinker status
  names(df)[names(df) == "X20117.1.0"] <- "alcohol_drinker.1.0" 	# Alcohol drinker status
  names(df)[names(df) == "X20117.2.0"] <- "alcohol_drinker.2.0" 	# Alcohol drinker status
  names(df)[names(df) == "X50.0.0"]    <- "height.0.0"	# Standing height
  names(df)[names(df) == "X50.1.0"]    <- "height.1.0"	# Standing height
  names(df)[names(df) == "X50.2.0"]    <- "height.2.0"	# Standing height
  names(df)[names(df) == "X21002.0.0"] <- "weight.0.0"	# Weight
  names(df)[names(df) == "X21002.1.0"] <- "weight.1.0"	# Weight
  names(df)[names(df) == "X21002.2.0"] <- "weight.2.0"	# Weight
  df$BMI.0.0 			<- compute_BMI(df$weight.0.0, df$height.0.0)
  df$BMI.1.0 			<- compute_BMI(df$weight.1.0, df$height.1.0)
  df$BMI.2.0 			<- compute_BMI(df$weight.2.0, df$height.2.0)
  names(df)[names(df) == "X48.0.0"]    <-"waist_circumference.0.0"
  names(df)[names(df) == "X48.1.0"]    <-"waist_circumference.1.0"
  names(df)[names(df) == "X48.2.0"]    <-"waist_circumference.2.0"
  names(df)[names(df) == "X49.0.0"]    <- "hip_circumference.0.0" 	
  names(df)[names(df) == "X49.1.0"]    <- "hip_circumference.1.0" 	
  names(df)[names(df) == "X49.2.0"]    <- "hip_circumference.2.0" 	
  df$WHR.0.0 			<- compute_WHR(df$waist_circumference.0.0,df$hip_circumference.0.0) # Waist to hip ratio
  df$WHR.1.0 			<- compute_WHR(df$waist_circumference.1.0,df$hip_circumference.1.0) # Waist to hip ratio
  df$WHR.2.0 			<- compute_WHR(df$waist_circumference.2.0,df$hip_circumference.2.0) # Waist to hip ratio
  names(df)[names(df) == "X20116.0.0"] <- "smoking.status.0.0" # Summary of current / past smoking status 
  names(df)[names(df) == "X20116.1.0"] <- "smoking.status.1.0" # Summary of current / past smoking status 
  names(df)[names(df) == "X20116.2.0"] <- "smoking.status.2.0" # Summary of current / past smoking status 
  
  # Variables from Cardiac MRI
  # names(df)[names(df) == "X22420.2.0"] <- "LV_ejection_fraction.2.0"
  # names(df)[names(df) == "X22421.2.0"] <- "LV_end_diastolic_volume.2.0"
  # names(df)[names(df) == "X22422.2.0"] <- "LV_end_systolic_volume.2.0"
  # names(df)[names(df) == "X22423.2.0"] <- "LV_stroke_volume.2.0"
  # names(df)[names(df) == "X22424.2.0"] <- "cardiac_output.2.0" 
  # names(df)[names(df) == "X22425.2.0"] <- "cardiac_index.2.0" 
  # names(df)[names(df) == "X22426.2.0"] <- "average_heart_rate.2.0" 
  # names(df)[names(df) == "X22427.2.0"] <- "body_surface_area.2.0" 
  # 
  # Diastolic BP (automatic reading)
  df$diastolic.BP.0.0 <- compute_single_measure_from_two(df[,"X4079.0.0"],df[,"X4079.0.1"])
  df$diastolic.BP.1.0 <- compute_single_measure_from_two(df[,"X4079.1.0"],df[,"X4079.1.1"])
  df$diastolic.BP.2.0 <- compute_single_measure_from_two(df[,"X4079.2.0"],df[,"X4079.2.1"])
  
  # Systiolic BP (automatic reading)
  df$systolic.BP.0.0 <- compute_single_measure_from_two(df[,"X4080.0.0"],df[,"X4080.0.1"])
  df$systolic.BP.1.0 <- compute_single_measure_from_two(df[,"X4080.1.0"],df[,"X4080.1.1"])
  df$systolic.BP.2.0 <- compute_single_measure_from_two(df[,"X4080.2.0"],df[,"X4080.2.1"])
  
  # Pulse rate (automatic reading)
  # df$pulse_rate.0.0 <- compute_single_measure_from_two(df[,"X102.0.0"],df[,"X102.0.1"])
  # df$pulse_rate.1.0 <- compute_single_measure_from_two(df[,"X102.1.0"],df[,"X102.1.1"])
  # df$pulse_rate.2.0 <- compute_single_measure_from_two(df[,"X102.2.0"],df[,"X102.2.1"])
  # 
  # # Compute pulse pressure
  # df$pulse_pressure.0.0 <- df$systolic.BP.0.0 - df$diastolic.BP.0.0 
  # df$pulse_pressure.1.0 <- df$systolic.BP.1.0 - df$diastolic.BP.1.0 
  # df$pulse_pressure.2.0 <- df$systolic.BP.2.0 - df$diastolic.BP.2.0 
  
  # Ethnicity variables
  names(df)[names(df) == "X21000.0.0"] <- "ethnicity.0.0"
  names(df)[names(df) == "X21000.1.0"] <- "ethnicity.1.0"
  names(df)[names(df) == "X21000.2.0"] <- "ethnicity.2.0"
  
  # Blood cell stuff
  # names(df)[names(df) == "X30000.0.0"] <- "white_blood_cell_count.0.0"
  # names(df)[names(df) == "X30000.1.0"] <- "white_blood_cell_count.1.0"
  # names(df)[names(df) == "X30000.2.0"] <- "white_blood_cell_count.2.0"
  # 
  # names(df)[names(df) == "X30010.0.0"] <- "red_blood_cell_count.0.0"
  # names(df)[names(df) == "X30010.1.0"] <- "red_blood_cell_count.1.0"
  # names(df)[names(df) == "X30010.2.0"] <- "red_blood_cell_count.2.0"
  # 
  # Find all that have caucasian origine
  df$caucasian.0.0 <- create_caucasian_noncaucasian(df$ethnicity.0.0)
  df$caucasian.1.0 <- create_caucasian_noncaucasian(df$ethnicity.1.0)
  df$caucasian.2.0 <- create_caucasian_noncaucasian(df$ethnicity.2.0)
  
  n_items = length(grep( "X20002.2.", names(df), value = TRUE))
  # Create diabetic yes/no diagnosis
  diagnosis <- c(1221, 1222, 1223) # Gestational diabetes, DT1, DT2 
  df$diabetic.0.0 <- create_dictomous_diagnosis_variable(df,var="X20002.0.",Nitems = n_items,diagnosis)
  df$diabetic.1.0 <- create_dictomous_diagnosis_variable(df,var="X20002.1.",Nitems = n_items,diagnosis)
  df$diabetic.2.0 <- create_dictomous_diagnosis_variable(df,var="X20002.2.",Nitems = n_items,diagnosis)
  
  diagnosis <- c(1222) # Diabetes Type 1
  df$diabeticType1.0.0 <- create_dictomous_diagnosis_variable(df,var="X20002.0.",Nitems = n_items,diagnosis)
  df$diabeticType1.1.0 <- create_dictomous_diagnosis_variable(df,var="X20002.1.",Nitems = n_items,diagnosis)
  df$diabeticType1.2.0 <- create_dictomous_diagnosis_variable(df,var="X20002.2.",Nitems = n_items,diagnosis)
  
  diagnosis <- c(1223) # Diabetes Type 2
  df$diabeticType2.0.0 <- create_dictomous_diagnosis_variable(df,var="X20002.0.",Nitems = n_items,diagnosis)
  df$diabeticType2.1.0 <- create_dictomous_diagnosis_variable(df,var="X20002.1.",Nitems = n_items,diagnosis)
  df$diabeticType2.2.0 <- create_dictomous_diagnosis_variable(df,var="X20002.2.",Nitems = n_items,diagnosis)
  
  # # Create depresion yes/no diagnosis
  # diagnosis <- c(1286) # depression
  # df$depression.0.0 <- create_dictomous_diagnosis_variable(df,var="X20002.0.",Nitems = n_items,diagnosis)
  # df$depression.1.0 <- create_dictomous_diagnosis_variable(df,var="X20002.1.",Nitems = n_items,diagnosis)
  # df$depression.2.0 <- create_dictomous_diagnosis_variable(df,var="X20002.2.",Nitems = n_items,diagnosis)
  
  # Create hypertension yes/no diagnosis
  diagnosis <- c(1065,1072,1073) # Hypertension, essential hypertension, gestational hypertension/pre-eclampsia
  df$hypertension.0.0 <- create_dictomous_diagnosis_variable(df,var="X20002.0.",Nitems = n_items,diagnosis)
  df$hypertension.1.0 <- create_dictomous_diagnosis_variable(df,var="X20002.1.",Nitems = n_items,diagnosis)
  df$hypertension.2.0 <- create_dictomous_diagnosis_variable(df,var="X20002.2.",Nitems = n_items,diagnosis)
  
  # Create high cholesterol yes/no diagnosis
  diagnosis <- c(1473) # High cholesterol
  df$high_cholesterol.0.0 <- create_dictomous_diagnosis_variable(df,var="X20002.0.",Nitems = n_items,diagnosis)
  df$high_cholesterol.1.0 <- create_dictomous_diagnosis_variable(df,var="X20002.1.",Nitems = n_items,diagnosis)
  df$high_cholesterol.2.0 <- create_dictomous_diagnosis_variable(df,var="X20002.2.",Nitems = n_items,diagnosis)
  
  # Prepare alcohol variable 
  ind <- df$alcohol_drinker.2.0 %in% c("-3")
  df$smoking.status.2.0[ind] <- NA
  df$alcohol_drinker.2.0 <- factor(df$alcohol_drinker.2.0, levels = c("0","1","2")) 
  
  df$alcohol.2.0 <- NA
  ind <- df$alcohol_drinker.2.0 %in% c(2) # Current=2 consumption
  df$alcohol.2.0[ind] <- TRUE 
  ind <- df$alcohol_drinker.2.0 %in% c(0,1) # Never=0, previous=1 consumption
  df$alcohol.2.0[ind] <- FALSE 
  df$alcohol.2.0 <- factor(df$alcohol.2.0)
  
  # Prepare smoker variable 
  ind <- df$smoking.status.2.0 %in% c("-3") 
  df$smoking.status.2.0[ind] <- NA
  df$smoking.status.2.0 <- factor(df$smoking.status.2.0,levels = c("0","1","2"))  
  
  df$smoking.2.0 <- NA
  ind <- df$smoking.status.2.0 %in% c(2) # Current=2 consumption
  df$smoking.2.0[ind] <- TRUE 
  ind <- df$smoking.status.2.0 %in% c(0,1) # Never=0, previous=1 consumption
  df$smoking.2.0[ind] <- FALSE 
  df$smoking.2.0 <- factor(df$smoking.2.0)
  
  # Generate caucasian/non caucasian variable from two timepoints
  levels(df$caucasian.0.0)[levels(df$caucasian.0.0) == ""] <- NA
  levels(df$caucasian.2.0)[levels(df$caucasian.2.0) == ""] <- NA
  
  df$Ethnicity <- df$caucasian.2.0
  ind <- is.na(df$Ethnicity) & !is.na(df$caucasian.0.0)
  df$Ethnicity[ind] <- df$caucasian.0.0[ind]
  
  ###############################################################################
  # Prepare subset that we are interested in.
  ###############################################################################
  keeps <- c("eid","sex", "birth.year","birth.month","age.0.0","age.1.0",
             "age.2.0",
             # "date.of.death.0.0","date.of.death.1.0","date.of.death.2.0",
             # "date.of.myocardinal.infarction.0.0",
             "alcohol_drinker.0.0","alcohol_drinker.1.0",
             "alcohol_drinker.2.0","alcohol.2.0",
             "smoking.status.0.0","smoking.status.1.0",
             "smoking.status.2.0","smoking.2.0",
             "height.0.0","height.1.0",
             "height.2.0",
             "weight.0.0","weight.1.0",
             "weight.2.0",#"BMI.0.0","BMI.1.0",
             "BMI.2.0",
             "waist_circumference.0.0","waist_circumference.1.0",
             "waist_circumference.2.0",
             "hip_circumference.0.0","hip_circumference.1.0",
             "hip_circumference.2.0",
             "WHR.0.0","WHR.1.0","WHR.2.0",
             "diastolic.BP.0.0","diastolic.BP.1.0","diastolic.BP.2.0",
             "systolic.BP.0.0","systolic.BP.1.0", "systolic.BP.2.0",
             # "pulse_pressure.0.0","pulse_pressure.1.0","pulse_pressure.2.0",
             # "pulse_rate.0.0","pulse_rate.1.0","pulse_rate.2.0",
             "Ethnicity",
             "Assessment_centre.0.0","Assessment_centre.1.0",
             "Assessment_centre.2.0",
             "diabetic.0.0","diabetic.1.0","diabetic.2.0",
             "diabeticType1.0.0","diabeticType1.1.0","diabeticType1.2.0",
             "diabeticType2.0.0","diabeticType2.1.0","diabeticType2.2.0",
             # "depression.0.0","depression.1.0","depression.2.0",
             "high_cholesterol.0.0","high_cholesterol.1.0","high_cholesterol.2.0",
             "hypertension.0.0","hypertension.1.0","hypertension.2.0" 
             # "LV_ejection_fraction.2.0","LV_end_diastolic_volume.2.0",
             # "LV_end_systolic_volume.2.0","LV_stroke_volume.2.0","cardiac_output.2.0",
             # "cardiac_index.2.0","average_heart_rate.2.0","body_surface_area.2.0",
             # "white_blood_cell_count.0.0","white_blood_cell_count.1.0","white_blood_cell_count.2.0",
             # "red_blood_cell_count.0.0","red_blood_cell_count.1.0","red_blood_cell_count.2.0"
             )
  
  # Keep non-cancer diagnosis X20002.2.* in the file
  var <- "X20002.2."
  Nitems <- n_items
  for(i in 1:Nitems){
    tvar <- paste0(var,as.character(i-1))
    keeps <- append(keeps,tvar)
  }
  
  df <- df[,names(df) %in% keeps]
  Ndemo <- nrow(df)
  
  # Exclude participants that do not have antropometric measure or the included covariates
  vars <- c("height.2.0","weight.2.0","BMI.2.0","waist_circumference.2.0","hip_circumference.2.0","WHR.2.0",
            "Ethnicity","diabetic.2.0","high_cholesterol.2.0","hypertension.2.0","smoking.2.0","alcohol.2.0")
  ind <- complete.cases(df[,vars])
  df <- df[ind,]
  
  print("N participants after only keeping complete cases - demographcs")
  print(nrow(df))
  
  return(df)
}


prepare_imaging_data <- function(df.area, df.thickness, df.aseg){

  # Create combined dataframe thickness / area
  df.area_thickness <- merge(df.area,df.thickness, by.x = "lh.aparc.area",
                             by.y = "lh.aparc.thickness",all = FALSE)

  ########################
  # Create imaging dataframe
  ########################
  df.img <- merge(df.aseg, df.area_thickness,
                  by.x = "MRid", by.y = "lh.aparc.area", all = FALSE)

  # Remove  "FS_" from MRid
  df.img$eid <- as.integer(gsub("FS_","",df.img$MRid)) # Create eid

  ########################
  # Create combined measures
  ########################

  tmeasures <- c("Lateral.Ventricle","Cerebellum.White.Matter","Cerebellum.Cortex","Thalamus.Proper",
                 "Caudate","Putamen","Pallidum","Hippocampus","Amygdala","Accumbens.area")
  for(tmp in tmeasures){
    df.img[,paste0("Total.",tmp)] <- (df.img[,paste0("Right.",tmp)] + df.img[,paste0("Left.",tmp)])/2
  }

  # Create mean global measure
  df.img$MeanCorticalThickness <- (df.img$rh_MeanThickness_thickness +
                                     df.img$lh_MeanThickness_thickness)/2

  # Create mean global measure
  df.img$MeanWhiteSurfArea <- (df.img$rh_WhiteSurfArea_area +
                                 df.img$lh_WhiteSurfArea_area)/2

  return(df.img)
}

prepare_imaging_data_updated <- function(df.area, df.thickness, df.aseg, df.volume){
  
  # Create combined dataframe thickness / area
  df.area_thickness <- merge(df.area,df.thickness, 
                             by.x = "lh.aparc.area",
                             by.y = "lh.aparc.thickness",
                             all = FALSE)
  
  # Add in volume as well
  df.area_thickness <- merge(df.area_thickness,df.volume, 
                             by.x = "lh.aparc.area",
                             by.y = "lh.aparc.volume",
                             all = FALSE)
  
  ########################
  # Create imaging dataframe
  ########################
  df.img <- merge(df.aseg, df.area_thickness, 
                  by.x = "MRid", by.y = "lh.aparc.area", all = FALSE)

  ########################
  # Remove  "FS_" from MRid
  ########################
  df.img$eid <- as.integer(gsub("FS_","",df.img$MRid)) # Create eid
  
  ########################
  # Create combined measures
  ########################
  
  tmeasures <- c("Lateral.Ventricle","Cerebellum.White.Matter","Cerebellum.Cortex","Thalamus.Proper",
                 "Caudate","Putamen","Pallidum","Hippocampus","Amygdala","Accumbens.area")
  for(tmp in tmeasures){
    df.img[,paste0("Total.",tmp)] <- (df.img[,paste0("Right.",tmp)] + df.img[,paste0("Left.",tmp)])/2  
  }
  
  # Create mean global measure
  df.img$MeanCorticalThickness <- (df.img$rh_MeanThickness_thickness +
                                     df.img$lh_MeanThickness_thickness)/2
  
  # Create mean global measure
  df.img$MeanWhiteSurfArea <- (df.img$rh_WhiteSurfArea_area + 
                                 df.img$lh_WhiteSurfArea_area)/2
  
  return(df.img)
}


prepare_body_MRI_data_updated <- function(df){
  
  drops <- c("X20260.2.0","X20260.3.0","X12223.2.0","X12223.3.0",
             "X12140.2.0","X12140.3.0","X20201.2.0","X20201.3.0",
             "X20202.2.0","X20202.3.0","X20203.2.0","X20203.3.0",
             "X20204.2.0","X20204.3.0","X20206.2.0","X20206.3.0",
             "X20254.2.0","X20254.3.0","X20259.2.0","X20259.3.0",
             "X22400.0.0","X22400.1.0","X22401.0.0")
  df <- df[,!names(df) %in% drops]
  
  # Only include data fields from time point 2. 
  names(df)[names(df) == "X12224.2.0"] <- "AbdominalMRI_completed.2.0"
  # names(df)[names(df) == "X12224.3.0"] <- "AbdominalMRI_completed.3.0"
  # names(df)[names(df) == "X22402.2.0"] <- "Liver_PDFF.2.0" 		# Liver fat percentage 
  # names(df)[names(df) == "X22403.2.0"] <- "ATLMV_R.2.0" 		# Anterior thigh lean muscle volume (right)
  # names(df)[names(df) == "X22404.2.0"] <- "PTLMV_R.2.0" 		# Posterior thigh lean muscle volume (right)
  # names(df)[names(df) == "X22405.2.0"] <- "ATLMV_L.2.0" 		# Anterior thigh lean muscle volume (left)
  # names(df)[names(df) == "X22406.2.0"] <- "PTLMV_L.2.0" 		# Posterior thigh lean muscle volume (left)
  names(df)[names(df) == "X22407.2.0"] <- "VAT.2.0" 		    # Visceral adipose tissue volume (VAT)
  names(df)[names(df) == "X22408.2.0"] <- "ASAT.2.0" 		    # Abdominal subcutaneous adipose tissue voluem (ASAT) 
  names(df)[names(df) == "X22409.2.0"] <- "TTMV.2.0" 		    # Total thigh muscle volumea (sum of all muscle groups)
  names(df)[names(df) == "X22410.2.0"] <- "VAT_ASAT.2.0" 		# Total trunk fat (sum of VAT and ASAT)
  # names(df)[names(df) == "X22411.2.0"] <- "Error_abdomen.2.0" 	# Vat/ASAT error fields
  # names(df)[names(df) == "X22412.2.0"] <- "Thigh_error_L.2.0" 	# Thigh error indicator (left)
  # names(df)[names(df) == "X22412.2.1"] <- "Thigh_error_L.2.1" 	# Thigh error indicator (left)
  # names(df)[names(df) == "X22413.2.0"] <- "Thigh_error_R.2.0" 	# Thigh error indicator (right)
  # names(df)[names(df) == "X22413.2.1"] <- "Thigh_error_R.2.1" 	# Thigh error indicator (right)
  names(df)[names(df) == "X22414.2.0"] <- "Image_quality.2.0" 	# Image quality indicator
  names(df)[names(df) == "X22414.2.1"] <- "Image_quality.2.1"   # Image quality indicator
  # names(df)[names(df) == "X22415.2.0"] <- "TAT.2.0"   # Total adipose tissue volume measured between the bottom of thigh muscle volume and the top of vertebrae T9.
  # names(df)[names(df) == "X22416.2.0"] <- "TLT.2.0"   # Total lean tissue volume measured between the bottom of thigh muscle volume and the top of vertebrae T9.
  # names(df)[names(df) == "X22417.2.0"] <- "Liver_iron.2.0"   # Liver iron corrected T1 (ct1) which measure liver inflammation and fibrosis.
  # names(df)[names(df) == "X22432.2.0"] <- "total_abdominal_adipose_tissue_index.2.0"   # (VAT+ASAT)/(height*height)
  # names(df)[names(df) == "X22433.2.0"] <- "weight2muscle_ratio.2.0"   # Weight-to-muscle ratio.
  # names(df)[names(df) == "X22434.2.0"] <- "abdominal_fat_ratio.2.0"   # Total abdominal fat divided by total abdominal fat and thigh muscle volume.
  names(df)[names(df) == "X22435.2.0"] <- "MFI.2.0"   # Muscle fat infiltration
  names(df)[names(df) == "X22436.2.0"] <- "Liver_PDFF_AMRA.2.0"   # Liver proton density fat fraction based on AMRA technology
  
  # Create my VAT + ASAT variable
  df$myVat_ASAT.2.0 <- df$VAT.2.0 + df$ASAT.2.0
  
  # # Comput visceral fat ratio: VAT / (VAT + ASAT)
  # df$VAT_fat_ratio.2.0 <- df$VAT.2.0/df$VAT_ASAT.2.0
  # 
  # # Comput fat to muscle ratio: (VAT + ASAT) / (VAT + ASAT + TTMV)
  # df$Fat2muscle_ratio.2.0 <- df$VAT_ASAT.2.0/(df$VAT_ASAT.2.0 + df$TTMV.2.0)
  
  # Remove those with NA or 0 for body MRI
  df$AbdominalMRI_completed.2.0 <- factor(df$AbdominalMRI_completed.2.0)
  ind <- df$AbdominalMRI_completed.2.0 %in% c(1) # | is.na(df$AbdominalMRI_completed.2.0)
  df <- df[ind,]
  
  # Include only those without all columns equal to NA - do not count the two first columns.
  # ind <- rowSums(is.na(df[,-c(1,2)])) == ncol(df[,-c(1,2)])
  ind <- exclude_all_missing(df,-c(1,2,3))
  df <- df[!ind,]
  
  # Number of participants with body MRI
  Nbody <- nrow(df)
  print("No participants with body MRI: ")
  print(Nbody)
  
  df <- df[,!names(df)%in% c("AbdominalMRI_completed.2.0","AbdominalMRI_completed.3.0")]
  
  # Write to file those with body MRI
  # write.csv(df, paste0(fpath,"ukb_bodyMRI_subset.csv"), na = "", row.names = FALSE)
  
  #############################################################
  #Remove participants that meet exclusion criteria - body MRI
  #############################################################
  exclude_body <- c(1,3,6,7,8,9)
  ind <- (df$Image_quality.2.0 %in% exclude_body) | (df$Image_quality.2.1 %in% exclude_body)
  df <- df[!ind,]
  
  # Figure out how many was excluded due to image quality
  Nbody_excluded <- Nbody - nrow(df)
  print("No participants excluded from body MRI: ")
  print(Nbody_excluded)
  
  df$HasBody <- TRUE
  
  return(df)
}

prepare_body_MRI_data <- function(df){
  
  # Only include data fields from time point 2. 
  names(df)[names(df) == "X12224.2.0"] <- "AbdominalMRI_completed.2.0"
  names(df)[names(df) == "X22402.2.0"] <- "Liver_PDFF.2.0" 		# Liver fat percentage 
  names(df)[names(df) == "X22403.2.0"] <- "ATLMV_R.2.0" 		# Anterior thigh lean muscle volume (right)
  names(df)[names(df) == "X22404.2.0"] <- "PTLMV_R.2.0" 		# Posterior thigh lean muscle volume (right)
  names(df)[names(df) == "X22405.2.0"] <- "ATLMV_L.2.0" 		# Anterior thigh lean muscle volume (left)
  names(df)[names(df) == "X22406.2.0"] <- "PTLMV_L.2.0" 		# Posterior thigh lean muscle volume (left)
  names(df)[names(df) == "X22407.2.0"] <- "VAT.2.0" 		# Visceral adipose tissue volume (VAT)
  names(df)[names(df) == "X22408.2.0"] <- "ASAT.2.0" 		# Abdominal subcutaneous adipose tissue voluem (ASAT) 
  names(df)[names(df) == "X22409.2.0"] <- "TTMV.2.0" 		# Total thigh muscle volumea (sum of all muscle groups)
  names(df)[names(df) == "X22410.2.0"] <- "VAT_ASAT.2.0" 		# Total trunk fat (sum of VAT and ASAT)
  names(df)[names(df) == "X22411.2.0"] <- "Error_abdomen.2.0" 	# Vat/ASAT error fields
  names(df)[names(df) == "X22412.2.0"] <- "Thigh_error_L.2.0" 	# Thigh error indicator (left)
  names(df)[names(df) == "X22412.2.1"] <- "Thigh_error_L.2.1" 	# Thigh error indicator (left)
  names(df)[names(df) == "X22413.2.0"] <- "Thigh_error_R.2.0" 	# Thigh error indicator (right)
  names(df)[names(df) == "X22413.2.1"] <- "Thigh_error_R.2.1" 	# Thigh error indicator (right)
  names(df)[names(df) == "X22414.2.0"] <- "Image_quality.2.0" 	# Image quality indicator
  names(df)[names(df) == "X22414.2.1"] <- "Image_quality.2.1"   # Image quality indicator
  
  
  # Comput visceral fat ratio: VAT / (VAT + ASAT)
  df$VAT_fat_ratio.2.0 <- df$VAT.2.0/df$VAT_ASAT.2.0
  
  # Comput fat to muscle ratio: (VAT + ASAT) / (VAT + ASAT + TTMV)
  df$Fat2muscle_ratio.2.0 <- df$VAT_ASAT.2.0/(df$VAT_ASAT.2.0 + df$TTMV.2.0)
  
  # Drop certain data fields that are not used here
  drops <- c("X12140.2.0","X12223.2.0","X12623.2.0","X12664.2.0","X12848.2.0","X20201.2.0","X20202.2.0","X20203.2.0",
             "X20204.2.0","X20206.2.0","X20254.2.0","X20259.2.0","X20260.2.0","X22400.0.0","X22401.0.0","X22414.2.0",
             "X22414.2.1")
  df <- df[,!names(df)%in% drops]
  
  
  # Remove those with NA or 0 for body MRI
  df$AbdominalMRI_completed.2.0 <- factor(df$AbdominalMRI_completed.2.0)
  ind <- df$AbdominalMRI_completed.2.0 == 0 | is.na(df$AbdominalMRI_completed.2.0)
  df <- df[!ind,]
  
  # Include only those without all columns equal to NA - do not count the two first columns.
  # ind <- rowSums(is.na(df[,-c(1,2)])) == ncol(df[,-c(1,2)])
  ind <- exclude_all_missing(df,-c(1,2))
  df <- df[!ind,]
  
  # Number of participants with body MRI
  Nbody <- nrow(df)
  print("No participants with body MRI: ")
  print(Nbody)
  
  df <- df[,!names(df)%in% c("AbdominalMRI_completed.2.0")]
  
  # # Write to file those with body MRI
  # write.csv(df, paste0(fpath,"ukb26346_wBodyMRI.csv"), na = "", row.names = FALSE)
  
  #############################################################
  #Remove participants that meet exclusion criteria - body MRI
  #############################################################
  exclude_body <- c(1,3,6,7,8,9)
  ind <- (df$Image_quality.2.0 %in% exclude_body) | (df$Image_quality.2.1 %in% exclude_body)
  df <- df[!ind,]
  
  # Figure out how many was excluded due to image quality
  Nbody_excluded <- Nbody - nrow(df)
  print("No participants excluded from body MRI: ")
  print(Nbody_excluded)
  
  df$HasBody <- TRUE
  
  return(df)
}

prepare_euler <- function(df){
  # Process Euler numbers for QC
  
  # remove those with missing euler numbers
  ind <- is.na(df$euler_lh) | is.na(df$euler_rh)
  df <- df[!ind,]
  
  df$euler_average <- (as.numeric(df$euler_lh) + as.numeric(df$euler_rh))/2
  
  names(df)[names(df) == "subject"] <- "eid"     # MR-id
  df$eid <- as.integer(gsub("FS_","",df$eid)) # Create eid
  
  drops <- c("defectindex_lh", "defectindex_rh")
  
  df <- df[,!names(df) %in% drops]
  
  return(df)
}
