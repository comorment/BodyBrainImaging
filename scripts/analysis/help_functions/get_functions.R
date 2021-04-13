get_subset_barplot_for_lars_df <- function (opath){
  
  # Create dataframe for barplots: BMI, WHR, waist circumference
  df1 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","BMI.c_Subset_For_Lars.csv"))
  df1$Covariate <- "BMI"
  
  df2 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","WHR.c_Subset_For_Lars.csv"))
  df2$Covariate <- "WHR"
  
  df3 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","diastolic.BP.c_Subset_For_Lars.csv"))
  df3$Covariate <- "Diastolic BP"
  
  df4 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","systolic.BP.c_Subset_For_Lars.csv"))
  df4$Covariate <- "Systolic BP"
  
  df5 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","Liver_PDFF.c_Subset_For_Lars.csv"))
  df5$Covariate <- "Liver fat"
  
  df6 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","VAT.c_Subset_For_Lars.csv"))
  df6$Covariate <- "Visceral fat"
  
  # Create combined 
  df <- rbind(rbind(rbind(rbind(rbind(df1,df2), 
                                df3),df4),df5),df6)
  df$Covariate <- factor(df$Covariate, levels = c("BMI", "WHR","Diastolic BP",
                                                  "Systolic BP", "Liver fat","Visceral fat"))
  
  return(df)
  
}

get_Anthropometric_barplot_df <- function(opath, number, ending="", L = "", R = ""){
  
  # Create dataframe for barplots: BMI, WHR, waist circumference
  ifile1 <- paste0(opath,"brain_body/RegressorOfInterest/",
                  "BMI.c_",L,R,"anthrometric_ALL",as.character(number),ending,".csv")
  df1 <- read.csv(ifile1)
  df1$Covariate <- "BMI"
  print(ifile1)
  
  ifile2 <- paste0(opath,"brain_body/RegressorOfInterest/",
                   "WHR.c_",L,R,"anthrometric_ALL",as.character(number),ending,".csv")
  df2 <- read.csv(ifile2)
  df2$Covariate <- "WHR"
  print(ifile2)
  
  ifile3 <- paste0(opath,"brain_body/RegressorOfInterest/",
                   "waist_circumference.c_",L,R,"anthrometric_ALL",as.character(number),ending,".csv")
  df3 <- read.csv(ifile3)
  df3$Covariate <- "WC"
  print(ifile3)
  
  df <- rbind(rbind(df1,df2), df3)
  if(ending != ""){
    df$Covariate[df$Covariate == "WC"] <- "WC E1"
    df$Covariate[df$Covariate == "BMI"] <- "BMI E1"
    df$Covariate[df$Covariate == "WHR"] <- "WHR E1"
    df$Covariate <- factor(df$Covariate, levels = c("WC E1","WHR E1","BMI E1"))
  }else{
    df$Covariate <- factor(df$Covariate, levels = c("WC","WHR","BMI"))
  }
  
  
  return(df)  
}

get_whole_body_barplot_df <- function(opath, number, L = "", R = ""){
  
  # Create dataframe for barplots: BMI, WHR, waist circumference
  df1 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","Liver_PDFF.c_",L,R,"bodyMRI_subsample",as.character(number),".csv"))
  df1$Covariate <- "Liver PDFF"
  
  df2 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","VAT.c_",L,R,"bodyMRI_subsample",as.character(number),".csv"))
  df2$Covariate <- "VAT"
  
  df3 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","ASAT.c_",L,R,"bodyMRI_subsample",as.character(number),".csv"))
  df3$Covariate <- "ASAT"
  
  df4 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","VAT_ASAT.c_",L,R,"bodyMRI_subsample",as.character(number),".csv"))
  df4$Covariate <- "VAT+ASAT"

  df5 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","MFI.c_",L,R,"bodyMRI_subsample",as.character(number),".csv"))
  df5$Covariate <- "MFI"
  
  df6 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","TTMV.c_",L,R,"bodyMRI_subsample",as.character(number),".csv"))
  df6$Covariate <- "TTMV"
  
  df7 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","BMI.c_",L,R,"bodyMRI_subsample",as.character(number),".csv"))
  df7$Covariate <- "BMI"
  
  df8 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","WHR.c_",L,R,"bodyMRI_subsample",as.character(number),".csv"))
  df8$Covariate <- "WHR"
  
  df9 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","waist_circumference.c_",L,R,"bodyMRI_subsample",as.character(number),".csv"))
  df9$Covariate <- "WC"
  
  df <- rbind(rbind(rbind(rbind(rbind(rbind(rbind(rbind(df1,df2), 
                                                  df3),df4),df5),df6),df7),df8),df9)
  df$Covariate <- factor(df$Covariate, levels = c("TTMV","MFI","VAT+ASAT","ASAT","VAT","Liver PDFF","WC","WHR","BMI"))
  
  return(df)
  
}

get_BP_barplot_df <- function(opath){
  
  # Create dataframe for barplots: BMI, WHR, waist circumference
  
  df1 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","BMI.c_anthrometric_and_BP.csv"))
  df1$Covariate <- "BMI"
  
  df2 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","WHR.c_anthrometric_and_BP.csv"))
  df2$Covariate <- "WHR"
  
  df3 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","waist_circumference.c_anthrometric_and_BP.csv"))
  df3$Covariate <- "WC"
  
  df4 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","diastolic.BP.c_anthrometric_and_BP.csv"))
  df4$Covariate <- "Diastolic BP"
  
  df5 <- read.csv(paste0(opath,"brain_body/RegressorOfInterest/","systolic.BP.c_anthrometric_and_BP.csv"))
  df5$Covariate <- "Systolic BP"
  
  df <- rbind(rbind(rbind(rbind(df1,df2), df3), df4), df5)
  df$Covariate <- factor(df$Covariate, levels = c("Diastolic BP","Systolic BP","WC","WHR","BMI"))
  
  return(df)
}


getN <- function(df, Variable, val)
{ 	# Get number with value val for a given variable in data frame df
  v <- df[,Variable]
  N <- length(v[v==val])
  return(N)
}

get_output <- function(m,df, dep_var, sexInteraction = FALSE)
{
  cnames <- names(m$coefficients)
  # cnames <- cnames[!cnames %in% c("(Intercept)")]
  
  # print(cnames)
  
  #######################################
  # Categorical variables
  #######################################
  vOut <- NULL
  categories <- c("sex","diabetic.2.0","smoking.2.0","alcohol.2.0",
                  "high_cholesterol.2.0","hypertension.2.0","Ethnicity",
                  "Assessment_centre.2.011027","Assessment_centre.2.011026")
  rnames <- NULL
  for(v in categories){
    if(v == "sex"){
      n1 <- getN(df, v, 0)
      n2 <- getN(df, v, 1)
      v  <- paste0(v,"1")  
    }else if(v== "Ethnicity"){
      n1 <- getN(df, v, "Caucasian")
      n2 <- getN(df, v, "nonCaucasian")
      v <- paste0(v,"nonCaucasian")
    }else if(v== "Assessment_centre.2.011027"){
      n1 <- getN(df, "Assessment_centre.2.0", 11025)
      n2 <- getN(df, "Assessment_centre.2.0", 11027)
    }else if(v== "Assessment_centre.2.011026"){
      n1 <- getN(df, "Assessment_centre.2.0", 11025)
      n2 <- getN(df, "Assessment_centre.2.0", 11026)
    }else{
      n1 <- getN(df, v, TRUE)
      
      n2 <- getN(df, v, FALSE)
      v  <- paste0(v,"TRUE")
    }
    if(!(v%in% cnames)){
      #print("v not in cnames")
      next
    }
    
    # print(v)
    # Compute cohen's d, then r from d
    d <- cohens_d(summary(m)$coefficients[v,3],summary(m)$df[2],n1,n2)
    seD <- standard_error_d(d,n1,n2)
    r <- convert_d_to_r(d,n1,n2)  
    
    # compute lower/upper CI
    rCI <- compute_CI_from_r(r,nrow(df)) 
    
    # print(v)
    # 
    # print(n1)
    # print(n2)
    # print(r)
    # 
    # Note: confidence interval can probably be computed from se d by transforming to r. 
    
    # Store d, t-value, p-value
    vOut <-  rbind(vOut,c(r, #d, # d
                          summary(m)$coefficients[v,3],  # t-value
                          summary(m)$coefficients[v,4], # p-value
                          rCI[1],rCI[2])) # Lower/upper CI empty for categorical value
    rnames <- append(rnames,v)
  } #end for 
  
  
  # print("Continuous vars next")
  
  #######################################
  # Continuous variables
  #######################################
  categories <- c("poly(age.c, 2)1","poly(age.c, 2)2",
                  "poly(age.c, 2)1:sex1","poly(age.c, 2)2:sex1",
                  "df[, reg_var]","poly(df[, reg_var], 2)1","poly(df[, reg_var], 2)2",
                  "sex1:df[, reg_var]","euler_average","ICV","systolic.BP.c",
                  "diastolic.BP.c","BMI.c","WHR.c","waist_circumference.c","(Intercept)")
  
  # include additional variables if processing imaging data
  c_cat <- cnames[cnames %in% categories]  
  # print(c_cat)
  for(v in c_cat){
    # if(v %in% cnames){
    # Compute cohen's d via the partial correlation coefficient r
    # d <- convert_r_to_D(partial_corr(summary(m)$coefficients[v,3],summary(m)$df[2]))
    # print(v)
    r <- partial_corr(summary(m)$coefficients[v,3],summary(m)$df[2])
    
    # compute lower/upper CI
    rCI <- compute_CI_from_r(r,nrow(df)) 
    
    # Store r, t-value, p-value
    vOut <-  rbind(vOut,c(r,#d, #d
                          summary(m)$coefficients[v,3],  # t-value
                          summary(m)$coefficients[v,4],  # p-value
                          rCI[1],rCI[2]))
    rnames <- append(rnames,v)
    # }
  }
  
  d.out <- as.data.frame(vOut) 
  colnames(d.out) <- c("r", "t-value","p-value","lCI","uCI")
  d.out$Covariate <- factor(rnames) 
  
  # Convert to numeric
  d.out <- d.out[, c("Covariate","r", "t-value","p-value","lCI","uCI")]
  d.out$p_full          <- d.out$"p-value"
  d.out$adj.r.squared   <- summary(m)$adj.r.squared
  d.out$degrees_freedom <- summary(m)$df[2]
  
  # round columns
  d.out$r_full <- d.out$r
  d.out$r <- round(d.out$r, digits = 2)
  d.out$"t-value"   <- round(d.out$"t-value", digits = 2)
  d.out$"p-value"   <- round(d.out$"p-value", digits = 4)
  d.out$adj.r.squared <- round(d.out$adj.r.squared, digits = 2)
  
  # print(names(d.out))
  # print("done get output")
  
  return(d.out)
}

# get_lm_output <- function(m, n1, n2, catVariables){
# 	# m: output from lm
# 	# n1: number of subj with sex=0
# 	# n2: number of subj with sex=1
# 
# 	# Create dataframe from lm model coefficients
# 	out <- as.data.frame(summary(m)$coefficients)
# 	out$D <- NA
# 	out$seD <- NA
# 
# 	out$df <- summary(m)$df[2]
# 	out$r2 <- summary(m)$r.squared
# 	# f <- summary(m)$fstatistic
# 
# 	# Remove estimate and standard error
# 	drops <- c("Estimate","Std. Error")
# 	out <- out[,!names(out) %in% drops]
# 	
# 	# Remove the intercept
# 	out <- out[!(rownames(out) == "(Intercept)"),]
# 	
# 	# Compute Cohen's d for categorical variables
# 	for(ii in 1:length(catVariables)){ 
# 		ind <- rownames(out) == catVariables[ii]
# 		out$D[ind] <- cohens_d(out$"t value"[ind],out$df[ind],n1,n2)
# 		out$seD[ind] <- standard_error_d(out$D[ind],n1,n2)
# 	}
# 	# Compute Cohen's d for non-categorical variables
# 	rnames <- rownames(out)[!rownames(out) %in% catVariables]
# 	for(ii in 1:length(rnames)){
# 		ind <- rownames(out) == rnames[ii]
# 		r <- partial_corr(out$"t value"[ind],out$df[ind])
# 		out$D[ind] <-convert_r_to_D(r) 
# 
# 		# TODO:compute se of D when derived from r
# 	}
# 		
# 	# Round outputs to desired no of digits
# 	out$t_value <- round(out$"t value", digits = 2)
# 	out$D <- round(out$D, digits = 2)
# 	out$p_value <- round(out$"Pr(>|t|)", digits = 4)
# 	
# 	keeps <- c("D","seD","t_value","p_value","df","r2")
# 
# 	return(out[,keeps])
# }

get_anova_output <- function(m1,m2){
  
  a <- anova(m1,m2)
  a <-as.data.frame(a)
  return(a)
}

format_body_name <- function(v){
  v <- gsub(".2.0","",v)
  v <- gsub("VAT_ASAT","VAT+ASAT",v)
  v <- gsub("_", " ", v)
  return(v)
}

format_structure_name <- function(v){
  v <- gsub("[.]"," ", x=v)
  v <- gsub("[_]"," ", v)
  v <- gsub("Total ","", v)
  v <- gsub("Left ","", v)
  v <- gsub("lh ","", v)
  v <- gsub("lh","", v)
  v <- gsub("Right ","", v)
  v <- gsub("rh ","", v)
  v <- gsub("rh","", v)
  v <- gsub("MeanWhiteSurfArea","Surface area (white)",
            gsub("MeanCorticalThickness","Cortical thickness",
                 gsub(" Proper","",
                      gsub(" area","",
                           gsub("X3","3",
                                gsub("Cerebellum White Matter","Cerebellum WM",
                                     gsub("Cerebellum Cortex","Cerebellum GM",
                                          gsub("CorticalWhiteMatterVol","Cortical WM",
                                               gsub("CortexVol", "Cortical GM", v)))))))))
  
  v <- gsub("MeanThickness thickness","Cortical thickness",v)
  v <- gsub("WhiteSurfArea","Surface area (white)",v)
  
  return(v)
}

