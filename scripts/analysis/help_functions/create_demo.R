get_mean_sd <- function(input, d=1){
  out <- paste0(paste0(as.character(round(mean(input, na.rm = TRUE), digits = d)),"±"),
                as.character(round(sd(input, na.rm = TRUE), digits = d))) 
  return(out);
}

get_missing <- function(f_in,m_in){
  out <- c(as.character(length(f_in[is.na(f_in)])),
           as.character(length(m_in[is.na(m_in)])))  
}

get_range <- function(input){
  out <- paste0("[", as.character(min(input))," ",as.character(max(input)),"]")
  return(out)
}

d_wilcoxon_test <- function(v, df){
  val <- df[,v]
  (w=wilcox.test(val[df$sex == 1],val[df$sex == 0],
                 conf.int= TRUE, alternative = "two.sided"))
  out <- c(as.character(round(w$estimate, digits = 4)), 
           as.character(w$p.value),"wilcoxon")
  return(out);
}

d_t_test <- function(v,df,d=1){
  val <- df[,v]
  if(var.test(val[df$sex == 1],val[df$sex == 0])$p.value <= 0.05){
    t <- t.test(val[df$sex == 1],val[df$sex == 0], var.equal = FALSE)
  }else{
    t <- t.test(val[df$sex == 1],val[df$sex == 0], var.equal = TRUE)
  }
  return(c(as.character(round(t$statistic, digits = d)),
           as.character(t$p.value),t$method))
}

d_Chi2_test <- function(v,df,d=1){
  
  M = table(df[,v],df$sex)
  (Xsq = chisq.test(M))
  
  out <- c(as.character(round(Xsq$statistic[[1]], digits = d)), 
           as.character(Xsq$p.value),
           Xsq$method)
  return(out)
}
  
get_never_prev_current <- function(df,val){
  never <- getN(df,val,0)
  previous <- getN(df,val,1)
  current <- getN(df,val,2)
  out <- paste(as.character(current),"/",
               as.character(previous),"/",as.character(never))
  return(out)
}

get_row_val_cont <- function(df,val,sname){
  ind <- df$sex == 1
  df.male  <- df[ind,]
  df.women <- df[!ind,]
  
  cwomen <- get_mean_sd(df.women[,val]) 
  cmale  <- get_mean_sd(df.male[,val])
  t <- NULL
  if(val %in% c("Liver_PDFF.2.0", "VAT.2.0", "ASAT.2.0")){
    t <- d_wilcoxon_test(val, df)
  }else{
    t <- d_t_test(val,df)
  }
  out <- append(append(c(sname, cwomen, cmale),t), get_missing(df.women[,val], df.male[,val]))
}

get_row_val_cat <- function(df,val,sname, i = 1){
  
  ind <- df$sex == 1
  df.male  <- df[ind,]
  df.women <- df[!ind,]
  
  fn <- getN(df.women,val,i)
  fnp <- (fn/nrow(df.women))*100
  
  mn <- getN(df.male,val,i)
  mnp <- (mn/nrow(df.male))*100
  
  x2 <- d_Chi2_test(val,df)
  
  out <- append(append(c(sname, 
                  paste0(as.character(fn)," (",as.character(round(fnp,digits = 1)),")"), 
                  paste0(as.character(mn)," (",as.character(round(mnp, digits = 1)),")")),
                  x2),get_missing(df.women[,val], df.male[,val]))
}


create_demographics <- function(df, ofile)
{

  ind <- df$sex == 1
  df.male  <- df[ind,]
  df.women <- df[!ind,]
  
  # Extract numbers
  row_val <- c("N",as.character(nrow(df.women)),as.character(nrow(df.male)),"","","","","")
  
  # Extract age mean ± standard deviation
  row_val <- rbind(row_val, get_row_val_cont(df,"age.2.0","Age (year)"))
  
  
  # Extract age range
  row_val <- rbind(row_val, c("Age range (year)", 
                                     get_range(df.women$age.2.0), 
                                     get_range(df.male$age.2.0),"","","","",""))
  
  # Extract caucasian
  row_val <- rbind(row_val, get_row_val_cat(df,"Ethnicity","Caucasian","Caucasian"))
  
  # Extract smoking information
  row_val <- rbind(row_val, get_row_val_cat(df,"smoking.2.0","Smoker",TRUE))
  
  # Smoking status
  cwomen <- get_never_prev_current(df.women,"smoking.status.2.0")
  cmen <- get_never_prev_current(df.male,"smoking.status.2.0")
  row_val <- rbind(row_val,c("Smoker Current/previous/never",cwomen,cmen,"","","","",""))
  
  # Extract alcohol information
  row_val <- rbind(row_val, get_row_val_cat(df,"alcohol.2.0","Alcohol drinker",TRUE))
  
  # alcohold status
  cwomen <- get_never_prev_current(df.women,"alcohol_drinker.2.0")
  cmen <- get_never_prev_current(df.male,"alcohol_drinker.2.0")
  row_val <- rbind(row_val,c("Alcohol drinker Current/previous/never",cwomen,cmen,"","","","",""))
  
  # Extract height
  row_val <- rbind(row_val, get_row_val_cont(df,"height.2.0","Height (cm)"))
  
  # Extract weight
  row_val <- rbind(row_val, get_row_val_cont(df,"weight.2.0","Weight (kg)"))
  
  # Extract BMI
  row_val <- rbind(row_val, get_row_val_cont(df,"BMI.2.0","BMI"))
  
  # Extract waist circumference
  row_val <- rbind(row_val, get_row_val_cont(df,"waist_circumference.2.0","Waist circumference (cm)"))
  
  # Extract HIP circumference
  row_val <- rbind(row_val, get_row_val_cont(df,"hip_circumference.2.0","Hip circumference (cm)"))
  
  # Extract WHR
  row_val <- rbind(row_val, get_row_val_cont(df,"WHR.2.0","WHR"))
  
  # Extract liver fat percentage
  row_val <- rbind(row_val, get_row_val_cont(df,"Liver_PDFF.2.0","Liver PDFF"))

  row_val <- rbind(row_val, get_row_val_cont(df,"VAT.2.0","VAT"))
  row_val <- rbind(row_val, get_row_val_cont(df,"ASAT.2.0","ASAT"))
  row_val <- rbind(row_val, get_row_val_cont(df,"VAT_ASAT.2.0","VAT+ASAT"))
  row_val <- rbind(row_val, get_row_val_cont(df,"MFI.2.0","MFI"))
  row_val <- rbind(row_val, get_row_val_cont(df,"TTMV.2.0","TTMV"))
  
  # Extract diagnosis information
  row_val <- rbind(row_val, get_row_val_cat(df,"diabetic.2.0","Diabetic",TRUE))
  row_val <- rbind(row_val, get_row_val_cat(df,"high_cholesterol.2.0","High cholesterol",TRUE))
  row_val <- rbind(row_val, get_row_val_cat(df,"hypertension.2.0","Hypertension",TRUE))

  # Dump data to file
  out <- data.frame(row_val, row.names = NULL)
  colnames(out) <- c("Variables", "Women","Men","test", "p-value","Applied test", "missing women", "missing men")
  
  corder <- c("Variables", "Men","Women","test", "p-value","Applied test", "missing women", "missing men")
  out <- out[,corder]
  
  write.csv(out, ofile, row.names = FALSE, na = "")
  
}
