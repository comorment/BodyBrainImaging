# Explore psychiatric diagnosis
explore_psychiatric_illness <- function(df, opath, var = "X20002.2.", Nitems = 33){
  # var <- "X20002.2."
  # Nitems <- 33
  schizophrenia <- c("1289")
  bipolar <- c("1291")
  depression <- c("1286")
  anxiety_panic <- c("1287")
  self_harm <- c("1290")
  nervous_breakdown <- c("1288")

  ind_sc <- NULL 
  ind_bip <- NULL 
  ind_dep <- NULL
  ind_anx <- NULL
  ind_sharm <- NULL
  ind_bdown <- NULL
  for(i in 1:Nitems){
    tvar <- paste0(var,as.character(i-1))
    
    if(i == 1){
      ind_sc <- df[,tvar] %in% schizophrenia
      ind_bip <- df[,tvar] %in% bipolar
      ind_dep <- df[,tvar] %in% depression
      ind_anx <- df[,tvar] %in% anxiety_panic
      ind_sharm <- df[,tvar] %in% self_harm
      ind_bdown <- df[,tvar] %in% nervous_breakdown
    }else{
      ind_sc <- ind_sc | df[,tvar] %in% schizophrenia
      ind_bip <- ind_bip | df[,tvar] %in% bipolar
      ind_dep <- ind_dep | df[,tvar] %in% depression
      ind_anx <- ind_anx | df[,tvar] %in% anxiety_panic
      ind_sharm <- ind_sharm | df[,tvar] %in% self_harm
      ind_bdown <- ind_bdown | df[,tvar] %in% nervous_breakdown
    }
  }
  eid_bip <- df$eid[ind_bip]
  ind <- df.body$eid %in% eid_bip
  
  N_bip.brain <- length(eid_bip)
  N_bip.body <- nrow(df.body[ind,])
  
  print("No. bipolar brain, body:")
  print(N_bip.brain)
  print(N_bip.body)
  
  eid_sz <- df$eid[ind_sc]
  ind <- df.body$eid %in% eid_sz
  N_sz.brain <- length(eid_sz)
  N_sz.body <- nrow(df.body[ind,])
  
  print("No. SZ brain, body:")
  print(N_sz.brain)
  print(N_sz.body)
  
  eid_dep <- df$eid[ind_dep]
  ind <- df.body$eid %in% eid_dep
  N_dep.brain <- length(eid_dep)
  N_dep.body <- nrow(df.body[ind,])
  
  print("No. depression brain, body:")
  print(N_dep.brain)
  print(N_dep.body)
  
  eid_anx <- df$eid[ind_anx]
  ind <- df.body$eid %in% eid_anx
  N_anx.brain <- length(eid_anx)
  N_anx.body <- nrow(df.body[ind,])
  
  print("No. anxiety brain, body:")
  print(N_anx.brain)
  print(N_anx.body)
  
  eid_sharm <- df$eid[ind_sharm]
  ind <- df.body$eid %in% eid_sharm
  N_sharm.brain <- length(eid_sharm)
  N_sharm.body <- nrow(df.body[ind,])
  
  print("No. self harm brain, body:")
  print(N_sharm.brain)
  print(N_sharm.body)
  
  eid_bdown <- df$eid[ind_bdown]
  ind <- df.body$eid %in% eid_bdown
  N_bdown.brain <- length(eid_bdown)
  N_bdown.body <- nrow(df.body[ind,])
  
  print("No. nervous breakdown brain, body:")
  print(N_bdown.brain)
  print(N_bdown.body)
  
  diagnosis <- c("Schizophrenia", "Bipolar", "Depression","Anxiety_Panic", 
                 "Self_harm", "Nervous_breakdown")
  vBrain <- c(N_sz.brain, N_bip.brain, N_dep.brain, N_anx.brain, 
              N_sharm.brain, N_bdown.brain) 
  vBody  <- c(N_sz.body,  N_bip.body,  N_dep.body, N_anx.body, 
              N_sharm.body, N_bdown.body) 
  vBrain
  vBody
  df.tmp <- data.frame(N_brain = vBrain,
                       N_body_brain  = vBody)
  rownames(df.tmp) <- diagnosis
  write.csv(df.tmp, paste0(opath,"explore/Overview_N_diagnosis.csv"), row.names = TRUE)
  
  # 
  # tmp <- merge(df, df.body, by = "eid", all = FALSE)
  # for(i in 1:Nitems){
  #   tvar <- paste0(var,as.character(i-1))
  #   
  #   if(i == 1){
  #     ind_dep <- tmp[,tvar] %in% depression
  #   }else{
  #     ind_dep <- ind_dep | tmp[,tvar] %in% depression
  #   }
  # }
  # nrow(tmp[ind_dep,])
}


# Helpfunctions
mean_sd <- function(val,d = 1){
  o <- paste0(as.character(round(mean(val),digits = d)),"±",as.character(round(sd(val), digits = d)))
  return(o)
} 

compute_single_measure_from_two <- function(var1,var2){
  
  out <- NULL
  
  # When both measures are available
  ind <- !is.na(var1) & !is.na(var2)
  out[ind] <- (var1[ind] + var2[ind])/ 2 
  
  # When the first exists but the seond does not
  ind <- !is.na(var1) & is.na(var2)
  out[ind] <- var1[ind]
  
  # When the seond exists but the first does not
  ind <- is.na(var1) & !is.na(var2)
  out[ind] <- var2[ind]
  
  return(out)
}

compute_BMI <- function(weight, height){
  height <- height / 100 # Convert to meter
  bmi <- weight /(height*height)
  return(bmi)
}

compute_WHR <- function(waistcircumference, hipcircumference){
  WHR <- waistcircumference/hipcircumference
  return(WHR)
}

create_caucasian_noncaucasian <- function(ethnicity){
  ethnicity <- factor(ethnicity)
#  out <- NULL
  isCaucasian <- c("1","1001","1002","1003")
  for(ii in isCaucasian){
    levels(ethnicity)[levels(ethnicity) == ii] <- "Caucasian"
  }
  ethnicity_NA <- c("-1","-3")
  for(ii in ethnicity_NA){
    levels(ethnicity)[levels(ethnicity) == ii] <- NA
  } 
  nonCaucasian <- levels(ethnicity)
  nonCaucasian <- nonCaucasian[!nonCaucasian %in% c("Caucasian")]
  for(ii in nonCaucasian){
    levels(ethnicity)[levels(ethnicity) == ii] <- "nonCaucasian"
  } 
  return(ethnicity)
}

find_diagnosis <- function(df, var, Nitems, diagnosis){
  ind_diag <- NULL
  for(i in 1:Nitems){
    tvar <- paste0(var,as.character(i-1))
    
    if(i == 1){
      ind_diag <- df[,tvar] %in% diagnosis
    }
    else{
      ind_diag <- ind_diag | df[,tvar] %in% diagnosis
    }
  }
  return(ind_diag)
}

fexclude_diagnosis <- function(df, var, Nitems, exclude_diagnosis){
  ind_diag <- find_diagnosis(df, var, Nitems, exclude_diagnosis)
  df <- df[!ind_diag,]
  return(df)
}

create_dictomous_diagnosis_variable <- function(df,var,Nitems, diagnosis){
  ind_diag <- find_diagnosis(df, var, Nitems, diagnosis)
  return(factor(ind_diag, levels=c(FALSE,TRUE)))
}

ind_mean_minus_Xsd <- function(val, X){
  ind <- (val < (mean(val)-X*sd(val)))
  return(ind)
}

gg_violin <- function(df,x,y,ylab, show_legend, alpha = 0.5, size_t = 15){
  g <- ggplot(df, aes_string(x=x, y = y, fill = x)) + 
    geom_violin(alpha=alpha, show.legend = show_legend) + ylab(ylab)+ xlab("") + 
    theme_classic() + scale_color_discrete(name = "") +
    theme(text = element_text(size = size_t), legend.title = element_blank(), legend.position = c(0.86,0.95))
  return(g)
}
