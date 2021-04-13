# Function for excluding participants

exclude_all_missing <- function(df, values_considered){
  ind <- rowSums(is.na(df[,values_considered])) == ncol(df[,values_considered])
  return(ind)
}

###############################################################################
# Exclude participant based on self-reported diagnosis - cancers - exclude them all!
###############################################################################
exclude_cancer_diagnosis <- function(df,ifile){
  df.cancers <- read.csv(ifile, header = TRUE, sep ="\t")
  cid <- df.cancers$coding %in% c(-1)
  
  exclude_diagnosis <- df.cancers$coding[!cid]
  var <- "X20001.2."
  Nitems <- length(grep( "X20001.2.", names(df), value = TRUE))
  
  # Exlude participants
  df <- fexclude_diagnosis(df,var,Nitems,exclude_diagnosis)
  
  return(df)
}
  
###############################################################################
# Exclude participant based on self-reported diagnosis - noncancers 
###############################################################################
exclude_noncancer_diagnosis <- function(df,ifile, ofile)
{
  df.noncancers <- read.csv(ifile, header = FALSE, sep ="\t")
  colnames(df.noncancers) <-c("coding", "meaning", "node_id", "parent_id", "selectable")
  head(df.noncancers)
  
  drops <- c("node_id", "parent_id", "selectable")
  df.noncancers <- df.noncancers[,!names(df.noncancers) %in% drops]
  
  exclude_diagnosis <- df.noncancers$coding
  var <- "X20002.2."
  Nitems <- length(grep( "X20002.2.", names(df), value = TRUE))
  
  # Exlude participants
  df <- fexclude_diagnosis(df,var,Nitems,exclude_diagnosis)
  
  write.csv(df.noncancers, ofile, row.names = FALSE)
  
  return(df)
}

###############################################################################
# Exclude participant based on self-reported diagnosis - drop all psyhiatric diagnosis
###############################################################################
exclude_psychiatric_diagnosis <- function(df,ifile){  
  
  df.psych <- read.csv(ifile, header = FALSE, sep ="\t")
  colnames(df.psych) <-c("coding", "meaning", "node_id", "parent_id", "selectable")
  head(df.psych)
   
  var <- "X20002.2."
  Nitems <- length(grep( "X20002.2.", names(df), value = TRUE))
  exclude_diagnosis <- df.psych$coding
  df <- fexclude_diagnosis(df,var,Nitems,exclude_diagnosis)
  
  return(df)
}

exclude_based_on_euler_number <- function(df, vsd = 3, ofile_csv, ofile_pdf,fpath){
 
  # Make a copy of the data frame
  tmp <-  df
  ii <- 1
  print("Ecluded")
  repeat{
    # Exclude those below vsd*sd - iteratively until no participants are excluded
    
    tmp$euler_exclude <- ind_mean_minus_Xsd(tmp$euler_lh, vsd) | ind_mean_minus_Xsd(tmp$euler_rh, vsd)  
    
    tmp$euler_exclude <- factor(tmp$euler_exclude,
                                   levels = c(FALSE, TRUE),
                                   labels = c("Included","Excluded"))
    
    if(length(tmp$euler_exclude[tmp$euler_exclude == "Excluded"]) == 0){ break }
    
    # Keep only the included participants
    ind <- tmp$euler_exclude == "Included"
    
    print(nrow(tmp[!ind,]))
    
    tmp <- tmp[ind,]
    # if(ii==1){
    #   # Dump resulting dataframe from 1st Euler iteration to file. 
    #   write.csv(tmp,paste0(fpath,"/","UKB_BodyBrain.CLEANED.OnlyOneEulerIteration.csv"), row.names = FALSE, na="")
    # }
    ii <- ii+1
  } 
  print("Iterations used while excluding participants - based on Euler numbers")
  print(ii-1)
  
  ind <- df$eid %in% tmp$eid
  df$euler_exclude <- NULL
  df$euler_exclude[ind] <- FALSE # FALSE, is Not excluded
  df$euler_exclude[!ind] <- TRUE # TRUE, is Excluded 
  df$euler_exclude <- factor(df$euler_exclude, 
                             levels = c(FALSE,TRUE),
                             labels = c("Included","Excluded"))
    
#   # Exclude those above/below vsd*sd
#   df$euler_exclude <- ind_plus_minus_Xsd(df$euler_lh, vsd) | ind_plus_minus_Xsd(df$euler_rh, vsd)
#   
#   df$euler_exclude <- factor(df$euler_exclude,levels = c(FALSE,TRUE), labels = c("Included","Excluded"))
#   
  ##############################
  # Create exclude/include plot
  ##############################
  g_lh <- gg_violin(df, df$euler_exclude, df$euler_lh, "Left Euler number", FALSE)
  g_rh <- gg_violin(df, df$euler_exclude, df$euler_rh, "Right Euler number", FALSE)
  g <- ggarrange(g_lh,g_rh,nrow = 1, ncol = 2)
  ggsave(file=ofile_pdf,plot = g, dpi = 300, units ="in", height = 5, width = 10)
  
  ##############################
  # Create exclude/include csv file
  ##############################
  
  ind <- df$euler_exclude == "Included"
  included <- c(nrow(df[ind,]), mean_sd(df$euler_lh[ind]),mean_sd(df$euler_rh[ind]))
  
  ind <- !ind
  excluded <- c(nrow(df[ind,]), mean_sd(df$euler_lh[ind]),mean_sd(df$euler_rh[ind]))
  
  tmp <- data.frame(Inluded = included,
                    Excluded = excluded)
  rownames(tmp) <- c("N","L Euler","R Euler")
  
  write.csv(tmp, ofile_csv, row.names = TRUE, na="") 
  
  ind <- df$euler_exclude == "Included"
  df <- df[ind,]
  
  return(df)
}