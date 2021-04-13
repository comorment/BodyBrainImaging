sort_df_out <- function(df){
  
  target <- c("(Intercept)", "df[, reg_var]",
              "poly(df[, reg_var], 2)1", "poly(df[, reg_var], 2)2", 
              "sex1", "poly(age.c, 2)1", "poly(age.c, 2)2",
              "poly(age.c, 2)1:sex1", "poly(age.c, 2)2:sex1", 
              "EthnicitynonCaucasian",  "alcohol.2.0TRUE", "smoking.2.0TRUE",
              "diabetic.2.0TRUE", "high_cholesterol.2.0TRUE", "hypertension.2.0TRUE", 
              "ICV", "euler_average","Assessment_centre.2.011026", 
              "Assessment_centre.2.011027","adj. R2","deg. freedom")

  ind <- match(target,as.character(df$Covariate))[!is.na(match(target,as.character(df$Covariate)))]
  df <- df[ind,]
  
  return(df)
}

process_body_MRI <- function(df, bodyVars, ofile_base, 
                             lm_model, opath, ppath){
  # print(bodyVars)
  print(ofile_base)
  
  df <- df[complete.cases(df[,bodyVars]),]
  
  df.out <- NULL
  glist <- list()
  # out_pValue <- NULL
  for(dep_var in bodyVars){
    m <- lm_body(df, dep_var, lm_model)
    df.body <- get_output(m, df, dep_var)
    
    df.body$Covariate <- as.character(df.body$Covariate)
    df.body <- rbind(df.body,c("adj. R2",df.body$adj.r.squared[1],NA,NA,NA,NA,NA,NA,NA,NA))
    df.body <- rbind(df.body,c("deg. freedom",df.body$degrees_freedom[1],NA,NA,NA,NA,NA,NA,NA,NA))
    df.body$Covariate <- as.factor(df.body$Covariate)
    # print(head(df.body))
    
    gd <- autoplot(m, which = c(1:2)) + labs(title = format_body_name(dep_var)) + theme_classic()
    glist <- append(glist, gd)
    
    keeps <- c("Covariate","r","t-value","p_full")
    df.body <- df.body[,keeps]

    ind <- !names(df.body) %in% c("Covariate")
    names(df.body)[ind] <- paste0(dep_var,"_", names(df.body)[ind])
    
    if(is.null(df.out)){
      df.out <- df.body
    }else{
      df.out <- merge(df.out,df.body,by = "Covariate")
    }  
  } # end for
  
  
  #################################
  # Sort output file
  print(as.character(df.out$Covariate))
  df.out <- sort_df_out(df.out)
  print(as.character(df.out$Covariate))
  #################################

  write.csv(df.out, file = paste0(opath,"body/",ofile_base,".csv"), na = "", row.names = FALSE)
  
  g <- autoplot(glist)
  ggsave(file=paste0(ppath,"body/QQ_",ofile_base,".pdf"),plot = g, 
         dpi = 300, units = "in", height = 15, width = 15)
  
  # return(out_pValue)
}

process_brain_MRI_age_sex <- function(df, brainVars, lm_model,
                                      ofile, multi_center = FALSE){

  print("Included participants:")
  print(nrow(df))

  # Loop over brain structures and plot for each regressor
  df.out <- NULL
  glist <- list()
  for(dep_var in brainVars){
    m <- lm_brain(df = df, dep_var = dep_var,
                  lm_model = lm_model, multi_center =  multi_center)
    m.out <- get_output(m, df, dep_var, sexInteraction = FALSE)

    if(!c("ICV")%in% m.out$Covariate){
      m.out$Covariate <- as.character(m.out$Covariate)
      m.out <- rbind(m.out,c("ICV",NA,NA,NA,NA,NA,NA,NA,NA,NA))
      m.out$Covariate <- as.factor(m.out$Covariate)
    }
    m.out$Covariate <- as.character(m.out$Covariate)
    m.out <- rbind(m.out,c("adj. R2",m.out$adj.r.squared[1],NA,NA,NA,NA,NA,NA,NA,NA))
    m.out <- rbind(m.out,c("deg. freedom",m.out$degrees_freedom[1],NA,NA,NA,NA,NA,NA,NA,NA))
    m.out$Covariate <- as.factor(m.out$Covariate)

    gd <- autoplot(m, which = c(1,2)) + labs(title = format_structure_name(dep_var)) + theme_classic()
    glist <- append(glist, gd)

    keeps <- c("Covariate","r","t-value","p_full")
    m.out <- m.out[,keeps]
    ind <- !colnames(m.out) %in% c("Covariate")
    colnames(m.out)[ind] <- paste0(dep_var,"_", colnames(m.out)[ind])
    if(is.null(df.out)){
      df.out <- m.out
    }else{
      df.out <- merge(df.out,m.out,by = "Covariate")
    }
  } # end for brain vars
  
  #################################
  # Sort output file
  print(as.character(df.out$Covariate))
  df.out <- sort_df_out(df.out)
  print(as.character(df.out$Covariate))
  #################################

  g <- autoplot(glist)

  write.csv(df.out, file = paste0(opath,"brain/Brain_",ofile,".csv"),
            na = "", row.names = FALSE)
  ggsave(file=paste0(ppath,"brain/QQ_Brain_",ofile,".pdf"),plot = g,
         dpi = 300, units = "in", height = 15, width = 15)
}

process_brain_body_MRI <- function(df, brainVars, bodyVars, lm_model, ofile, 
                                   sexInteraction = FALSE, multi_center = FALSE,
                                   poly_regressor = FALSE, opath, ppath, wICV = TRUE){
  
  print(nrow(df))
  ind <- complete.cases(df[,bodyVars])
  df <- df[ind,]  
  print("Included participants:")
  print(nrow(df))
  
  for(reg_var in bodyVars){
    
    # Loop over brain structures and plot for each regressor
    df.out <- NULL
    df.reg_out <- NULL
    glist <- list()
    for(dep_var in brainVars){
      m <- lm_brain_body(df = df, dep_var = dep_var, reg_var = reg_var, lm_model = lm_model,
                         multi_center =  multi_center, sexInteraction = sexInteraction, 
                         poly_regressor = poly_regressor, wICV = wICV)
      m.out <- get_output(m, df, dep_var,#reg_var = reg_var, 
                          sexInteraction = sexInteraction)
      
      if(!c("ICV")%in% m.out$Covariate){
        m.out$Covariate <- as.character(m.out$Covariate)
        m.out <- rbind(m.out,c("ICV",NA,NA,NA,NA,NA,NA,NA,NA))
        m.out$Covariate <- as.factor(m.out$Covariate)
      }
      
      m.out$Covariate <- as.character(m.out$Covariate)
      m.out <- rbind(m.out,c("adj. R2",m.out$adj.r.squared[1],NA,NA,NA,NA,NA,NA))
      m.out <- rbind(m.out,c("deg. freedom",m.out$degrees_freedom[1],NA,NA,NA,NA,NA,NA))
      m.out$Covariate <- as.factor(m.out$Covariate)
      
      gd <- autoplot(m, which = c(1,2)) + labs(title = format_structure_name(dep_var)) + theme_classic()
      glist <- append(glist, gd)
      
      reg_out <- NULL
      
      if(!poly_regressor){
        ind <- m.out$Covariate ==  "df[, reg_var]"
        reg_out <- m.out[ind,]
      }else{
        ind <- m.out$Covariate == "poly(df[, reg_var], 2)1"
        reg_out <- m.out[ind,]
        names(reg_out) <- paste0(reg_var,"_",names(reg_out))
        ind <- m.out$Covariate == "poly(df[, reg_var], 2)2"
        reg_out <- cbind(reg_out, m.out[ind,])
      }
      
      # print(reg_out)
      reg_out[1] <- dep_var
      # print(reg_out)
      df.reg_out <- rbind(df.reg_out,reg_out) 
      
      keeps <- c("Covariate","r","t-value","p_full")
      m.out <- m.out[,keeps]
      ind <- !colnames(m.out) %in% c("Covariate")
      colnames(m.out)[ind] <- paste0(dep_var,"_", colnames(m.out)[ind])
      if(is.null(df.out)){
        df.out <- m.out        
      }else{
        df.out <- merge(df.out,m.out,by = "Covariate")
      }    
    } # end for brain vars
    
    # Create table that will work with the manuscript.
    df.mtmp <- df.reg_out
    ind <- append(append(append(grep("CI",names(df.mtmp)), 
                                grep("adj.r.squared",names(df.mtmp))),
                                grep("r_full",names(df.mtmp))),
                                grep("degrees_",names(df.mtmp)))
    df.mtmp <- df.mtmp[,!names(df.mtmp)%in%names(df.mtmp)[ind]]
  
    if(!poly_regressor){
      print(names(df.mtmp))  
      
            names(df.mtmp) <- c("Covariate",paste0(reg_var,"_","r"),paste0(reg_var,"_","t-value"),paste0(reg_var,"_","p-value"),paste0(reg_var,"_","p-full"))
    }else{
      print(names(df.mtmp))  
      
      ind <- names(df.mtmp)== "Covariate" 
      df.mtmp <- df.mtmp[,!ind]
      names(df.mtmp) <- c("Covariate",paste0(reg_var,"_","r"),paste0(reg_var,"_","t-value"),
                          paste0(reg_var,"_","p-value"),paste0(reg_var,"_","p-full"),
                      paste0(reg_var,"_2_","r"),paste0(reg_var,"_2_","t-value"),
                      paste0(reg_var,"_2_","p-value"),paste0(reg_var,"_2_","p-full"))
    }

    g <- autoplot(glist)
    
    #################################
    # Sort output file
    print(as.character(df.out$Covariate))
    df.out <- sort_df_out(df.out)
    print(as.character(df.out$Covariate))
    #################################
    
    # Create plots for each individual regressor over brain structures 
    write.csv(df.out, file = paste0(opath,"brain_body/FullModel/Brain_",reg_var,ofile,".csv"), 
              na = "", row.names = FALSE)
    ggsave(file=paste0(ppath,"brain_body/FullModel/QQ_Brain_",reg_var,ofile,".pdf"),plot = g, 
           dpi = 300, units = "in", height = 15, width = 15)
    
    # Rename regressor of interest output
    if(!poly_regressor){
      df.reg_out$Covariate <- gsub("_"," ",gsub("[.]"," ",df.reg_out$Covariate))
      names(df.reg_out)[names(df.reg_out) == "Covariate"] <- "Dep_var"
    
      keeps <- c("Dep_var","r_full","lCI","uCI")
      df.reg_out <- df.reg_out[,keeps]

    }else{
    names(df.reg_out) <- c("Dep_var", "r","t-value","p-value","lCI","uCI","p_full",
                         "adj.r.squared","degrees_freedom","r_full",
                         "Dep_var_2","r_2","t-value_2","p-value_2","lCI_2","uCI_2", "p_full_2",
                         "adj.r.squared_2","degrees_freedom_2","r_full_2")  
    keeps <- c("Dep_var","r_full","lCI","uCI","r_full_2","lCI_2","uCI_2")
    df.reg_out <- df.reg_out[,keeps]
    }
    # Format output names of dependent variables
    df.reg_out$Dep_var <- gsub("_"," ",gsub("[.]"," ",df.reg_out$Dep_var))
    df.reg_out$Dep_var <- format_structure_name(df.reg_out$Dep_var)
    
    write.csv(df.reg_out, file = paste0(opath,"brain_body/RegressorOfInterest/",reg_var,ofile,".csv"), 
              na = "", row.names = FALSE)
  } # end for body vars
}
