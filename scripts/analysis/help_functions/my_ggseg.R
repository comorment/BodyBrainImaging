

get_df_hemi <- function(opath,measure,cm, ifile_end, hemi){
  
  # print(paste0(opath,cm,"_",hemi,"_",measure,ifile_end))
  df <- read.csv(paste0(opath,cm,"_",hemi,"_",measure,ifile_end))
  df$Dep_var <- gsub(paste0(hemi," "),paste0(hemi,"_"),gsub(paste0(" ", measure),"",df$Dep_var))
  return(df) 
}

get_cortical_effect_plot <- function(opath,measure, cm, ifile_end, 
                                     flims = c(-0.2,0.2), linear_term = TRUE){
  d.lh <- get_df_hemi(opath,measure,cm, ifile_end, "lh")
  d.rh <- get_df_hemi(opath,measure,cm, ifile_end, "rh")
  df.cs <- rbind(d.lh,d.rh)
  df <- NULL
  if(linear_term){
    # Extract r-values of the linear term
    df <- data.frame(label = df.cs$Dep_var, measure = df.cs$r_full, stringsAsFactors = FALSE)
  }else{ 
    # Extract r-values of the quadratic term
    df <- data.frame(label = df.cs$Dep_var, measure = df.cs$r_full_2, stringsAsFactors = FALSE)
  }
  
  g <- ggseg(.data = df, mapping = aes(fill=measure), colour = "white", adapt_scales = TRUE)  +
    theme_bw() + labs(title = measure, fill = "r") +   
    theme(legend.position = "right", legend.key.size = unit(0.75, "in"),  
          legend.text = element_text(size=25), text = element_text(size = 30), axis.text = element_text(size=25)) +
    scale_fill_gradient2(limits = flims, low = "blue", mid = "lightgreen", 
                         high = "yellow", na.value = "transparent")
  return(g)
}

create_combined_cortical_effect_plot <- function(opath, cm, ifile_end, 
                                                 flims, linear_term = TRUE){
  measure <- "area" 
  p1 <- get_cortical_effect_plot(opath,measure,cm, ifile_end, flims, linear_term)
  
  measure <- "thickness" 
  p2 <- get_cortical_effect_plot(opath,measure,cm, ifile_end, flims, linear_term)
  
  measure <- "volume"
  p3 <- get_cortical_effect_plot(opath,measure,cm, ifile_end, flims, linear_term)
  
  g <- ggarrange(p1,p2,p3,nrow = 3, ncol = 1, 
                 common.legend = TRUE, legend = "bottom")
}


ggseg_combined <- function(ppath, opath, cmet, ifile_end, 
                           flims = c(-0.2, 0.2)){
  
  ofile_ending <- NULL
  if(length(cmet) == 3){
    # Full sample
    ofile_ending <- "_fullsample"
  }else{
    # Whole body MRI measures
    ofile_ending <- "_subsample"
  }
  
  for(cm in cmet){
    
    ########################
    # full body measures 
    # linear terms
    ########################
    
    gl <- create_combined_cortical_effect_plot(opath, cm, ifile_end, flims, TRUE)
    ggsave(file=paste0(ppath,"Cortical_parcellation_",cm,ofile_ending,".pdf"),
           plot = gl, dpi = 300, units = "in", height = 10,width =15)
    
    ########################
    # full body measures 
    # quadratic terms
    ########################
    
    gq <- create_combined_cortical_effect_plot(opath, cm, ifile_end, flims, FALSE)
    ggsave(file=paste0(ppath,"Cortical_parcellation_",cm,"_quadraticTerm",ofile_ending,".pdf"),
           plot = gq, dpi = 300, units = "in", height = 10,width =15)
  }
}


