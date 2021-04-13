combine_outputs_for_barplot <- function(df1,df2,clevels){
  df1$Covariate <- as.character(df1$Covariate)
  df2$Covariate <- as.character(df2$Covariate)
  df.out <- rbind(df1,df2)
  df.out$Covariate <- factor(df.out$Covariate,levels <- clevels)
  return(df.out)
} 

# Plot functions
size_t <<- 30

plot_density <- function(df, sx, sfill, x_lab, alpha = 0.2, legend_pos="none")
{  
  g <- ggplot(df, aes_string(x = sx, fill = sfill)) + geom_density(alpha=alpha) + 
    xlab(x_lab)+theme_classic() + theme(legend.position = legend_pos) + 
    scale_fill_discrete(labels = c("women","men")) + theme(text = element_text(size = 20))
  return(g)
}


# g1 <- plot_scatter_density2d(df, reg_var, dep_vars[1], "sex", c("women","men"), 
#                              alpha = alpha, psize = psize, x_lab, ylabs[1])

plot_scatter_density2d <- function(df, sx, sy, scolor, slabels, 
                                   alpha = 0.1, psize = 2, x_lab, y_lab){
  g <- ggplot(df, aes_string(x=sx, y = sy, color = scolor)) + 
    geom_point(alpha = alpha, size=psize, na.rm = TRUE) + 
    geom_density_2d(na.rm = TRUE) +
    #  geom_smooth(method = 'gam',na.rm = TRUE, se = FALSE) +
    theme_classic() + labs(y=y_lab, x=x_lab)+
    scale_color_discrete(name = scolor, labels = slabels ) + theme(text = element_text(size = 20))
  return(g)
}

plot_scatter_second_order_lm <- function(df, sx, sy, alpha = 0.1, psize = 2, x_lab, y_lab, CI = FALSE){
  g <- ggplot(df, aes_string(x=sx, y = sy, color = "sex")) + 
    geom_point(alpha = alpha, size=psize) + 
    #stat_density_2d(aes(alpha = ..level.., fill = sex), geom = "polygon", colour = "white", bins = 6, alpha = .1, show.legend = FALSE)+
    geom_smooth(method = 'lm',formula = y~poly(x,2), na.rm = TRUE, se =CI) +
    scale_color_discrete(name = "sex", labels = c("women","men")) +
    theme_classic() + 
    labs(y=y_lab, x=x_lab)+ 
    theme(text = element_text(size = 20))
  return(g)
}

plot_scatter <- function(df, sx, sy, scolor, slabels, 
                         alpha = 0.1, psize = 2, x_lab, y_lab, 
                         smethod = 'gam',s_se = FALSE){
  g <- ggplot(df, aes_string(x=sx, y = sy, color = scolor)) + 
    geom_point(alpha = alpha, size=psize, na.rm = TRUE) + 
    geom_smooth(method = smethod,na.rm = TRUE, se = s_se) +
    theme_classic() + labs(y=y_lab, x=x_lab)+
    scale_color_discrete(name = scolor, labels = slabels ) +  
    theme(text = element_text(size = 20))
  return(g)
}

plot_scatter_body <- function(df, sx, sy, alpha = 0.1, psize = 2, x_lab, y_lab, smethod = 'gam',s_se = FALSE){
  g <- ggplot(data = df, aes_string(x=sx, y = sy), colour = "plum") + 
    geom_point(alpha = alpha, size=psize, na.rm = TRUE) + 
    geom_smooth(method = smethod,na.rm = TRUE, se = s_se) +
    theme_classic() + labs(y=y_lab, x=x_lab)
  return(g)
}

plot_scatter_density <- function(df, x_1, x_2, sfill, x_lab, x_lab2,
                                 slabels,
                                 alpha = 0.2, p_alpha = 0.5, p_size = 2, ofile) 
{
  g_density <- plot_density(df, x_1, sfill, x_lab, alpha)
  
  g_scatter <- plot_scatter(df, x_2, x_1, sfill, slabels, p_alpha, p_size, x_lab2, x_lab)
  
  g <- ggarrange(g_scatter,g_density, nrow = 2, ncol = 1)
  ggsave(file=ofile,plot = g, 
         dpi = 300, units = "in", height = 5, width = 5)
}

plot_effects_single <- function(df, gtitle, order_output){
  
  
  tmp <- data.frame(estimates = df$"Cohen's d",
                    Structures = df$Covariate)
  
  tmp$Structures  <- factor(tmp$Structures,rev(order_output))
  tmp$estimates   <- as.numeric(as.character(tmp$estimates))
  
  # choose a pleasing colour scheme
  cbPalette <- rep("lightblue",nrow(tmp))
  
  # use ggplot to plot the Cohen's d estimates
  g <-   ggplot(data = tmp, mapping =  aes(x=Structures, y=estimates)) + coord_flip() +
    geom_bar(stat = "identity", position = position_dodge(), color = "lightblue", fill="lightblue", size = .3) +
    theme_classic()+ labs(y="Cohen's d", x="")+ ggtitle(gtitle)+
    geom_hline(yintercept = 0, color = "darkgray")+
    theme(text = element_text(size = size_t))+ 
    theme(plot.title = element_text(size = 30, face = "bold"))
  
  return(g)
}

plot_effects_multiple <- function(df, order_output, depVars, gtitle){
  
  estimates <- NULL
  Dependent_Variable <- NULL
  Structures <- NULL 
  for(d in depVars){
    estimates <- append(estimates, df[,paste0(d,"_","Cohen's d")]) 
    Structures <- append(Structures,as.character(df$Covariate))
    Dependent_Variable <- append(Dependent_Variable, df[,paste0(d,"_dep_var")])
  }
  
  Dependent_Variable <- gsub("_"," ",gsub(".2.0","",Dependent_Variable))
  
  tmp <- data.frame(estimates = estimates,
                    Structures = Structures,
                    Dependent_Variable = Dependent_Variable)  
  tmp$Dependent_Variable <- factor(tmp$Dependent_Variable)
  tmp$Structures <- factor(tmp$Structures,rev(order_output))
  
  
  # use ggplot to plot the Cohen's d estimates
  g <-   ggplot(data = tmp, mapping =  aes(x=Structures, y=estimates, fill = Dependent_Variable)) + coord_flip() +
    geom_bar(stat = "identity", position = position_dodge(), color = "darkgray", size = .3) +
    theme_classic()+ labs(y="Cohen's d", x="")+ ggtitle(gtitle)+
    geom_hline(yintercept = 0, color = "darkgray")+
    theme(text = element_text(size = size_t))+ 
    theme(plot.title = element_text(size = 30, face = "bold"))
  
  return(g)  
}

plots_demographics <- function(ofile,df, sfill = "sex", bodyMRI = FALSE) 
{
  if(!bodyMRI){
    g_a <- plot_density(df,   "age.2.0", sfill, "Age (years)")
    g_h <- plot_density(df,   "height.2.0", sfill, "Height (cm)")
    g_w <- plot_density(df,   "weight.2.0", sfill, "Weight (kg)")
    g_wc <- plot_density(df,  "waist_circumference.2.0", sfill, "Waist circumference (cm)")
    g_hc <- plot_density(df,  "hip_circumference.2.0", sfill, "Hip circumference (cm)")
    g_BMI <- plot_density(df, "BMI.2.0", sfill, "BMI")
    g_WHR <- plot_density(df, "WHR.2.0", sfill, "WHR")
    
    g <- ggarrange(g_a,g_h,g_w,g_wc,g_hc,
                   g_BMI,g_WHR,
                   nrow = 3, ncol = 3, common.legend = TRUE, legend = "top")
    
  }else{
    g_a <- plot_density(df,   "age.2.0", sfill, "Age (years)")
    g_h <- plot_density(df,   "height.2.0", sfill, "Height (cm)")
    g_w <- plot_density(df,   "weight.2.0", sfill, "Weight (kg)")
    g_wc <- plot_density(df,  "waist_circumference.2.0", sfill, "Waist circumference (cm)")
    g_hc <- plot_density(df,  "hip_circumference.2.0", sfill, "Hip circumference (cm)")
    g_BMI <- plot_density(df, "BMI.2.0", sfill, "BMI")
    g_WHR <- plot_density(df, "WHR.2.0", sfill, "WHR")
    g_Liver <- plot_density(df, "Liver_PDFF.2.0", sfill, "Liver PDFF (%)")
    g_VAT <- plot_density(df, "VAT.2.0", sfill, "VAT (L)")
    g_ASAT <- plot_density(df, "ASAT.2.0", sfill, "ASAT (L)")
    g_VAT_ASAT <- plot_density(df, "VAT_ASAT.2.0", sfill, "VAT+ASAT (L)")
    g_MFI <- plot_density(df, "MFI.2.0", sfill, "MFI (%)")
    g_TTMV <- plot_density(df, "TTMV.2.0", sfill, "TTMV (L)")
    
    g <- ggarrange(g_a,g_h,g_w,g_wc,g_hc,
                   g_BMI,g_WHR, 
                   g_Liver, g_VAT, 
                   g_ASAT, g_VAT_ASAT, g_MFI, g_TTMV, 
                   nrow = 4, ncol = 4, common.legend = TRUE, legend = "top")
  }
  
  ggsave(file=ofile,plot = g, 
         dpi = 300, units = "in", height = 20, width = 20)
}

plots_brain_MRI <- function(ofile,df, sfill = "sex") 
{
  
  g1 <- plot_density(df,  "MeanCorticalThickness", sfill, "Cortical thickness")
  g2 <- plot_density(df,  "MeanWhiteSurfArea_scaled", sfill, "Surface area (white)")
  g3 <- plot_density(df,  "CortexVol_ml", sfill, "Cortical GM")
  g4 <- plot_density(df,  "Total.Cerebellum.Cortex_ml", sfill, "Cerebellum GM")
  g5 <- plot_density(df,  "Brain.Stem_ml", sfill, "Brain stem")
  g6 <- plot_density(df,  "CorticalWhiteMatterVol_ml", sfill, "Cortical WM")
  g7 <- plot_density(df,  "Total.Cerebellum.White.Matter_ml", sfill, "Cerebellum WM")
  g8 <- plot_density(df,  "CSF_ml", sfill, "CSF")
  g9 <- plot_density(df,  "Total.Lateral.Ventricle_ml", sfill, "Lateral ventricle")
  g10 <- plot_density(df, "X3rd.Ventricle_ml", sfill, "3rd ventricle")
  g11 <- plot_density(df, "Total.Thalamus.Proper_ml", sfill, "Thalamus")
  g12 <- plot_density(df, "Total.Hippocampus_ml", sfill, "Hippocampus")
  g13 <- plot_density(df, "Total.Amygdala_ml", sfill, "Amygdala")
  g14 <- plot_density(df, "Total.Accumbens.area_ml", sfill, "Accumbens")
  g15 <- plot_density(df, "Total.Caudate_ml", sfill, "Caudate")
  g16 <- plot_density(df, "Total.Putamen_ml", sfill, "Putamen")
  g17 <- plot_density(df, "Total.Pallidum_ml", sfill, "Pallidum")
  g18 <- plot_density(df, "EstimatedTotalIntraCranialVol_ml", sfill, "ICV")
  
  g <- ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,
                 g10,g11,g12,g13,g14,g15,g16,g17,g18,
                 nrow = 5, ncol = 4, common.legend = TRUE, legend = "top")
  
  ggsave(file=ofile,plot = g, 
         dpi = 300, units = "in", height = 20,width =15)
}


plots_brain_MRI_followup <- function(ofile,df, sfill = "sex", hemi, measure, cparc){
  
  if(measure == "thickness"){
    for(t in cparc){
      ovar <- paste0(t,"_s")
      df[,ovar] <- df[,paste0(paste0(paste0(paste0(hemi,"_"),t),"_"),measure)]
    }
  }else{ # scale by 1000 to obtain ml / mm2 measures
    for(t in cparc){
      ovar <- paste0(t,"_s")
      df[,ovar] <- df[,paste0(paste0(paste0(paste0(hemi,"_"),t),"_"),measure)]/1000
    }
  }
  g1 <- plot_density(df,  "bankssts_s", sfill, "banks superior temporal")
  g2 <- plot_density(df,  "caudalanteriorcingulate_s", sfill, "caudal anterior cinculate")
  g3 <- plot_density(df,  "caudalmiddlefrontal_s", sfill, "caudal middle frontal")
  g4 <- plot_density(df,  "cuneus_s", sfill, "cuneus")
  g5 <- plot_density(df,  "entorhinal_s", sfill, "entorhinal")
  g6 <- plot_density(df,  "fusiform_s", sfill, "fusiform")
  g7 <- plot_density(df,  "inferiorparietal_s", sfill, "inferior parietal")
  g8 <- plot_density(df,  "inferiortemporal_s", sfill, "inferior temporal")
  g9 <- plot_density(df,  "isthmuscingulate_s", sfill, "isthmus cingulate")
  g10 <- plot_density(df, "lateraloccipital_s", sfill, "lateral occipital")
  g11 <- plot_density(df, "lateralorbitofrontal_s", sfill, "lateral orbitofrontal")
  g12 <- plot_density(df, "lingual_s", sfill, "lingual")
  g13 <- plot_density(df, "medialorbitofrontal_s", sfill, "medial orbito frontal")
  g14 <- plot_density(df, "middletemporal_s", sfill, "middle temporal")
  g15 <- plot_density(df, "parahippocampal_s", sfill, "parahippocampal")
  g16 <- plot_density(df, "paracentral_s", sfill, "para central")
  g17 <- plot_density(df, "parsopercularis_s", sfill, "pars opercularis")
  g18 <- plot_density(df, "parsorbitalis_s", sfill, "pars orbitalis")
  g19 <- plot_density(df, "parstriangularis_s", sfill, "pars triangularis")
  g20 <- plot_density(df, "pericalcarine_s", sfill, "pericalcarine")
  g21 <- plot_density(df, "posteriorcingulate_s", sfill, "posterior cingulate")
  g22 <- plot_density(df, "precentral_s", sfill, "pre central")
  g23 <- plot_density(df, "precuneus_s", sfill, "precuneus")
  g24 <- plot_density(df, "rostralanteriorcingulate_s", sfill, "rostral anterior cingulate")
  g25 <- plot_density(df, "rostralmiddlefrontal_s", sfill, "rostral middle frontal")
  g26 <- plot_density(df, "superiorfrontal_s", sfill, "superior frontal")
  g27 <- plot_density(df, "superiorparietal_s", sfill, "superior parietal")
  g28 <- plot_density(df, "superiortemporal_s", sfill, "superior temporal")
  g29 <- plot_density(df, "supramarginal_s", sfill, "supramarginal")
  g30 <- plot_density(df, "frontalpole_s", sfill, "frontal pole")
  g31 <- plot_density(df, "temporalpole_s", sfill, "temporal pole")
  g32 <- plot_density(df, "transversetemporal_s", sfill, "transverse temporal")
  g33 <- plot_density(df, "insula_s", sfill, "insula")
  g34 <- plot_density(df, "postcentral_s", sfill, "post central")
  
  g <- ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,
                 g10,g11,g12,g13,g14,g15,g16,
                 g17,g18,g19,g20,g21,g22,g23,
                 g24,g25,g26,g27,g28,g29,g30,
                 g31,g32,g33,g34,nrow = 5, ncol = 7,
                 common.legend = TRUE, legend = "top")
  
  ggsave(file=ofile,plot = g, 
         dpi = 300, units = "in", height = 20,width =15)
}

plot_results_barplot <- function(df,plot_title,no_covariates, firstOrder = TRUE, colorPalette = NULL, left_right = FALSE){
  
  sortedStructures <- c("Pallidum","Putamen","Caudate","Accumbens",
                        "Amygdala","Hippocampus","Thalamus",
                        "3rd Ventricle","Lateral Ventricle",
                        "CSF","Cerebellum WM","Cortical WM","Brain Stem",
                        "Cerebellum GM","Cortical GM","Surface area (white)","Cortical thickness")
  
  if(left_right){
    sortedStructures <- c("Pallidum","Putamen","Caudate","Accumbens",
                          "Amygdala","Hippocampus","Thalamus", "Lateral Ventricle",
                          "Cerebellum WM","Cortical WM", "Cerebellum GM",
                          "Cortical GM","Surface area (white)","Cortical thickness")
  }
  df$Dep_var <- factor(df$Dep_var, levels = sortedStructures)
  
  names(df)[names(df) == "Covariate"] <- "Regressor"
  
  if(is.null(colorPalette)){
    colorPalette <- NULL
    if(no_covariates == 3){
      colorPalette <- c("plum", "purple","magenta")
    }else if(no_covariates == 5){
      #colorPalette <- c("springgreen4","aquamarine","plum", "purple","magenta")
      colforPalette <- c("springgreen4","aquamarine","lightblue","turquoise3","dodgerblue3")
    }else if(no_covariates == 6){
      colorPalette <- c("cyan","turquoise3","paleturquoise3","lightcyan", "lightslateblue","blue")
    }else if(no_covariates == 8){
      colorPalette <- c("springgreen4","aquamarine",
                        "ligfhtblue","turquoise3","dodgerblue3","plum", "purple","magenta")
    }else{
      stop("In sults_barplot: The number of covariates must be 3,5, 6 or 8")
    }
  }
  g <- NULL
  if(firstOrder){
    g <- ggplot(data = df, aes(x=Dep_var, y=r_full, fill = Regressor)) + coord_flip() + 
      geom_bar(stat = "identity", position = position_dodge(), color = "black", size = .3) +
      geom_errorbar(aes(ymin=lCI, ymax = uCI), size=.3, width=.2, position = position_dodge(.9)) +
      ylab("Partial correlation coefficient, r") + ggtitle(plot_title) +
      xlab("") + theme_classic() + geom_hline(yintercept = 0) + 
      scale_fill_manual(values=colorPalette)  +guides(fill = guide_legend(reverse = TRUE))+ 
      theme(text = element_text(size = 30))
  }else{
    g <- ggplot(data = df, aes(x=Dep_var, y=r_full_2, fill = Regressor)) + coord_flip() + 
      geom_bar(stat = "identity", position = position_dodge(), color = "black", size = .3) +
      geom_errorbar(aes(ymin=lCI_2, ymax = uCI_2), size=.3, width=.2, position = position_dodge(.9)) +
      ylab("Partial correlation coefficient, r") + ggtitle(plot_title) +
      xlab("") + theme_classic() + geom_hline(yintercept = 0) + 
      scale_fill_manual(values=colorPalette)  +guides(fill = guide_legend(reverse = TRUE))+ 
      theme(text = element_text(size = 30))
    
  }
  return(g)
}


plot_results_barplot_whole_body_all <- function(opath){
  
  ###########################
  # Linear terms all
  ###########################
  
  # model ii-a
  dft <- get_whole_body_barplot_df(opath,1)
  ind <- dft$Covariate %in% c("BMI","WHR","WC")
  df1 <- dft[ind,]
  df2 <- dft[!ind,]
  
  g1a <- plot_results_barplot(df1,"Model 2a",length(levels(factor(df1$Covariate))), firstOrder = TRUE)
  g2a <- plot_results_barplot(df2,"Model 2a",length(levels(factor(df2$Covariate))), firstOrder = TRUE)
  
  # model ii-b
  dft <- get_whole_body_barplot_df(opath,2)
  ind <- dft$Covariate %in% c("BMI","WHR","WC")
  df1 <- dft[ind,]
  df2 <- dft[!ind,]
  
  # Linear term
  g1b <- plot_results_barplot(df1,"Model 2b",length(levels(factor(df1$Covariate))),firstOrder = TRUE)
  g2b <- plot_results_barplot(df2,"Model 2b",length(levels(factor(df2$Covariate))),firstOrder = TRUE)
  
  # nonlinear term
  g1b_nl <- plot_results_barplot(df1,"Model 2b",length(levels(factor(df1$Covariate))),firstOrder = FALSE)
  g2b_nl <- plot_results_barplot(df2,"Model 2b",length(levels(factor(df2$Covariate))),firstOrder = FALSE)
  
  # model iii
  dft <- get_whole_body_barplot_df(opath,3)
  ind <- dft$Covariate %in% c("BMI","WHR","WC")
  df1 <- dft[ind,]
  df2 <- dft[!ind,]
  
  g1c <- plot_results_barplot(df1,"Model 2c",length(levels(factor(df1$Covariate))),firstOrder = TRUE)
  g2c <- plot_results_barplot(df2,"Model 2c",length(levels(factor(df2$Covariate))),firstOrder = TRUE)
  
  g1c_nl <- plot_results_barplot(df1,"Model 2c",length(levels(factor(df1$Covariate))),firstOrder = FALSE)
  g2c_nl <- plot_results_barplot(df2,"Model 2c",length(levels(factor(df2$Covariate))),firstOrder = FALSE)
  
  ###########################
  # Combine plots
  ###########################
  # Linear terms all
  ###########################
  g1 <- ggarrange(g1a,g1b,g1c, nrow = 1, ncol = 3, common.legend = TRUE, legend = "right")
  ggsave(file=paste0(ppath,"FigureS24a_wholeBodyMRI_anthropometric_linear_term.pdf"),plot = g1,
         dpi = 300, units = "in", height = 15, width = 36)
  
  g2 <- ggarrange(g2a,g2b,g2c, nrow = 1, ncol = 3, common.legend = TRUE, legend = "right")
  ggsave(file=paste0(ppath,"FigureS25a_wholeBodyMRI_metrics_linear_term.pdf"),plot = g2,
         dpi = 300, units = "in", height = 15, width = 36)
  
  ###########################
  # Nonlinear terms all
  ###########################
  g1 <- ggarrange(g1b_nl,g1c_nl, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")
  ggsave(file=paste0(ppath,"FigureS24b_wholeBodyMRI_anthropometric_quadric_term.pdf"),plot = g1,
         dpi = 300, units = "in", height = 15, width = 24)
  
  g2 <- ggarrange(g2b_nl,g2c_nl, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")
  ggsave(file=paste0(ppath,"FigureS25b_wholeBodyMRI_metrics_quadric_term.pdf"),plot = g2,
         dpi = 300, units = "in", height = 15, width = 24)
}

plot_results_barplot_subset <- function(df,plot_title,no_covariates,sortedStructures){
  
  df$Dep_var <- factor(df$Dep_var, levels = sortedStructures)
  
  names(df)[names(df) == "Covariate"] <- "Regressor"
  
  
  colorPalette <- NULL
  if(no_covariates == 3){
    colorPalette <- c("plum", "purple","magenta")
  }else if(no_covariates == 5){
    colorPalette <- c("darkslateblue","aquamarine",
                      "lightblue","turquoise3","dodgerblue3")
  }else if(no_covariates == 6){
    colorPalette <- c("plum", "purple","magenta",
                      "darkslateblue","lightblue","dodgerblue3")
  }else{
    stop("In plot_results_barplot: The number of covariates must be 3 or 5")
  }
  
  g <- ggplot(data = df, aes(x=Dep_var, y=r_full, fill = Regressor)) + coord_flip() + 
    geom_bar(stat = "identity", position = position_dodge(), color = "black", size = .3) +
    geom_errorbar(aes(ymin=lCI, ymax = uCI), size=.3, width=.2, position = position_dodge(.9)) +
    ylab("Partial correlation coefficient, r") + ggtitle(plot_title) +
    xlab("") + theme_classic() + geom_hline(yintercept = 0) + 
    scale_fill_manual(values=colorPalette)  +guides(fill = guide_legend(reverse = TRUE))+ 
    theme(text = element_text(size = 30)) 
  return(g)
}


plot_scatter_sex <- function(df, reg_var, x_lab, dep_vars, ylabs,
                             alpha = 0.05, psize = 1){
  
  for(d in dep_vars){
    
    if(!d %in% c("MeanCorticalThickness","MeanWhiteSurfArea"))
    {
      df[,d] <- df[,d]/1000      
    }else if(d == "MeanWhiteSurfArea"){
      df[,d] <- df[,d]/100          
    }
  }
  
  
  g1 <- plot_scatter_density2d(df, reg_var, dep_vars[1], "sex", c("women","men"), 
                               alpha = alpha, psize = psize, x_lab, ylabs[1])
  
  g2 <- plot_scatter_density2d(df, reg_var, dep_vars[2], "sex", c("women","men"), 
                               alpha = alpha, psize = psize, x_lab, ylabs[2])
  
  g3 <- plot_scatter_density2d(df, reg_var, dep_vars[3], "sex", c("women","men"), 
                               alpha = alpha, psize = psize, x_lab, ylabs[3])
  
  g4 <- plot_scatter_density2d(df, reg_var, dep_vars[4], "sex", c("women","men"), 
                               alpha = alpha, psize = psize, x_lab, ylabs[4])
  
  g <- NULL
  if(length(ylabs)==5){
    g5 <- plot_scatter_density2d(df, reg_var, dep_vars[5], "sex", c("women","men"), 
                                 alpha = 0.1, psize = 1, x_lab, ylabs[5])
    g <- ggarrange(g1,g2,g3,g4,g5, nrow = 1, ncol = 5,
                   common.legend = TRUE, legend = "top")  
  }else{
    g <- ggarrange(g1,g2,g3,g4, nrow = 1, ncol = 4,
                   common.legend = TRUE, legend = "top")
    
  }
  return(g)
}

plot_catter_body_age <- function(df, reg_var, x_lab, dep_vars,
                                 alpha = 0.1, psize = 1){
  
  y_lab <- gsub(".2.0","",dep_vars)
  
  df$log_var <- log(df[,dep_vars[1]])
  g1 <- plot_scatter(df, reg_var, "log_var", "sex", c("women","men"), 
                     alpha = alpha, psize = psize, x_lab, paste("log of", y_lab[1]))
  
  df$log_var <- log(df[,dep_vars[2]])
  g2 <- plot_scatter(df, reg_var, "log_var", "sex", c("women","men"), 
                     alpha = alpha, psize = psize, x_lab, paste("log of", y_lab[2]))
  
  df$log_var <- log(df[,dep_vars[3]])
  g3 <- plot_scatter(df, reg_var, "log_var", "sex", c("women","men"), 
                     alpha = alpha, psize = psize, x_lab, paste("log of", gsub("_"," ",y_lab[3])))
  
  if (length(dep_vars) > 3){
    
    # Replace 0 by 0.1
    ind <- df$Liver_PDFF.2.0 == 0 & !is.na(df$Liver_PDFF.2.0)
    df$Liver_PDFF.2.0[ind] <- 0.1
    
    df$log_var <- log(df[,dep_vars[4]])
    g4 <- plot_scatter(df, reg_var, "log_var", "sex", c("women","men"), 
                       alpha = alpha, psize = psize, x_lab, paste("log of", gsub("_"," ",y_lab[4])))
    
    df$log_var <- log(df[,dep_vars[5]])
    g5 <- plot_scatter(df, reg_var, "log_var", "sex", c("women","men"), 
                       alpha = alpha, psize = psize, x_lab, paste("log of", y_lab[5]))
    
    df$log_var <- log(df[,dep_vars[6]])
    g6 <- plot_scatter(df, reg_var, dep_vars[6], "sex", c("women","men"), 
                       alpha = alpha, psize = psize, x_lab, paste("log of", y_lab[6]))
    
    df$log_var <- log(df[,dep_vars[7]])
    g7 <- plot_scatter(df, reg_var, dep_vars[7], "sex", c("women","men"), 
                       alpha = alpha, psize = psize, x_lab, paste("log of", gsub("_","+",y_lab[7])))
    
    df$log_var <- log(df[,dep_vars[8]])
    g8 <- plot_scatter(df, reg_var, dep_vars[8], "sex", c("women","men"), 
                       alpha = alpha, psize = psize, x_lab, paste("log of", y_lab[8]))
    g <- ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,
                   ncol = 3, nrow=3, common.legend = TRUE, legend = "top")
    
  }else{
    g <- ggarrange(g1,g2,g3,
                   ncol = 3, nrow=1, common.legend = TRUE, legend = "top")
  }
  
  
  return(g)
}

plots_body_comp_scatter_loess_age_sex <- function(ofile,df, bodyMRI = FALSE, sx = "age.2.0", 
                                                  sg = "sex", sgg = c("women","men"),smethod = "loess",
                                                  s_se = TRUE, sage = "Age"){
  
  g1 <- plot_scatter(df, sx,"BMI.2.0",sg, sgg, 0.1, 2, sage, "BMI", smethod, s_se) 
  g2 <- plot_scatter(df, sx,"WHR.2.0",sg, sgg, 0.1, 2, sage, "WHR", smethod, s_se) 
  g3 <- plot_scatter(df, sx,"waist_circumference.2.0",sg, sgg, 0.1, 2, sage, "Waist circumference", smethod, s_se) 
  
  if(bodyMRI){
    g4 <- plot_scatter(df, sx,"Liver_PDFF.2.0",sg, sgg, 0.1, 2, sage, "Liver PDFF", smethod, s_se) 
    g5 <- plot_scatter(df, sx,"VAT.2.0",sg, sgg, 0.1, 2, sage, "VAT", smethod, s_se) 
    g6 <- plot_scatter(df, sx,"ASAT.2.0",sg, sgg, 0.1, 2, sage, "ASAT", smethod, s_se) 
    g7 <- plot_scatter(df, sx,"VAT_ASAT.2.0",sg, sgg, 0.1, 2, sage, "VAT+ASAT", smethod, s_se) 
    g8 <- plot_scatter(df, sx,"MFI.2.0",sg, sgg, 0.1, 2, sage, "MFI", smethod, s_se)
    g9 <- plot_scatter(df, sx,"TTMV.2.0",sg, sgg, 0.1, 2, sage, "TTMV", smethod, s_se)
    
    g <- ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,
                   nrow = 3, ncol = 3, common.legend = TRUE, legend = "top")
    ggsave(file=ofile,plot = g, 
           dpi = 300, units = "in", height = 12, width =15)
  }else{
    g <- ggarrange(g1,g2,g3,
                   nrow = 1, ncol = 3, common.legend = TRUE, legend = "top")
    ggsave(file=ofile,plot = g, 
           dpi = 300, units = "in", height = 4,width =12)
  }
}


plots_brain_MRI_scatter_loess_age_sex <- function(ofile,df){
  
  sx <- "age.2.0"
  sg <- "sex"
  sgg <- c("women","men")
  smethod = "loess"
  s_se = TRUE
  sage = "Age"
  
  g1 <- plot_scatter(df, sx,"MeanCorticalThickness",sg, sgg, 0.1, 2, sage, "Cortical thickness", smethod, s_se)
  g2 <- plot_scatter(df, sx,"MeanWhiteSurfArea_scaled",sg, sgg, 0.1, 2, sage, "Surface area (white)", smethod, s_se)
  g3 <- plot_scatter(df, sx,"CortexVol_ml",sg, sgg, 0.1, 2, sage, "Cortical GM", smethod, s_se)
  g4 <- plot_scatter(df, sx,"Total.Cerebellum.Cortex_ml",sg, sgg, 0.1, 2, sage, "Cerebellum GM", smethod, s_se)
  g5 <- plot_scatter(df, sx,"Brain.Stem_ml",sg, sgg, 0.1, 2, sage, "Brain stem", smethod, s_se)
  g6 <- plot_scatter(df, sx,"CorticalWhiteMatterVol_ml",sg, sgg, 0.1, 2, sage, "Cortical WM", smethod, s_se)
  g7 <- plot_scatter(df, sx,"Total.Cerebellum.White.Matter_ml",sg, sgg, 0.1, 2, sage, "Cerebellum WM", smethod, s_se)
  g8 <- plot_scatter(df, sx,"CSF_ml",sg, sgg, 0.1, 2, sage, "CSF", smethod, s_se)
  g9 <- plot_scatter(df, sx,"Total.Lateral.Ventricle_ml",sg, sgg, 0.1, 2, sage, "Lateral ventricle", smethod, s_se)
  g10 <- plot_scatter(df, sx,"X3rd.Ventricle_ml",sg, sgg, 0.1, 2, sage, "3rd ventricle", smethod, s_se)
  g11 <- plot_scatter(df, sx,"Total.Thalamus.Proper_ml",sg, sgg, 0.1, 2, sage, "Thalamus", smethod, s_se)
  g12 <- plot_scatter(df, sx,"Total.Hippocampus_ml",sg, sgg, 0.1, 2, sage, "Hippocampus", smethod, s_se)
  g13 <- plot_scatter(df, sx,"Total.Amygdala_ml",sg, sgg, 0.1, 2, sage, "Amygdala", smethod, s_se)
  g14 <- plot_scatter(df, sx,"Total.Accumbens.area_ml",sg, sgg, 0.1, 2, sage, "Accumbens", smethod, s_se)
  g15 <- plot_scatter(df, sx,"Total.Caudate_ml",sg, sgg, 0.1, 2, sage, "Caudate", smethod, s_se)
  g16 <- plot_scatter(df, sx,"Total.Putamen_ml",sg, sgg, 0.1, 2, sage, "Putamen", smethod, s_se)
  g17 <- plot_scatter(df, sx,"Total.Pallidum_ml",sg, sgg, 0.1, 2, sage, "Pallidum", smethod, s_se)
  
  g <- ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,
                 g10,g11,g12,g13,g14,g15,g16,g17,
                 nrow = 5, ncol = 4, common.legend = TRUE, legend = "top")
  
  ggsave(file=ofile,plot = g,
         dpi = 300, units = "in", height = 20,width =15)
  
}

plots_brain_MRI_scatter_loess_body_comp <- function(ofile,df, val,bodyVars,smethod = "loess",s_se = TRUE){
  
  df <- df[complete.cases(df[,bodyVars]),]
  
  ofile <- paste0(ofile,"_n",as.character(nrow(df)),".pdf")
  
  sage <- gsub("_","+",gsub(".2.0","",val))
  sage <- gsub("Liver+PDFF", "Liver PDFF",sage)
  
  
  sg <- "sex"
  sgg <- c("women","men")
  
  g1 <- plot_scatter(df, val,"MeanCorticalThickness", sg, sgg, 0.1, 2, sage, "Cortical thickness", smethod, s_se) 
  g2 <- plot_scatter(df, val,"MeanWhiteSurfArea_scaled",sg, sgg, 0.1, 2, sage, "Surface area (white)", smethod, s_se) 
  g3 <- plot_scatter(df, val,"CortexVol_ml",sg, sgg, 0.1, 2, sage, "Cortical GM", smethod, s_se) 
  g4 <- plot_scatter(df, val,"Total.Cerebellum.Cortex_ml",sg, sgg,  0.1, 2, sage, "Cerebellum GM", smethod, s_se) 
  g5 <- plot_scatter(df, val,"Brain.Stem_ml",sg, sgg,  0.1, 2, sage, "Brain stem", smethod, s_se) 
  g6 <- plot_scatter(df, val,"CorticalWhiteMatterVol_ml",sg, sgg,  0.1, 2, sage, "Cortical WM", smethod, s_se) 
  g7 <- plot_scatter(df, val,"Total.Cerebellum.White.Matter_ml",sg, sgg,  0.1, 2, sage, "Cerebellum WM", smethod, s_se) 
  g8 <- plot_scatter(df, val,"CSF_ml",sg, sgg,  0.1, 2, sage, "CSF", smethod, s_se) 
  g9 <- plot_scatter(df, val,"Total.Lateral.Ventricle_ml",sg, sgg, 0.1, 2, sage, "Lateral ventricle", smethod, s_se) 
  g10 <- plot_scatter(df, val,"X3rd.Ventricle_ml", sg, sgg, 0.1, 2, sage, "3rd ventricle", smethod, s_se) 
  g11 <- plot_scatter(df, val,"Total.Thalamus.Proper_ml",sg, sgg,  0.1, 2, sage, "Thalamus", smethod, s_se) 
  g12 <- plot_scatter(df, val,"Total.Hippocampus_ml", sg, sgg, 0.1, 2, sage, "Hippocampus", smethod, s_se) 
  g13 <- plot_scatter(df, val,"Total.Amygdala_ml",sg, sgg, 0.1, 2, sage, "Amygdala", smethod, s_se) 
  g14 <- plot_scatter(df, val,"Total.Accumbens.area_ml", sg, sgg, 0.1, 2, sage, "Accumbens", smethod, s_se) 
  g15 <- plot_scatter(df, val,"Total.Caudate_ml",sg, sgg,  0.1, 2, sage, "Caudate", smethod, s_se) 
  g16 <- plot_scatter(df, val,"Total.Putamen_ml",sg, sgg,  0.1, 2, sage, "Putamen", smethod, s_se) 
  g17 <- plot_scatter(df, val,"Total.Pallidum_ml", sg, sgg, 0.1, 2, sage, "Pallidum", smethod, s_se) 
  
  g <- ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,
                 g10,g11,g12,g13,g14,g15,g16,g17,
                 nrow = 5, ncol = 4, common.legend = TRUE, legend = "top")
  
  ggsave(file=ofile,plot = g, 
         dpi = 300, units = "in", height = 20,width =15)
  
} 

plots_brain_MRI_body_composition_scatter_loess <- function(ofile, df, val,bodyVars,
                                                           smethod = "loess",
                                                           s_se = TRUE, subsample = FALSE){
  
  sage <- gsub("_","+",gsub(".2.0","",val))
  if(subsample){
    df <- df[complete.cases(df[,bodyVars]),]
    sage <- gsub("Liver+PDFF", "Liver PDFF",sage)
    
  }
  
  ofile <- paste0(ofile,"_n",as.character(nrow(df)),".pdf")
  
  g1 <- plot_scatter_body(df, val,"MeanCorticalThickness",    0.1, 2, sage, "Cortical thickness", smethod, s_se) 
  g2 <- plot_scatter_body(df, val,"MeanWhiteSurfArea_scaled", 0.1, 2, sage, "Surface area (white)", smethod, s_se) 
  g3 <- plot_scatter_body(df, val,"CortexVol_ml",             0.1, 2, sage, "Cortical GM", smethod, s_se) 
  g4 <- plot_scatter_body(df, val,"Total.Cerebellum.Cortex_ml", 0.1, 2, sage, "Cerebellum GM", smethod, s_se) 
  g5 <- plot_scatter_body(df, val,"Brain.Stem_ml",            0.1, 2, sage, "Brain stem", smethod, s_se) 
  g6 <- plot_scatter_body(df, val,"CorticalWhiteMatterVol_ml", 0.1, 2, sage, "Cortical WM", smethod, s_se) 
  g7 <- plot_scatter_body(df, val,"Total.Cerebellum.White.Matter_ml", 0.1, 2, sage, "Cerebellum WM", smethod, s_se) 
  g8 <- plot_scatter_body(df, val,"CSF_ml",                   0.1, 2, sage, "CSF", smethod, s_se) 
  g9 <- plot_scatter_body(df, val,"Total.Lateral.Ventricle_ml", 0.1, 2, sage, "Lateral ventricle", smethod, s_se) 
  g10 <- plot_scatter_body(df, val,"X3rd.Ventricle_ml",       0.1, 2, sage, "3rd ventricle", smethod, s_se) 
  g11 <- plot_scatter_body(df, val,"Total.Thalamus.Proper_ml", 0.1, 2, sage, "Thalamus", smethod, s_se) 
  g12 <- plot_scatter_body(df, val,"Total.Hippocampus_ml",    0.1, 2, sage, "Hippocampus", smethod, s_se) 
  g13 <- plot_scatter_body(df, val,"Total.Amygdala_ml",       0.1, 2, sage, "Amygdala", smethod, s_se) 
  g14 <- plot_scatter_body(df, val,"Total.Accumbens.area_ml", 0.1, 2, sage, "Accumbens", smethod, s_se) 
  g15 <- plot_scatter_body(df, val,"Total.Caudate_ml",        0.1, 2, sage, "Caudate", smethod, s_se) 
  g16 <- plot_scatter_body(df, val,"Total.Putamen_ml",        0.1, 2, sage, "Putamen", smethod, s_se) 
  g17 <- plot_scatter_body(df, val,"Total.Pallidum_ml",       0.1, 2, sage, "Pallidum", smethod, s_se) 
  
  g <- ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,
                 g10,g11,g12,g13,g14,g15,g16,g17,
                 nrow = 5, ncol = 4, common.legend = TRUE, legend = "top")
  
  ggsave(file=ofile,plot = g, 
         dpi = 300, units = "in", height = 20,width =15)
  
} 

plots_brain_MRI_figure1 <- function(ofile,df,val){
  xLab <- gsub("\\+"," ",gsub("_"," ",gsub(".2.0","",val)))
  ofile <- paste0(ofile,"_n",as.character(nrow(df)),".pdf")
  
  alpha <- 0.05
  psize <- 2
  
  if(val == "WHR.2.0")
  {
    g2 <- plot_scatter_second_order_lm(df, val,"Total.Cerebellum.Cortex_ml", 
                                       alpha, psize, xLab, "Cerebellum GM", CI = TRUE) 
    g1 <- plot_scatter_second_order_lm(df, val,"CortexVol_ml",               
                                       alpha, psize, xLab, "Cortical GM", CI = TRUE)
    g3 <- plot_scatter_second_order_lm(df, val,"Brain.Stem_ml",              
                                       alpha, psize, xLab, "Brain stem", CI = TRUE) 
  }
  else if(val == "BMI.2.0") # BMI
  {
    # g2 <- plot_scatter_second_order_lm(df, val,"Total.Cerebellum.Cortex_ml", 
    #                                    alpha, psize, xLab, "Cerebellum GM", CI = TRUE) 
    g1 <- plot_scatter_second_order_lm(df, val,"MeanCorticalThickness",     
                                       alpha, psize, xLab, "Cortical thickness", CI = TRUE)
    g2 <- plot_scatter_second_order_lm(df, val,"Total.Accumbens.area_ml",   
                                       alpha, psize, xLab, "Accumbens", CI = TRUE) 
  }
  else{ #waist circumference
    g2 <- plot_scatter_second_order_lm(df, val,"Total.Cerebellum.Cortex_ml", 
                                       alpha, psize, xLab, "Cerebellum GM", CI = TRUE) 
    g1 <- plot_scatter_second_order_lm(df, val,"CortexVol_ml",               
                                       alpha, psize, xLab, "Cortical GM", CI = TRUE)
    g3 <- plot_scatter_second_order_lm(df, val,"Total.Accumbens.area_ml",   
                                       alpha, psize, xLab, "Accumbens", CI = TRUE) 
  }
  
  if(val == "BMI.2.0"){
    g <- ggarrange(g1,g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "top")
    ggsave(file=ofile,plot = g, dpi = 300, units = "in", height = 5, width =10)
  }else{
    g <- ggarrange(g1,g2,g3, nrow = 1, ncol = 3, common.legend = TRUE, legend = "top")
    ggsave(file=ofile,plot = g, dpi = 300, units = "in", height = 5, width =15)
  }
}


plots_brain_MRI_body_composition_scatter_lm <- function(ofile, df, val,bodyVars,subsample = FALSE){
  
  xLab <- gsub("\\+"," ",gsub("_"," ",gsub(".2.0","",val)))
  if(subsample){
    df <- df[complete.cases(df[,bodyVars]),]
  }
  if(xLab == "VAT ASAT")
  {
    xLab <- "VAT+ASAT"
  }
  ofile <- paste0(ofile,"_n",as.character(nrow(df)),".pdf")
  
  alpha <- 0.05
  psize <- 2
  CI <- TRUE
  g1 <- plot_scatter_second_order_lm(df, val,"MeanCorticalThickness",           alpha, psize, xLab, "Cortical thickness", CI = CI) 
  g2 <- plot_scatter_second_order_lm(df, val,"MeanWhiteSurfArea_scaled",        alpha, psize, xLab, "Surface area (white)", CI = CI) 
  g3 <- plot_scatter_second_order_lm(df, val,"CortexVol_ml",                    alpha, psize, xLab, "Cortical GM", CI = CI) 
  g4 <- plot_scatter_second_order_lm(df, val,"Total.Cerebellum.Cortex_ml",      alpha, psize, xLab, "Cerebellum GM", CI = CI) 
  g5 <- plot_scatter_second_order_lm(df, val,"Brain.Stem_ml",                   alpha, psize, xLab, "Brain stem", CI = CI) 
  g6 <- plot_scatter_second_order_lm(df, val,"CorticalWhiteMatterVol_ml",       alpha, psize, xLab, "Cortical WM", CI = CI) 
  g7 <- plot_scatter_second_order_lm(df, val,"Total.Cerebellum.White.Matter_ml",alpha, psize, xLab, "Cerebellum WM", CI = CI) 
  g8 <- plot_scatter_second_order_lm(df, val,"CSF_ml",                          alpha, psize, xLab, "CSF", CI = CI) 
  g9 <- plot_scatter_second_order_lm(df, val,"Total.Lateral.Ventricle_ml",      alpha, psize, xLab, "Lateral ventricle", CI = CI) 
  g10 <- plot_scatter_second_order_lm(df, val,"X3rd.Ventricle_ml",              alpha, psize, xLab, "3rd ventricle", CI = CI) 
  g11 <- plot_scatter_second_order_lm(df, val,"Total.Thalamus.Proper_ml",       alpha, psize, xLab, "Thalamus", CI = CI) 
  g12 <- plot_scatter_second_order_lm(df, val,"Total.Hippocampus_ml",           alpha, psize, xLab, "Hippocampus", CI = CI) 
  g13 <- plot_scatter_second_order_lm(df, val,"Total.Amygdala_ml",              alpha, psize, xLab, "Amygdala", CI = CI) 
  g14 <- plot_scatter_second_order_lm(df, val,"Total.Accumbens.area_ml",        alpha, psize, xLab, "Accumbens", CI = CI) 
  g15 <- plot_scatter_second_order_lm(df, val,"Total.Caudate_ml",               alpha, psize, xLab, "Caudate", CI = CI) 
  g16 <- plot_scatter_second_order_lm(df, val,"Total.Putamen_ml",               alpha, psize, xLab, "Putamen", CI = CI) 
  g17 <- plot_scatter_second_order_lm(df, val,"Total.Pallidum_ml",              alpha, psize, xLab, "Pallidum", CI = CI) 
  
  g <- ggarrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,
                 g10,g11,g12,g13,g14,g15,g16,g17,
                 nrow = 5, ncol = 4, common.legend = TRUE, legend = "top")
  
  ggsave(file=ofile,plot = g, 
         dpi = 300, units = "in", height = 20,width =15)
  
} 




plot_residual_plots_selected_models <- function(df, Structures, reg_var ="WHR.c",
                                                lm_model = lm_model, multi_center = TRUE, wICV = TRUE, 
                                                poly_regressor = TRUE, ppath = ppath, ofile=ofile){
  print(nrow(df))
  log_transform <- FALSE
  for(ii in 1:2){
    
    log_transform <- !log_transform
    glist <- list()
    for(dep_var in Structures){
      m <- lm_brain_body_residual_plot_illustration(df = df, dep_var = dep_var, reg_var = reg_var, 
                                                    lm_model = lm_model, log_transform  = log_transform, 
                                                    multi_center =  multi_center, wICV = wICV,
                                                    poly_regressor = poly_regressor)
      gd <- autoplot(m, which = c(1,2)) + labs(title = format_structure_name(dep_var)) + theme_classic()
      glist <- append(glist, gd)
      
    }
    g <- autoplot(glist)
    ggsave(file=paste0(ppath,ofile,as.character(ii),".pdf"),plot = g, 
           dpi = 300, units = "in", height = 10, width = 10)
    
  }
  
}

plot_results <- function(df,opath, ppath,
                         antVars,bodyVars,antVars.c,bodyVars.c, 
                         lm_model)
{
  
  ################
  # Figure 2 - scatter plot of body-brain associations. 
  ################ 
  # plot_results(df.in,opath,ppath,anthrometricVars,bodyMRIVars,
  # anthrometricVars.c, bodyMRIVars.c, lm_model = lm_model3)
  
  # antVars <- anthrometricVars
  # bodyVars <- bodyMRIVars
  # 
  # val <- "WHR.2.0"
  # plots_brain_MRI_figure1(paste0(ppath,"Figure1_WHR_body_brain_",val),df,val)
  # 
  # val <- "BMI.2.0"
  # plots_brain_MRI_figure1(paste0(ppath,"Figure1_BMI_body_brain_",val),df,val)
  # 
  # val <- "waist_circumference.2.0"
  # plots_brain_MRI_figure1(paste0(ppath,"Figure1_WC_body_brain_",val),df,val)
  
  # for(val in antVars){
  #   plots_brain_MRI_body_composition_scatter_lm(paste0(ppath,"FigureXX_body_brain_",val),df, 
  #                                               val, bodyVars,subsample = FALSE)
  # }
  # 
  # for(val in bodyVars){
  #   plots_brain_MRI_body_composition_scatter_lm(paste0(ppath,"FigureXX_body_brain_",val),df, 
  #                                               val, bodyVars,subsample = TRUE)
  # }
  
  ################
  # Figure 2
  ################ 
  
  # df.t1 <- get_Anthropometric_barplot_df(opath,1)
  # df.t2 <- get_Anthropometric_barplot_df(opath,2)
  df.t3 <- get_Anthropometric_barplot_df(opath,3)
  
  # Linear term
  g1 <-plot_results_barplot(df.t3,"",length(levels(df.t3$Covariate)), firstOrder = TRUE)
  ggsave(file=paste0(ppath,"Figure2_anthropometric_linear_terms.pdf"),plot = g1, dpi = 300, units = "in", height = 15, width = 12)
  
  # Quadratic term
  g2 <-plot_results_barplot(df.t3,"",length(levels(df.t3$Covariate)), firstOrder = FALSE)
  ggsave(file=paste0(ppath,"Figure2_anthropometric_quadric_terms.pdf"),plot = g2, dpi = 300, units = "in", height = 15, width = 12)
  
  
    # g <- ggarrange(g1,g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")
  # ggsave(file=paste0(ppath,"Figure1_barplot_anthropometric_measures.pdf"),plot = g, 
  #        dpi = 300, units = "in", height = 15, width = 24)
  
  ################
  # Figure 4
  ################ 
  
  dft <- get_whole_body_barplot_df(opath,3)
  ind <- dft$Covariate %in% c("BMI","WHR","WC")
  df1 <- dft[ind,]
  df2 <- dft[!ind,]
  
  g1c <- plot_results_barplot(df1,"",length(levels(factor(df1$Covariate))),firstOrder = TRUE)
  ggsave(file=paste0(ppath,"Figure4_anthropometric_linear_terms.pdf"),plot = g1c, 
         dpi = 300, units = "in", height = 15, width = 12)
  
  g2c <- plot_results_barplot(df2,"",length(levels(factor(df2$Covariate))),firstOrder = TRUE)
  ggsave(file=paste0(ppath,"Figure4_bodyMRI_linear_terms.pdf"),plot = g2c, 
         dpi = 300, units = "in", height = 15, width = 12)
  
  
  
  ################
  # Figure 3 - poly body composition measures with brain structure
  ################ 
  
  # bVars <- antVars
  # for(ii in 1:length(bVars)){
  #   v <- bVars[ii]
  #   print(v)
  #   
  #   k <- 6+ii
  #   plots_brain_MRI_scatter_loess_body_comp(paste0(ppath,"FigureS",as.character(k),"_scatter_loess_",v),df, v, bVars) 
  # }
  # 
  # bVars <- append(antVars,bodyVars)
  # for(ii in 1:length(bVars)){
  #   v <- bVars[ii]
  #   print(v)
  #   
  #   k <- 10+ii
  #   plots_brain_MRI_scatter_loess_body_comp(paste0(ppath,"FigureS",as.character(k),"_scatter_loess_",v),df, v, bVars) 
  # }
  # 
  
  # ##################
  # # Figure Sxx - Antrhopometric + 1 single Euler number
  # ###################
  # clevels <- c("WC","WC E1","WHR","WHR E1","BMI","BMI E1")
  # df.t1a <- get_Anthropometric_barplot_df(opath,1,ending ="")
  # df.t1b <- get_Anthropometric_barplot_df(opath,1,ending ="_singleEuler")
  # df.t1 <- combine_outputs_for_barplot(df.t1a,df.t1b,clevels) 
  # 
  # df.t2a <- get_Anthropometric_barplot_df(opath,2,ending ="")
  # df.t2b <- get_Anthropometric_barplot_df(opath,2,ending ="_singleEuler")
  # df.t2 <-combine_outputs_for_barplot(df.t2a,df.t2b,clevels)
  # 
  # df.t3a <- get_Anthropometric_barplot_df(opath,3,ending ="")
  # df.t3b <- get_Anthropometric_barplot_df(opath,3,ending ="_singleEuler")
  # df.t3 <-combine_outputs_for_barplot(df.t3a,df.t3b,clevels)
  # 
  # g1 <-plot_results_barplot(df.t1,"Model (ii-a)",length(levels(df.t1$Covariate)))
  # g2 <-plot_results_barplot(df.t2,"Model (ii-b)",length(levels(df.t2$Covariate)))
  # g3 <-plot_results_barplot(df.t3,"Model (ii-c)",length(levels(df.t3$Covariate)))
  # g <- ggarrange(g1,g2,g3, nrow = 1, ncol = 3, common.legend = TRUE, legend = "right")
  # 
  # ggsave(file=paste0(ppath,"FigureSxx_barplot_anthropometric_measures_singleEuler.pdf"),plot = g, 
  #        dpi = 300, units = "in", height = 15, width = 35)
  # 
  # 
  ################
  # Figure 3
  # ################
  # dep_vars <- c("MeanCorticalThickness","MeanWhiteSurfArea","CortexVol","Total.Cerebellum.Cortex","Total.Amygdala")
  # ylabs <- c("Cortical thickness (mm)", "Surface area (white) (cm2)", "Cortical GM (ml)", "Cerebellum GM (ml)", "Amygdala (ml)")
  # 
  # # Figure 3a
  # g1 <- plot_scatter_sex(df, "BMI.2.0", "BMI", dep_vars, ylabs)
  # ggsave(file=paste0(ppath,"Figure3a_BMI.pdf"),plot = g1, 
  #        dpi = 300, units = "in", height = 5, width = 25)
  # 
  # # Figure 3b
  # dep_vars <- c("MeanCorticalThickness","CortexVol","Total.Cerebellum.Cortex","Brain.Stem")
  # ylabs <- c("Cortical thickness (mm)","Cortical GM (ml)", "Cerebellum GM (ml)", "Brain Stem (ml)")  
  # g2 <- plot_scatter_sex(df, "WHR.2.0", "WHR",dep_vars, ylabs)
  # ggsave(file=paste0(ppath,"Figure3b_WHR.pdf"),plot = g2, 
  #        dpi = 300, units = "in", height = 5, width = 20)
  # 
  # # Figure 3c
  # dep_vars <- c("MeanCorticalThickness","CortexVol","Total.Cerebellum.Cortex","Total.Amygdala")
  # ylabs <- c("Cortical thickness (mm)","Cortical GM (ml)", "Cerebellum GM (ml)", "Amygdala (ml)")  
  # g3 <- plot_scatter_sex(df, "waist_circumference.2.0","Waist circumference", 
  #                        dep_vars, ylabs)
  # ggsave(file=paste0(ppath,"Figure3c_waist_circumference.pdf"),plot = g3, 
  #        dpi = 300, units = "in", height = 5, width = 20)
  # 
  # ################
  # # Figure Supplemental figure 
  # ################
  # dep_vars <- c("BMI.2.0","WHR.2.0","waist_circumference.2.0")
  # g <- plot_catter_body_age(df[complete.cases(df[,dep_vars]),], "age.2.0", "Age", dep_vars)
  # ggsave(file=paste0(ppath,"FigureS3a_body_comp_wAge.pdf"),plot = g, 
  #        dpi = 300, units = "in", height = 5, width = 10)
  # 
  # 
  # dep_vars <- c("BMI.2.0","WHR.2.0","waist_circumference.2.0","Liver_PDFF.2.0",
  #               "VAT.2.0","ASAT.2.0","VAT_ASAT.2.0","TTMV.2.0")
  # g <- plot_catter_body_age(df[complete.cases(df[,dep_vars]),], "age.2.0", "Age", dep_vars)
  # ggsave(file=paste0(ppath,"FigureS3b_body_comp_wAge.pdf"),plot = g, 
  #        dpi = 300, units = "in", height = 15, width = 15)
  # 
  # 
}

plot_BP_results <- function(df,opath, ppath){
  df.t <- get_BP_barplot_df(opath)
  g1 <-plot_results_barplot(df.t,"",length(levels(df.t$Covariate)))
  ggsave(file=paste0(ppath,"Figure_barplot_BP_anthropometric_measures.pdf"),plot = g1, 
         dpi = 300, units = "in", height = 15, width = 13)
  
}

# Create correlation plots
create_corr_plots <- function(df, ofile, fullsample = TRUE){
  
  tmp <- NULL
  if(fullsample){
    tmp <- data.frame(Age = df$age.2.0,
                      BMI = df$BMI.2.0, 
                      WHR = df$WHR.2.0, 
                      WC = df$waist_circumference.2.0)
  }else{
    tmp <- data.frame(Age = df$age.2.0,
                      BMI = df$BMI.2.0, 
                      WHR = df$WHR.2.0, 
                      WC = df$waist_circumference.2.0,
                      VAT = df$VAT.2.0,
                      SAT = df$ASAT.2.0,
                      VS = df$VAT_ASAT.2.0,
                      LF = df$Liver_PDFF.2.0, 
                      MFI = df$MFI.2.0,
                      TMV = df$TTMV.2.0)
  }
  
  
  pdf(ofile)
  G <- qgraph(cor(tmp), layout = "spring",minimum = 0.2, cut = 0.5, maximum = 1, vsize = 8, 
              graph = "cor", edge.labels = T, theme = "colorblind")
  G
  dev.off()
}


plot_results_supplement <- function(df,opath, ppath,
                                   antVars,bodyVars,antVars.c,bodyVars.c, 
                                   lm_model)
{
  ################
  # Figure S18 residual plots - full sample - csf/lateral ventricle/3rd ventricle with/without log-transformation for regressor-of-interest WHR
  ################ 
  Structures <- c("CSF","Total.Lateral.Ventricle","X3rd.Ventricle")
  plot_residual_plots_selected_models(df, Structures, reg_var ="WHR.c", lm_model = lm_model,
                                      multi_center = TRUE, wICV = TRUE, poly_regressor = TRUE,
                                      ppath = ppath, ofile="FigureS18_toalSamlpe_with_without_log_transform_regressorWHR")
  # 
  # ################
  # # Figure S20 - body MRI subsample  - csf/lateral ventricle/3rd ventricle with/without log-transformation for regressor-of-interest WHR
  # ################ 
  ind <- complete.cases(df[,append(antVars,bodyVars)])
  plot_residual_plots_selected_models(df[ind,], Structures, reg_var ="WHR.c", lm_model = lm_model,
                                      multi_center = FALSE, wICV = TRUE, poly_regressor = TRUE,
                                      ppath = ppath, ofile="FigureS20_bodyMRIsubsamlpe_with_without_log_transform_regressorWHR")

  ###################################################################
  # Plots for supplemental note S7
  ###################################################################
 
  # Note S7 - Figure SN2 - full sample age/sex on body comp
  plots_body_comp_scatter_loess_age_sex(paste0(ppath,"FigureSN2_scatter_body_comp_age_sex_full_sample.pdf"),df, bodyMRI = FALSE)

  # Note S7 - Figure SN3
  ind <- complete.cases(df[,append(antVars,bodyVars)])
  plots_body_comp_scatter_loess_age_sex(paste0(ppath,"FigureSN3_scatter_body_comp_age_sex_bodyMRI_sample.pdf"),df[ind,], bodyMRI = TRUE)
  
  # Note S7 - Figure SN4 - full sample age/sex on brain structure
  plots_brain_MRI_scatter_loess_age_sex(paste0(ppath,"FigureSN4_scatter_brain_age_sex.pdf"),df) 
  
  ###################################################################
  # Barplots
  ###################################################################
  
  # Figure S21 - Left hemisphere
  df.t3 <- get_Anthropometric_barplot_df(opath,3, L = "LEFT_", R = "")
  
  # Linear termft_r 
  g1 <-plot_results_barplot(df.t3,"Linear term",length(levels(df.t3$Covariate)), 
                            firstOrder = TRUE, left_right = TRUE)
  
  # Quadratic term
  g2 <-plot_results_barplot(df.t3,"Quadratic term",length(levels(df.t3$Covariate)), 
                            firstOrder = FALSE, left_right = TRUE)
  
  g <- ggarrange(g1,g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")
  ggsave(file=paste0(ppath,"FigureS21_barplot_anthropometric_measures_LEFT_hemisphare.pdf"),plot = g, 
          dpi = 300, units = "in", height = 15, width = 24)
  

  # Figure S21 - Right hemisphere
  df.t3 <- get_Anthropometric_barplot_df(opath,3, L = "", R = "RIGHT_")
  
  # Linear termft_r 
  g1 <-plot_results_barplot(df.t3,"Linear term",length(levels(df.t3$Covariate)), 
                            firstOrder = TRUE, left_right = TRUE)

  # Quadratic term
  g2 <-plot_results_barplot(df.t3,"Quadratic term",length(levels(df.t3$Covariate)), 
                            firstOrder = FALSE, left_right = TRUE)

  g <- ggarrange(g1,g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")
  ggsave(file=paste0(ppath,"FigureS21_barplot_anthropometric_measures_RIGHT_hemisphare.pdf"),plot = g, 
         dpi = 300, units = "in", height = 15, width = 24)
  
  
  ################
  # Figure S22 - barplots model 2a/b/c - full sample
  ################ 
  
  df.t1 <- get_Anthropometric_barplot_df(opath,1)
  df.t2 <- get_Anthropometric_barplot_df(opath,2)
  df.t3 <- get_Anthropometric_barplot_df(opath,3)
  
  g1 <-plot_results_barplot(df.t1,"Model 2a",length(levels(df.t1$Covariate)), firstOrder = TRUE)
  g2 <-plot_results_barplot(df.t2,"Model 2b",length(levels(df.t2$Covariate)), firstOrder = TRUE)
  g3 <-plot_results_barplot(df.t3,"Model 2c",length(levels(df.t3$Covariate)), firstOrder = TRUE)
  g <- ggarrange(g1,g2,g3, nrow = 1, ncol = 3, common.legend = TRUE, legend = "right")

  ggsave(file=paste0(ppath,"FigureS22a_barplot_anthropometric_measures_linear_associations.pdf"),plot = g,
         dpi = 300, units = "in", height = 15, width = 36)

  g4 <-plot_results_barplot(df.t2,"Model 2b",length(levels(df.t2$Covariate)), firstOrder = FALSE)
  g5 <-plot_results_barplot(df.t3,"Model 2c",length(levels(df.t3$Covariate)), firstOrder = FALSE)
  g <- ggarrange(g4,g5, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")

  ggsave(file=paste0(ppath,"FigureS22b_barplot_anthropometric_measures_nonlinear_associations.pdf"),plot = g,
         dpi = 300, units = "in", height = 15, width = 24)
  
  # ################
  # Figures S24-S25 - whole body MRI subsample
  ################
  plot_results_barplot_whole_body_all(opath)
  
  
  #################
  # Figure S26-S27 - whole body MRI subsample LEFT/RIGHT hemisphere 
  ##################
  # Left hemisphere
  plot_barplot_bodyMRI_by_hemisphere(isLeft = TRUE)
  # Right hemisphere
  plot_barplot_bodyMRI_by_hemisphere(isLeft = FALSE)
  
}

plot_barplot_bodyMRI_by_hemisphere <- function(isLeft = TRUE)
{
  dft <- NULL
  if(isLeft){
    dft <- get_whole_body_barplot_df(opath,3, L = "LEFT_", R = "")   
  }else{
    dft <- get_whole_body_barplot_df(opath,3, L = "", R = "RIGHT_")   
  }
  
  ind <- dft$Covariate %in% c("BMI","WHR","WC")
  df1 <- dft[ind,]
  df2 <- dft[!ind,]
  
  df1$Covariate <- factor(df1$Covariate)
  df2$Covariate <- factor(df2$Covariate)

  ######################
  # Antropometrics
  ######################
  # Linear term
  g1 <-plot_results_barplot(df1,"Linear term",length(levels(df1$Covariate)), 
                            firstOrder = TRUE, left_right = TRUE)
  
  # Quadratic term
  g2 <-plot_results_barplot(df1,"Quadratic term",length(levels(df1$Covariate)), 
                            firstOrder = FALSE, left_right = TRUE)
  
  g <- ggarrange(g1,g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")
  
  if(isLeft){
    ggsave(file=paste0(ppath,"FigureS26a_barplot_bodyMRIsubset_anthropometric_measures_LEFT_hemisphare.pdf"),plot = g, 
           dpi = 300, units = "in", height = 15, width = 24)
  }else{
    ggsave(file=paste0(ppath,"FigureS26b_barplot_bodyMRIsubset_anthropometric_measures_RIGHT_hemisphare.pdf"),plot = g, 
           dpi = 300, units = "in", height = 15, width = 24)
  }

  ######################
  # Body MRI
  ######################
  # Linear term
  g1 <-plot_results_barplot(df2,"Linear term",length(levels(df2$Covariate)), 
                            firstOrder = TRUE, left_right = TRUE)
  
  # Quadratic term
  g2 <-plot_results_barplot(df2,"Quadratic term",length(levels(df2$Covariate)), 
                            firstOrder = FALSE, left_right = TRUE)
  
  g <- ggarrange(g1,g2, nrow = 1, ncol = 2, common.legend = TRUE, legend = "right")
  
  if(isLeft){
    ggsave(file=paste0(ppath,"FigureS27a_barplot_bodyMRIsubset_bodyMRI_measures_LEFT_hemisphare.pdf"),plot = g, 
           dpi = 300, units = "in", height = 15, width = 24)
  }else{
    ggsave(file=paste0(ppath,"FigureS27b_barplot_bodyMRIsubset_bodyMRI_measures_RIGHT_hemisphare.pdf"),plot = g, 
           dpi = 300, units = "in", height = 15, width = 24)
  }
  
  
  
}


