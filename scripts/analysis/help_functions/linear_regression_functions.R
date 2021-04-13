# Linear regression models. 
# All continuous variables are centered. 
# All categorical variables are factors.

lm_body <- function(df, dep_var, lm_model)
{	# Adjusts for interaction between sex and BMI
  
  if(dep_var %in% c("Liver_PDFF.2.0", "BMI.2.0", "MFI.2.0","VAT.2.0", "TTMV.2.0",
                    "ASAT.2.0", "VAT_ASAT.2.0","waist_circumference.2.0","WHR.2.0"))
  {
    model <- paste0("log(df[,dep_var])~",lm_model)
  }else{
    model <- paste0("df[,dep_var]~",lm_model)
  }
  
  print(dep_var)
  # print(model)
  
  # Run model
  m <- lm(eval(parse(text=model)), data = df) 
  return(m)
}

lm_brain <- function(df, dep_var, lm_model, wICV = TRUE, multi_center = TRUE)
{
  model <- NULL
  if(dep_var %in% c("CSF","X3rd.Ventricle","Total.Lateral.Ventricle"))
  {
    model <- paste0("log(df[,dep_var])~",lm_model,"+ euler_average")
  }else{
    model <-  paste0("df[,dep_var]~",lm_model,"+ euler_average")   
  }
  if(wICV & (dep_var != "MeanCorticalThickness")){
    model <- paste0(model," + ICV")
  }
  if(multi_center){
    model <- paste0(model,"+ Assessment_centre.2.0")
  }
  
  print(model)
  
  # Run model
  m <- lm(eval(parse(text=model)), data = df)    
  return(m)
}

lm_brain_body <- function(df, dep_var, reg_var, lm_model, multi_center = TRUE, 
                          sexInteraction = FALSE, wICV = TRUE, poly_regressor = FALSE)
{
  model <- NULL
  if(dep_var %in% c("CSF","X3rd.Ventricle","Total.Lateral.Ventricle"))
  {
    model <- paste0("log(df[,dep_var])~",lm_model)
  }else{
    model <-  paste0("df[,dep_var]~",lm_model)   
  }
  # if(!sexInteraction){
  if(poly_regressor){
    model <- paste0(model," + poly(df[,reg_var],2)"," + euler_average")
  }else{
    model <- paste0(model," + df[,reg_var]"," + euler_average")
  }
  # }else{
  #   model <- paste0(model," + df[,reg_var]*sex"," + euler_average")  
  # }
  if(wICV & (dep_var != "MeanCorticalThickness")){
    model <- paste0(model," + ICV")
  }
  if(multi_center){
    model <- paste0(model,"+ Assessment_centre.2.0")
  }
  
  print(model)
  
  # Run model
  m <- lm(eval(parse(text=model)), data = df)    
  return(m)
}


lm_brain_body_residual_plot_illustration <- function(df, dep_var, reg_var, lm_model, 
                                                     log_transform = TRUE, multi_center = TRUE, 
                                                     wICV = TRUE, poly_regressor = FALSE)
{
  model <- NULL
  if(log_transform)
  {
    model <- paste0("log(df[,dep_var])~",lm_model)
  }else{
    model <-  paste0("df[,dep_var]~",lm_model)   
  }
  if(poly_regressor){
    model <- paste0(model," + poly(df[,reg_var],2)"," + euler_average")
  }else{
    model <- paste0(model," + df[,reg_var]"," + euler_average")
  }
  if(wICV & (dep_var != "MeanCorticalThickness")){
    model <- paste0(model," + ICV")
  }
  if(multi_center){
    model <- paste0(model,"+ Assessment_centre.2.0")
  }
  
  print(model)
  
  # Run model
  m <- lm(eval(parse(text=model)), data = df)    
  return(m)
}
