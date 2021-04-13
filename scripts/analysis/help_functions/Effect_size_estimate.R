# Function based on Nakagawa, S. and Cuthill, I. C., 2007.
cohens_d <- function(t_coeff,df,n1,n2){ 
  D <- (t_coeff*(n1+n2))/(sqrt(n1*n2)*sqrt(df))
}

# Function based on Nakagawa, S. and Cuthill, I. C., 2007.
partial_corr <- function(t_coeff, df){
  r <- t_coeff/sqrt(t_coeff*t_coeff + df)
}

# Function from Moberget et al. 2017.
var_d <- function(D,n1,n2){
  VarD <- ( (n1+n2)/(n1*n2)+(D*D)/(2*(n1+n2-2)))*((n1+n2)/(n1+n2-2))
}

# Function from Nakagawa, S. and Cuthill, I. C., 2007.
standard_error_d <- function(D,n1,n2){
  se_D <- sqrt( ( (n1+n2-1) / (n1+n2-3) ) * ( (4/(n1+n2) ) * ( 1 + ( D*D )/8 ) ) )
}

# Function from Nakagawa, S. and Cuthill, I. C., 2007.
standard_error_r <- function(r,n1,n2){
  # se_r <- sqrt( ((n1+n2-1)/(n1+n2-3)) * ((4/(n1+n2)) *(1+ (D*D)/8)) )
}

# Function from Nakagawa, S. and Cuthill, I. C., 2007.
convert_d_to_r <- function(D,n1,n2){
  r <- D / sqrt( D*D +( ((n1+n2)*(n1+n2))/(n1*n2) ) )
}

# Function from Nakagawa, S. and Cuthill, I. C., 2007.
convert_r_to_D <- function(r){
  D <- 2*r / sqrt(1-r*r) 
}

# Function from Nakagawa, S. and Cuthill, I. C., 2007.
convert_r_to_Z_fisher_transform <- function(r){
  Zr <- 0.5*log( (1+r)/(1-r) )  
}

# Function from Nakagawa, S. and Cuthill, I. C., 2007.
# Jf Fundamentals of biostatistics this function is also 
# valid for backtransforming lower/uppen confidence intervals computed from r, via z
convert_z_to_r_fisher_transform <- function(Zr){
  r <- (exp(2*Zr) - 1) / (exp(2*Zr) + 1) 
}

compute_CI_from_r <- function(r,n){
  Zr <- convert_r_to_Z_fisher_transform(r)
  se <- 1/sqrt(n-3)
  
  lCI <- Zr - 1.96*se
  uCI <- Zr + 1.96*se
  
  # Backtransform to r, and return. 
  CI <- c(convert_z_to_r_fisher_transform(lCI),
          convert_z_to_r_fisher_transform(uCI))
}
