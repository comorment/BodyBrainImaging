# Formula based on Benjamini and Hochberg 1995
fdr_threshold <- function(p_values, alpha = 0.05){
  
  # Convert to a vector
  p <- as.vector(p_values) 
  
  # Remove NA entries if present
  p <- p[!is.na(p)] 
  
  # Compute the FDR thresholding
  i <- 1:length(p)
  FDR <- (i/length(p))*alpha
  p_sorted <- sort(p)
  ind <- p_sorted <= FDR
  
  # Return the largest occurence of p_sorted <= FDR 
  threshold <- max(p_sorted[ind])  
}
