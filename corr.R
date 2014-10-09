corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  cor_vect <- numeric(0)
  for (i in list.files(directory)){
    temp <- read.csv(paste(directory, "/", i, sep=""))
    comp_rows <- subset(temp, !is.na(sulfate) & !is.na(nitrate))
    if (length(comp_rows[,1]) > threshold){
      cor_vect <- rbind(cor_vect, cor(comp_rows$sulfate, comp_rows$nitrate))
    }
  }
  return(cor_vect)
}