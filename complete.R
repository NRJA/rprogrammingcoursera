complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  mat <- matrix(,nrow=0,ncol=2)
  colnames(mat) <- c("id", "nobs")
  for (i in id){
    if (i < 10) {
      char_id <- paste("00",i,sep="")
    } else if (i < 100) {
      char_id <- paste("0",i,sep="")
    } else char_id <- as.character(i)
    temp <- read.csv(paste(directory,"/",char_id,".csv",sep=""))
    mat <- rbind(mat,c(i, length(temp$ID[!is.na(temp$sulfate)&!is.na(temp$nitrate)&!is.na(temp$Date)&!is.na(temp$ID)])))
  }
  
  return(data.frame(mat))
}