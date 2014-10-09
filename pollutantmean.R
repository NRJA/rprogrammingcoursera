pollutantmean <- function(directory,pollutant, id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  char_id <- character(0)
  if (length(id[id<10]) > 0) char_id <- c(char_id, paste("00", id[id < 10],sep=""))
  if (length(id[id >= 10 & id < 100] > 0)) char_id <- c(char_id, paste("0", id[id >= 10 & id < 100],sep=""))
  char_id <- c(char_id, id[id >= 100])
  files <- paste(directory,"/",char_id,".csv",sep="")
  all_data <- lapply(files, read.csv)
  all_data <- do.call(rbind, all_data)
  
  return (mean(all_data[pollutant][,],na.rm=1))
}