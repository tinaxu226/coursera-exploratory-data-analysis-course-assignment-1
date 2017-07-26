pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## Get a list of filenames
  filenames <- list.files(path=directory, pattern="*.csv")
  vals <- vector()
  for(i in id) {
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    ## Load the data
    data <- read.csv(filepath)
    d <- na.omit(data[,pollutant])
    d <- d[!is.na(d)]
    vals <- c(vals, d)
  }
  mean(vals)
}
pollutantmean("specdata","sulfate",1:10)
pollutantmean("specdata","sulfate",70:72)
pollutantmean("specdata","sulfate",34)
pollutantmean("specdata","nitrate")

