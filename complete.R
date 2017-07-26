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

  filenames <- list.files(path=directory, pattern="*.csv")
  ids <-vector()
  counts = vector()

  for(i in id) {
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    data1 <- read.csv(filepath)
    ## Store the id
    ids <- c(ids, i)
    
    ## Calculate and store the count of complete cases
    compl <- data1[complete.cases(data1),]
    counts <- c(counts, nrow(compl))
  }
  
  ## Return the data frame
  data.frame(id=ids, nobs=counts)
}
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
