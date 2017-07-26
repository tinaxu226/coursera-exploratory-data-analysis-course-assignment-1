corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  comp <- complete(directory, 1:332)
  comp <- subset(comp, nobs > threshold )
  
  filenames <- list.files(path=directory, pattern="*.csv")
  correlations <- vector()
  
  for(i in comp$id) {
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    data3 <- read.csv(filepath)
    ## Calculate and store the count of complete cases
    comple <- data3[complete.cases(data3),]
    counts <- c(counts, nrow(comple))
  
  if( counts >= threshold ) {
    correlations <- c(correlations, cor(comple$nitrate, comple$sulfate) )
  }
  ## Return the data frame
 
  }
  correlations
}

cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))