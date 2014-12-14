corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## This function returns a numeric vector of correlations

    
    ############################################################################
    
    # get the list of all the files in the directory with full paths
    fullpath <- list.files(directory, full.names = TRUE)
    
    # use the complete function to get table of complete cases
    completes <- complete(directory, 1:length(fullpath))
    
    # check which cases are above the threshold
    above_threshold <- which(completes[,2] > threshold)
    
    # initialze the output vector
    corr <- double(length(above_threshold))
    
    
    # loop over relevant monitors
    counter <- 0
    for(monitor in above_threshold) {
        
        counter <- counter + 1
        
        newdata <- read.csv(fullpath[monitor])
        
        rows <- complete.cases(newdata)
        
        corr[counter] <- cor(newdata[rows,2],newdata[rows,3])
        # it is actually possible to avoid using counter at all by growing the 
        # corr vector, however I prefer preallocating and populating to growing
   
    }   # end of the monitor loop
    
    return(corr)
    
}   # end of the function definition