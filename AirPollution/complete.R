complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## This function returns a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    ############################################################################
    
    
    # instead of changing the working directory and manually constructing the 
    # the filenames and filepath (like we did in pollutant.R) we use 
    # the list.files() function to get the 
    # list of all the files with corresponding paths
    fullpath <- list.files(directory, full.names = TRUE)
    
    
    # initialize the output dataframe
    complete <- data.frame(id = numeric(length(id)), nobs = numeric(length(id)))
    
    # loop over the data files
    counter <- 0
    for(monitor in id){
        counter <- counter + 1
        
        # read the data
        newdata <- read.csv(fullpath[monitor])

        # populate the dataframe one value at a time
        complete$id[counter] <- monitor
        complete$nobs[counter] <- sum(complete.cases(newdata))
        
    }   # end of the monitor loop
    
    return(complete)   
}   # end of the function definition