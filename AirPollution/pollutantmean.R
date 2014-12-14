pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## This function returns the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    ############################################################################
    
    
    
    
    # This is not the most efficient or elegant implementation. This is rather 
    # the minimal-R-knowledge/use implementation.
    # For better, R-like ways to implement this function see
    # https://github.com/derekfranks/practice_assignment
    
    
    
    # save the current directory for future use
    current_dir<-getwd()
    
    # assemble the path and change the working directory
    fullpath <- paste(current_dir,"/",directory, sep="")
    setwd(fullpath)
    
    
    # initialize the vectors (we are going to compute the mean!)
    sump <- double(length = length(id))   # sum of the pollutant values
    lenp <- double(length = length(id))   # length of the available pollutant data
    
    
    # loop over the data files
    counter <- 0
    for(monitor in id){
        counter <- counter + 1
        
        # convert the monitor id into a string with leading zeros: 1 => "001"
        mon_id <- formatC(monitor, width=3, flag="0")
        
        # assemble the filename
        filename <- paste(mon_id,".csv", sep="")
        
        # read the data
        newdata <- read.csv(filename)
        
        # get the column number of the corresponding pollutant
        pol_col <- which(colnames(newdata)==pollutant)
        
        # sum the observations of each monitor and get the number of observations
        sump[counter] <- sum(newdata[,pol_col], na.rm = TRUE)
        lenp[counter] <- sum(!is.na(newdata[,pol_col]))
        
    }   # end of the monitor loop
    
    # back to the original dir
    setwd(current_dir)
    
    
    # check the divide by zero and compute the mean
    if(sum(lenp)) mean <-sum(sump)/sum(lenp)
    
    return(mean)
    
}   # end of the function definition
