pollutantmean <- function(directory = "specdata", pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    toRet <- numeric()
    for(i in id) {
        if(i<10){
            prefix <- "00"
        } else if(i<100) {
                prefix <- "0"
        } else {
            prefix <- ""
        }
        fname <- paste(directory,"/",prefix,as.character(i),".csv", sep="")
        # print(fname)
        this.data <- read.csv(fname)
        polvec <- this.data[[pollutant]]
        toRet <- append(toRet,polvec[!is.na(polvec)]) 
    }
    mean(toRet)
}