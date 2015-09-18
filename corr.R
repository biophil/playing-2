corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    compObs <- complete(directory)
    allIds <- 1:332
    idToCheck <- allIds[compObs$nobs>threshold] # these are the ids with enough observations
    
    cors = numeric() # init vector of correlations
    for(i in idToCheck) {
        if(i<10){           # build filename
            prefix <- "00"
        } else if(i<100) {
            prefix <- "0"
        } else {
            prefix <- ""
        }
        fname <- paste(directory,"/",prefix,as.character(i),".csv", sep="")
        #print(fname)
        this.data <- read.csv(fname) # get data
        sulfvec <- this.data[['sulfate']] # raw sulf and nit from file
        nitvec <- this.data[['nitrate']]
        sulf <- sulfvec[!is.na(sulfvec) & !is.na(nitvec)] # save if complete
        nit <- nitvec[!is.na(sulfvec) & !is.na(nitvec)]
        cors <- append(cors,cor(sulf,nit)) # compute correlations
    }
    cors
}