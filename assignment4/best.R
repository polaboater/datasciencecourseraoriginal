best <- function(state,outcome) {
    #options(warn=-1)
    ## read outcome data
    wd <- getwd()
    dir <- paste(wd,"/","outcome-of-care-measures.csv",sep="")
    data <- read.csv(dir,colClasses="character")
    
    #coerce outcome data to numeric
    hA <- as.numeric(data[,11])
    data[,11] <- hA
    hF <- as.numeric(data[,17])
    data[,17] <- hF
    pn <- as.numeric(data[,23])
    data[,23] <- pn
    
    ## check that the state and outcome are valid
    ch_st <- data[,7] == state
    if (sum(ch_st) == 0) {
        stop("invalid state")
    } else {
        shrink <- data[ch_st,]
    }
    
    if (outcome == "heart failure") {
        col <- 17
    } else if (outcome == "heart attack") {
        col <- 11
    } else if (outcome == "pneumonia") {
        col <- 23
    } else {
        stop("invalid outcome")
    } 
    
    ## return hospital name in the particular state with the lowest 30-day
    ## death rate
    w <- which.min(shrink[,col])
    hospital <- shrink[w,2]
    hospital
    
}