rank <- function(state, outcome, num = "worst") {
    #read outcome data
    wd <- getwd()
    dir <- paste(wd,"/","outcome-of-care-measures.csv",sep="")
    data <- read.csv(dir,colClasses="character")
    
    #check that state an d outcome are valid
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
    
    w <- !is.na(shrink[,col])
    shrunk <- shrink[w,]
    rows <- nrow(shrunk)
    
    frame <- data.frame("Hospital.Name" = character(rows), "Rate" = numeric(rows), "Rank" = numeric(rows))
    frame[,1] <- shrunk[,2] 
    frame[,2] <- shrunk[,col]
    q <- order(frame[,2], frame[,1], decreasing = FALSE)
    z <- vector()
    
    for (i in 1:rows) {
        x <- q == i
        y <- which.max(x)
        z <- c(z,y)
    }
    
    frame[,3] <- z
    
    if (num == "best") {
        w <- which.min(frame[,3])
        hospital <- frame[w,1]
    } else if (num == "worst") {
        w <- which.max(frame[,3])
        hospital <- frame[w,1]
    } else if (num > rows) {
        hospital <- NA
    } else {
        x <- frame[,3] == num
        w <- which.max(x)
        hospital <- frame[x,1]
    }
    hospital
}
