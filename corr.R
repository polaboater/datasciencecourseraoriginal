corr <- function(directory, threshold = 0){
    wd <- getwd()
    fn_call <- paste(wd,"/complete.R",sep="")
    source(fn_call)
    cases <- complete(directory = directory)
    match <- cases[,2] >= threshold
    matchedcases <- cases[match,]
    matchedcases
    ids <- matchedcases[,1]
    mrows <- sum(cases[,2])
    
    sulfate <- vector()
    nitrate <- vector()
    corels <- vector()
    
    for (i in ids) {
        if (i < 10) {
            str_id <- paste("00",i,".csv",sep="")
            str_id
        } else if (i < 100) {
            str_id <- paste("0",i,".csv",sep="")
            str_id
        } else {
            str_id <- paste(i,".csv",sep="")
            str_id
        }
        
        dir <- paste(getwd(),"/",directory,sep="")
        filename <- paste(dir,"/",str_id,sep="")
        
        
        data <- read.csv(file = filename, header = T, sep = ",")
        
        w <- complete.cases(data)
        shrink <- data[w,]
        corel <- cor(x = shrink[,2], y = shrink[,3])
        corels <- c(corels,corel)
        sulfate <- c(sulfate, shrink[,2])
        nitrate <- c(nitrate, shrink[,3])
    }
    corels
}