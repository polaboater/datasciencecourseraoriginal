complete <- function(directory,  id = 1:332) {
    frame <- data.frame(matrix(ncol=2,nrow=332))
    colnames(frame) <-c("id", "nobs")
    c <- colnames(frame)
    n = 1
    
    for (i in id) {
        frame[n,1] <- i
        
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
        nobs <- complete.cases(data)
        frame[n,2]<- sum(nobs)
        n<-n+1
    }
    w = is.na(frame[,2])
    w = !w
    shrink = frame[w,]
    shrink
    #     w = is.na(shrink)
    #     w = !w
    #     shrunk = shrink[w,]
    #     shrunk
    
}