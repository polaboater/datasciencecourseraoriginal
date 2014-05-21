pollutantmean <- function(directory, pollutant, id = 1:332) {
    if (pollutant != "sulfate" & pollutant != "nitrate") { 
        "learn to spell!"
    } else {
        
        if (pollutant == "nitrate") {
            n = 3
        } else {
            n = 2
        }
        
        total <- vector()
        number <- vector()

                
        for (i in id) {
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
            
             w <- is.na(data[,n])
             w <- !w
             number <- c(number, sum(w))
             shrink <- data[w,]
             total <- c(total, sum(shrink[,n]))
        }
    
         tot = sum(total)
         num = sum(number)
         pollutantmean = tot/num
         pollutantmean
    }
}