rankhospital <- function(state, outcome, num = "best"){
    wd <- getwd()
    source(paste(wd,"/","best.R",sep =""))
    source(paste(wd,"/","rank.R",sep =""))
    
    if (num == "best") {
        rank <- best(state, outcome)
    } else if (num == "worst") {
        rank <- rank(state, outcome)
    } else {
        rank <- rank(state, outcome, num)
    }
    rank
}



