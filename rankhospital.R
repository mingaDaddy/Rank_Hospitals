rankhospital <- function(state, outcome, num="best") {
        ## Read outcome data
        careMeasures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state, outcome, and num are valid
        states <- unique(careMeasures$State)
        validState <- state %in% states
        
        if (validState == "FALSE") {
                return(sprintf("%s is not a valid state. Exiting function", state))
        }
        
        
        outcomes <- c("heartattack", "heartfailure", "pneumonia")
        cleanOutcome <- tolower(gsub(" ", "", outcome, fixed = TRUE))
        
        validOutcome <- cleanOutcome %in% outcomes
        
        if (validOutcome == "FALSE") {
                return(sprintf("%s is not a valid outcome (heart attack, heart failure, pneumonia). Exiting function", outcome))
        }
        
        if (cleanOutcome == "heartattack") {
                outcomeCol = 11
        } else if (cleanOutcome == "heartfailure") {
                outcomeCol = 17
        } else {
                outcomeCol = 23
        } 
        
        if (num == as.character("best") || num == as.character("worst") || is.numeric(num)) {
        
        } else {
                return(sprintf("%s is not a valid number (best, worst, integer). Exiting function", num))
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## Make a subset of the state, keeping only hospital name and 30.day.mortality by outcome
        stateSubset <<- subset(careMeasures[c(2,7,outcomeCol)], State == state)
        ## Delete not available observations
        stateSubset <<- subset(stateSubset, stateSubset[,3]!="Not Available")
        ## Convert mortality rate into numeric
        stateSubset[,3] <<- as.numeric(stateSubset[,3])
        ## Order by mortality and then alphabetic     
        stateSubset <<- stateSubset[order(stateSubset[,3], stateSubset[,1]),]
        
        stateSubset <<- transform(stateSubset, 
                  hospital.rank = ave(stateSubset[,3], stateSubset[,2], 
                                  FUN = function(x) rank(x, ties.method = "first")))
        
        maxHospitals <- nrow(stateSubset)
        
        ## Assign best and worst as numeric
        if (num == "best") {
                num=1
        } else if (num == "worst") {
                num=maxHospitals
        }
        
        
        if (maxHospitals<num) {
                return (sprintf("Num %s out of bonds. Try with a positiv integer lesser than %s, or with best or worst", num, maxHospitals+1))
        }
        
        

       
        ## Get hospital name by given rank
        hospital <<- stateSubset[ stateSubset[,4] == num , ][,1]
        
        ## Get rate from hospital itself
        rate <<- stateSubset[ stateSubset[,4] == num , ][,3]
        
        ## Print output
        print(sprintf("%s: %s", hospital, rate))
        
}
