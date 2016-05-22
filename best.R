best <- function( state, outcome){
      
      Full_Data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")           #copy all data from original file
      
      
      #Checking Outcome input for invalid arguments and selecting the desried column
      
      if (outcome == "heart failure" | outcome == "heart attack" | outcome == "pneumonia") {
            if (outcome == "heart failure") {
                  full_outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
            }
            if(outcome == "heart attack"){
                  
                  full_outcome <-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                  
            }
            if(outcome == "pneumonia"){
                  full_outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                  
            }   
      }
      else{
            stop("invalid outcome")
      }
      
      if(state %in% Full_Data$State){
            arguments <- c("State", "Hospital.Name", full_outcome )
            selected_data1 <- Full_Data[arguments]                                        #selecting and storing a subset of data with required columns
            selected_data2 <- subset(selected_data1, selected_data1$State == state )      #selecting data with only the desired state
            suppressWarnings( selected_data2[, 3] <- as.numeric(selected_data2[, 3]))     #Changing to numeric for sorting
            suppressWarnings(selected_data2 <- selected_data2[complete.cases(selected_data2),])    #removing na value from the data
            selected_data3 <- selected_data2[order(selected_data2[, 2]), ]                #sorting the hospital name in ascending order
            selected_data3 <- selected_data3[order(selected_data3[,3]), ]                 #sorting the mortality rates in ascending order
            
            return(selected_data3$Hospital.Name[1])                                       #printing the first hospital with the lowest mortality rate i.e. first row of the data
            
      }
      else{
            
            stop("invalid state")
      }
      
      
}