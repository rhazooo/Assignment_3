rankhospital <- function( state, outcome, num = "best"){
      
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
            stop("invalid outcome")                                                                      # executes when outcome is invalid
      }
      
      if(state %in% Full_Data$State){
            arguments <- c("State", "Hospital.Name", full_outcome )
            selected_data1 <- Full_Data[arguments]                                       #selecting and storing a subset of data with required columns
            selected_data2 <- subset(selected_data1, selected_data1$State == state)      #selecting data containing only the desired state
            suppressWarnings( selected_data2[, 3] <- as.numeric(selected_data2[, 3]))    #Changing to numeric for sorting
            suppressWarnings(selected_data2 <- selected_data2[complete.cases(selected_data2),])    #removing na value from the data
            selected_data3 <- selected_data2[order(selected_data2[, 2]), ]                #sorting the hospital name in ascending order
            selected_data3 <- selected_data3[order(selected_data3[, 3]), ]                #sorting the mortality rates in ascending order
            
            if(!is.numeric(num) && num == "best" || num == "worst"){                      #IF statement checking if the given num has value of "best" or "worst"
                  if(num == "best"){
                        return(selected_data3$Hospital.Name[1])                           #Returns the hospital name at first entry of selected_data3, as it is in ascending order
                  }
                  else{
                        return(selected_data3$Hospital.Name[length(selected_data3$Hospital.Name)])  #returns hospital name of last row i.e. worst
                  }
            }
            else{
                  if(num < length(selected_data3$State)){                                 #checking if the given number is out of range of the selected_data3
                        return(selected_data3$Hospital.Name[num])
                  }
                  else{ 
                        return(NA)                                                          #stops the program and returns NA if number is out of range or wrong input
                  }
            }
      }
      else{
            stop("invalid state")                                                                       #if the given state is not available, then return "invalid state"
      }
      
}      