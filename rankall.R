rankall <- function(outcome, num = "best"){
      
      Full_Data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")           #copy all data from original file
      
      
      #Checking if Outcome is valid or not and selecting the desried column
      
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
      
      # selecting the data with required columns from the whole data
      arguments <- c("State", "Hospital.Name", full_outcome )
      selected_data1 <- Full_Data[arguments]                                    
      
      #Making data frame "ranked_data" with two rows namely "state" and "hospital"
      ranked_data <- data.frame( NA, sort(unique(Full_Data$State)))
      colnames(ranked_data) <- c("hospital", "state")
      mystate <- NULL
      
      # a for loop to find and store the num of each state and store the required hospital name in ranked_data
      for (mystate in ranked_data$state) {
            selected_data2 <- subset(selected_data1, selected_data1$State == mystate)                 #selecting data containing only the desired state
            suppressWarnings(selected_data2[, 3] <- as.numeric(selected_data2[, 3]))                  #Changing to numeric for sorting
            suppressWarnings(selected_data2 <- selected_data2[complete.cases(selected_data2),])       #removing na value from the data
            selected_data3 <- selected_data2[order(selected_data2[, 2]), ]                            #sorting the hospital name in ascending order
            selected_data3 <- selected_data3[order(selected_data3[, 3]), ]                            #sorting the mortality rates in ascending order
            
            if(num == "best" | num == "worst"){
                  if(num == "best"){
                        ranked_data[ranked_data$state == mystate, 1] <- selected_data3$Hospital.Name[1]   #returns hospital name of first row i.e. best
                  }
                  
                  else{
                        ranked_data[ranked_data$state == mystate, 1] <- selected_data3$Hospital.Name[length(selected_data3$Hospital.Name)] #returns hospital name of last row i.e. worst
                  }
            }
            
            else{
                  if(num < length(selected_data3$State)){                                 #checking if the given num is greater than the length of the selected_data3
                        ranked_data[ranked_data$state == mystate, 1] <- selected_data3$Hospital.Name[num]
                  }
                  else{ 
                        ranked_data[ranked_data$state == mystate, 1] <- NA                 #returns NA if number is out of range or wrong input
                  }
            }
      }
      return(ranked_data)
}      