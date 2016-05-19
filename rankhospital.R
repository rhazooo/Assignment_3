rankhospital <- function( state, outcome, num = "best"){
      
      Full_Data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")           #copy all data from original file
      
      
      #Checking Outcome input for invalid arguments and selecting the desried column
      
      if (outcome == "heart failure" | outcome == "heart attack" | outcome == "pneumonia") {
            if (outcome == "heart failure") {
                  full_outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
            }
            ifelse(outcome == "heart attack", 
                   full_outcome <-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" ,
                   full_outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
            
      }
      else{
            #Inv_outcome <- paste( "Error in best(", state, ",", outcome, ") : invalid outcome", sep = "'" )
            
            stop("invalid outcome")
      }
      
      if(state %in% Full_Data$State){
            arguments <- c("State", "Hospital.Name", full_outcome )
            selected_data1 <- Full_Data[arguments]                                        #selecting and storing a subset of data with required columns
            selected_data2 <- subset(selected_data1, selected_data1$State == state )      #selecting data with only the desired state
            selected_data3 <- selected_data2[order(selected_data2[, 2], na.last = NA), ]  #sorting the hospital name in ascending order
            selected_data3 <- selected_data3[order(selected_data3[, 3], na.last = NA), ]   #sorting the mortality rates in ascending order
            
           # print(selected_data3$Hospital.Name[1:2])    #printing the first hospital with the lowest mortality rate i.e. first row of the data
            #print(selected_data3$Hospital.Name[length(selected_data3$Hospital.Name)])
            
            if(!is.numeric(num) && num == "best" || num == "worst"){
                  print("inside")
                  if(num == "best"){
                        print("all in")
                        return(selected_data3$Hospital.Name[1])
                        
                  }
                  else{
                        print("else inside")
                        return(selected_data3$Hospital.Name[length(selected_data3$Hospital.Name)])
                  }
            }
            else{
                  
                  print(head(selected_data3))
                  if(num < length(selected_data3$State)){
                        print(c("the length is", length(selected_data3$State)))
                        return(selected_data3$Hospital.Name[num])
                  }
                  else{
                        print("it is invalid Number") 
                        stop(NA)
                  }
                        
            }
            
            
      }
      else{
            
            stop(noquote("invalid state"))
      }
      
}      