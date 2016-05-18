best <- function( state, outcome){
      
      Full_Data <- read.csv("outcome-of-care-measures.csv")
      # selected_data <- subset(Full_Data, State == state)
      
      full_outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome, sep = "")
      arguments <- c("State", full_outcome, "Hospital.Name")
      selected_data1 <- Full_Data[arguments]
      
      #Selecting data of the given State
      selected_data2 <- subset(selected_data1, selected_data1$State == state && 
                                     selected_data1[full_outcome] == min(selected_data1[full_outcome]), na.rm =TRUE)
      #return the hospital name
      return(c(selected_data2$Hospital.Name, selected_data2$State))
      
      #Best_Hospital <- selected_data[which.max(full_outcome)]
      #Best_Hospital_name <- Best_Hospital$Hospital.Name
      #selected_variable <- c( "State", "Hospital.Name", "" )
      #concise_data <- Full_Data[selected_variable]
      #consise:data <- Full_Data[, c()]
}