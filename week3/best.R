best <- function(state, outcome) {

      ## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


	if( !any(state==data$State)){
		stop("invalid state")
	}

      if( outcome == "heart attack") { 
		ncol <- 13
      }	
	else if ( outcome == "heart failure") {
            ncol <- 19
      }
	else if ( outcome == "pneumonia") {
            ncol <- 25
      }
	else {
		stop("invalid outcome")
	}


      ## Check that state and outcome are valid
      
      valid_data  <- (subset(data, State == state))[,c(2,ncol)]

	#print ( valid_data)

      valid_data[,2] <- as.numeric(valid_data[,2])
	
	valid_data <- subset( valid_data, !is.na(valid_data[,2]))
      
	valid_data <- valid_data[ order(valid_data[,2],valid_data[,1]), ]

	#print ( valid_data)

	#print ( valid_data[1,1] )
	
	return( valid_data[1,1])

}