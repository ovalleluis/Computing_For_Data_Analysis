rankhospital <- function(state, outcome, num = "best") {
	
	 ## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


	if( !any(state==data$State)){
		stop("invalid state")
	}

      if( outcome == "heart attack") { 
		ncol <- 11
      }	
	else if ( outcome == "heart failure") {
            ncol <- 17
      }
	else if ( outcome == "pneumonia") {
            ncol <- 23
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

  	if ( num == "best") {
		pos <- 1
	}
	else if ( num == "worst") {
		pos <- nrow(valid_data)
	}
	else  {
		pos = as.numeric(num)
	}

	#print ( valid_data)

	return( valid_data[pos,1])

}