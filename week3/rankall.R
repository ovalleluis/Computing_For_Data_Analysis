rankall <- function(outcome, num = "best") {
	
	 ## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

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
      
      valid_data  <- data[,c(7,2,ncol)]	

	##print(valid_data)

      valid_data[,3] <- as.numeric(valid_data[,3])
	
	valid_data <- subset( valid_data, !is.na(valid_data[,3]))

	valid_data <- split(valid_data, valid_data$State)

	##print(valid_data)
	
	df <- NULL

	for (i in valid_data) {			 
		i <- i[ order(i[,3],i[,2]), ]

		## Processing pos for subrow
		if ( num == "best") {
			pos <- 1
		}
		else if ( num == "worst") {
			pos <- nrow(i)
		}
		else  {
			pos = as.numeric(num)
		}

		df = rbind(df, data.frame(hospital =  i[pos,2], state = i[1,1]))
	}

	return(df)
     

}