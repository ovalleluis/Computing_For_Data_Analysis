agecount <- function( age = NULL) {	

	
      if ( is.null(age) ) {
		stop("invalid age")
	}

	homicides <- readLines("homicides.txt")

	pattern <- paste(c(">.*",age,"\ +years?"), collapse="")

	#print(pattern) 
	matches <- regexpr(pattern, homicides, perl=TRUE,  ignore.case=TRUE)
	
	return(length(regmatches(homicides, matches)))

}