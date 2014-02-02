count <- function( cause = NULL) {	

	valid_causes <- c("asphyxiation", "blunt force", "other", "shooting","stabbing", "unknown")

      if ( is.null(cause) | !(cause %in% valid_causes) ) {
		stop("invalid cause")
	}

	homicides <- readLines("homicides.txt")

	pattern <- paste(c("Cause:\ ",cause,"<+?"), collapse="")

	matches <- regexpr(pattern, homicides, perl=TRUE,  ignore.case=TRUE)
	
	return(length(regmatches(homicides, matches)))

}