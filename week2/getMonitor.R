getmonitor <- function(id, directory, summarize = FALSE) {
        ## 'id' is a vector of length 1 indicating the monitor ID
        ## number. The user can specify 'id' as either an integer, a
        ## character, or a numeric.
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

	  id_ = sprintf("%03d", as.numeric(id))

	  file_ <- paste ( paste(directory, id_, sep = "/" ), "csv" , sep = ".")
		
	  data = read.csv(file = file_)

        ## 'summarize' is a logical indicating whether a summary of
        ## the data should be printed to the console; the default is
        ## FALSE

	  if (summarize) print (summary(data))
        
	  return(data) 	
        
}
