complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

	  df <- NULL	

	  for (x in id) {		  	

	  id_ = sprintf("%03d", as.numeric(x))

	  file_ <- paste ( paste(directory, id_, sep = "/" ), "csv" , sep = ".")	 
	
	  data = read.csv(file = file_)
	  
	  nobs_ = nrow(subset(data, !is.na(sulfate) & !is.na(nitrate)))
       
	  df = rbind(df, data.frame(id =  as.numeric(id_), nobs = nobs_)) 

	  }

	  print(df)
}