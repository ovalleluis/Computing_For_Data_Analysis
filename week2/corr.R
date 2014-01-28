corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations
	
	  corrs_ = c()

	  for (x in 1:332) {		  	

	  id_ = sprintf("%03d", as.numeric(x))

	  file_ <- paste ( paste(directory, id_, sep = "/" ), "csv" , sep = ".")	 
	
	  data = read.csv(file = file_)

	  subset_  = subset(data, !is.na(sulfate) & !is.na(nitrate) & !is.na(Date) & !is.na(ID)) 	

	  nobs_ = nrow(subset_)         
        	 
	  if ( nobs_ >= threshold )	{
		
		corrs_ = append(corrs_, cor( subset_$sulfate, subset_$nitrate))
        }

	  }

	  return(corrs_)  


}