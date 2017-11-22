SubsetCleaner <- function(x, sub, fact, respo, warn=TRUE, ...){
  #If the user wants to further subset the data before doing t-test 
  #comparisons then they will use this function that will then
  #call the DataCleaner function separately so you will not need to also call
  #that function on your own. 
  #x is the datafile, fact = name of the column that the user wants as the factor
  #respo = name of the column that the user wants as the response variable
  #warn = defaults to TRUE, will warnt he user that fact does not contain factors
  #and that response is not numeric or integer class. If set to FALSE then
  #the code will coerce the columns to being the correct class
  #sub = name of the column that want to subset the data by
  #... arguments to pass onto other functions if desired 
  
  if(is.data.frame(x)==FALSE) x <- as.data.frame(x)
  
  #to check if the column to subset by is in the dataframe
  if(any(colnames(x)==sub)==TRUE){
    sub_col <- which(colnames(x)==sub)
    x$sub <- as.factor(x[,sub_col])
    sub_names <- unique(levels(x$sub))
    numsub <- sub_names
    
    data_output1 <- list(numsub)
    #to cycle through the sub names to make different dataframes that can 
    #then be passed to DataCleaner to pull out the correct information
    for(i in 1:length(sub_names)){
      temp_file <- subset(x, sub==sub_names[i])
      temp_name <- sub_names[i]
      data_output1[[i]] <- as.numeric(temp_file[,c(respo_col,fact_col])
    }
    
    names(data_output1) <- sub_names
    
      
  }else stop("Sub column not found")
}
