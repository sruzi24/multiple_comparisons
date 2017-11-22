SubsetCleaner <- function(x, sub, fact, respo, warn=TRUE){
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
    data_output1 <- list(numsub) #to set up an empty list of length numsub
    data_output2 <- list(numsub) #to set up an empty list of length numsub
    if(any(colnames(x)==fact)==TRUE && any(colnames(x)==respo)==TRUE){
      factor_col <- which(colnames(x)==fact)
      respo_col <- which(colnames(x)==respo)
    
      if(warn==TRUE){ # stop the code if the following warnings are true
        if(is.factor(x[,factor_col])==FALSE) stop("Fact column does not have factors")
        if(is.numeric(x[,respo_col])==FALSE || is.integer(x[,respo_col])==FALSE) stop("Respo column is not numeric or an integer")
      }
        
    #to cycle through the sub names to make different dataframes that can 
    #then be passed to DataCleaner to pull out the correct information
    for(i in 1:length(sub_names)){
      temp_file <- subset(x, sub==sub_names[i])
      #send this subset to the DataCleaner file to get that data set up correctly
      temp_data <- DataCleaner(x=temp_file,fact=fact,respo=respo,warn=warn)
      data_output1[[i]] <- temp_data$compare_list
      data_output2[[i]] <- temp_data$data_frame
    }
    
    names(data_output1) <- sub_names
    names(data_output2) <- sub_names
    } else stop("Fact and/or respo column name(s) not found")
    list_outputs <- append(list(sub_compare_list=data_output1),list(sub_data_frame=data_output2))
    
    return(list_outputs)
    
    if(warn!=TRUE && warn!=FALSE) stop("Warn must be set to TRUE or FALSE")
    
  }else stop("Sub column not found")
  
}
