DataCleaner <- function(x, fact, respo, warn=TRUE){
  #x is the datafile, fact = name of the column that the user wants as the factor
  #respo = name of the column that the user wants as the response variable
  #warn = defaults to TRUE, will warnt he user that fact does not contain factors
  #and that response is not numeric or integer class. If set to FALSE then
  #the code will coerce the columns to being the correct class
  if(is.data.frame(x)==FALSE) x <- as.data.frame(x)
  if(any(colnames(x)==fact)==TRUE && any(colnames(x)==respo)==TRUE){
    factor_col <- which(colnames(x)==fact)
    respo_col <- which(colnames(x)==respo)
    if(warn==TRUE){ # stop the code if the following warnings are true
      if(is.factor(x[,factor_col])==FALSE) stop("Fact column does not have factors")
      if(is.numeric(x[,respo_col])==FALSE || is.integer(x[,respo_col])==FALSE) stop("Respo column is not numeric or an integer")
    }
    x$fact <- as.factor(x[,factor_col])
    x$respo <- as.numeric(x[,respo_col])
    factor_names <- unique(levels(x$fact))
    numfactors <- factor_names
    
    output1 <- list() # setting up a list so that can output a list of the
    #different numeric variables by factor for easier use in making multiple t-test
    #comparisons
    
    for(i in 1:length(factor_names)){
      temp <- subset(x, fact==factor_names[i])
      temp_name <- factor_names[i]
      output1[[i]] <- as.numeric(temp[,respo_col])
    }
    
    names(output1) <- factor_names
   
    output2 <- as.data.frame(cbind(variable=x$fact,response=x$respo))
    output2$variable <- as.factor(x$fact)
    
    products <- append(list(compare_list=output1),list(data_frame=output2))
    
    return(products)
    #return(output1)
    
    if(warn!=TRUE && warn!=FALSE) stop("Warn must be set to TRUE or FALSE")
      
    
  }else {
    stop("Fact and/or respo column name(s) not found")} 
}
