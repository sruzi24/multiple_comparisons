DataCleaner <- function(x, fact, respo, warn=TRUE){
  #x is the datafile, fact = name of the column that the user wants as the factor
  #respo = name of the column that the user wants as the response variable
  #warn = defaults to TRUE, will warnt he user that fact does not contain factors
  #and that response is not numeric or integer class. If set to FALSE then
  #the code will coerce the columns to being the correct class
  if(any(colnames(x)==fact)==TRUE && any(colnames(x)==respo)==TRUE){
    if(warn==TRUE){ # stop the code if the following warnings are true
      if(is.factor(x$fact)==FALSE) stop("Fact column does not have factors")
      if(is.numeric(x$respo)==FALSE || is.integer(x$respo)==FALSE) stop("Respo column is not numeric or an integer")
    }
    if(warn==FALSE){
      
      
      return(print("working")) ## will get rid of this everntually but need it for now
      
    }
    if(warn!=TRUE && warn!=FALSE) stop("Warn must be set to TRUE or FALSE")
      
    
    
    
  }else {
    stop("Fact and/or respo column name(s) not found")} 
}

### test data
setwd("/Users/Selina/Dropbox/Seed_Removal_BCI/Data/Data May 2015")
raw_data<-read.csv("2013_aboveground_totals_may2015.csv")
names(raw_data)
DataCleaner(raw_data, fact="Plot",respo="Total.Seeds.Removed")
DataCleaner(raw_data, fact="y",respo="Total.Seeds.Removed") 
DataCleaner(raw_data, fact="Plot",respo="y") 
