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
      #which(colname(x)==fact)
      #x$fact <- as.factor(x$fact)
      #x$respo <- as.numeric(x$respo)
      #numfactors <- length(levels(x$fact))
    
      print(head(x[,c(factor_col,respo_col)])) ## will get rid of this everntually but need it for now
   
       if(warn!=TRUE && warn!=FALSE) stop("Warn must be set to TRUE or FALSE")
      
    
    
    
  }else {
    stop("Fact and/or respo column name(s) not found")} 
}

### test data
setwd("/Users/Selina/Dropbox/Seed_Removal_BCI/Data/Data May 2015")
raw_data<-read.csv("2013_aboveground_totals_may2015.csv")
names(raw_data)
DataCleaner(x=raw_data, fact="Plot",respo="Total.Seeds.Removed", warn=TRUE)
DataCleaner(x=raw_data, fact="y",respo="Total.Seeds.Removed") 
DataCleaner(x=raw_data, fact="Plot",respo="y") 
class(raw_data$Total.Seeds.Removed)
    
