SubsetCleaner <- function(x, fact, respo, warn=TRUE){
  #If the user wants to further subset the data before doing t-test 
  #comparisons then they will use this function that will then
  #call the DataCleaner function separately so you will not need to also call
  #that function on your own. 
  #x is the datafile, fact = name of the column that the user wants as the factor
  #respo = name of the column that the user wants as the response variable
  #warn = defaults to TRUE, will warnt he user that fact does not contain factors
  #and that response is not numeric or integer class. If set to FALSE then
  #the code will coerce the columns to being the correct class
  print(trial)
}
