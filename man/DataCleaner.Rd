\name{DataCleaner}
\alias{DataCleaner}
\title{
Function for Cleaning Data for Multiple Comparisons
}
\description{
This function takes a data file, the user's choice of column to do the comparisons by, the user's choice of column for the numeric values, checkes that these columnds are in the correct class and then will output a list that has the numeric values  separated by each level of the variable the user wants to compare and a dataframe of the values for easy graphing in another function. There must be column names for this function to work.
}
\usage{
DataCleaner(x, fact, respo, warn = TRUE)
}
\arguments{
  \item{x}{
  Is a dataframe or other csv file read into R.
}
  \item{fact}{
The name of a column in \code{x} that has the variables in it that you want to make the comparisons between. \code{fact} must be written exactly as how column names appear in \code{x} otherwise the function will not work.
}
  \item{respo}{
The name of a column in \code{x} that has the numerical response. \code{respo} must be written exactly as how column names appear in \code{x} otherwise the function will not work.
}
  \item{warn}{
Either \code{TRUE} or \code{FALSE}, it allows the user to specify whether they want they code to stop if \code{fact} does not have factor levels or \code{respo} is not of class numeric or integer. The default is \code{warn = TRUE}. If \code{warn = FALSE} then no error message will be given and \code{fact} will be coerced into having factors respresented by unique values or characters in the column and \code{respo} will be coerced into class numeric. 
}
}
\details{
The output of this function should be assigned to an object and that object used in the other functions of this package. If you want to subset by another column use \code{SubsetCleaner()} and not \code{DataCleaner()}.
}
\author{
Selina A. Ruzi
}
\seealso{
\code{\link{SubsetCleaner}}
}
\examples{
Example_data <- data(Example_data)
names(Example_data)

## -- If do not want to further subset the data, do the following
MyCleanData <- DataCleaner(Example_data, fact="Plot", 
respo="Seed_removal_percent", 
warn=FALSE)
}