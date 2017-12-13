\name{SubsetCleaner}
\alias{SubsetCleaner}
\title{
Function for Subsetting and Cleaning Data for Multiple Comparisons
}
\description{
This function takes a data file, the user's choice of column to subset the data by, the user's choice of column to do the comparisons by, the user's choice of column for the numeric values, checkes that these columnds are in the correct class and then will output a list separated by subset that has the numeric values separated by each level of the variable the user wants to compare and dataframes based on the number of subset levels of the values for easy graphing in another function. There must be column names for this function to work.
}
\usage{
SubsetCleaner(x, sub, fact, respo, warn = TRUE)
}
\arguments{
  \item{x}{
Is a dataframe or other csv file read into R.
}
  \item{sub}{
The name of a column in \code{x} that has the variables in it that you want to subset the data by.\code{sub} must be written exactly as how column names appear in \code{x} otherwise the function will not work. The function automatically coerces this into having levels. 
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
The output of this function should be assigned to an object and that object used in the other functions of this package. If you do not want to subset by a column use \code{DataCleaner()} and not \code{SubsetCleaner()}.
}
\value{
\item{sub_compare_list} {a list of the \code{sub} levels each containing a list with all \code{fact} levels with the associated \code{respo} values }
\item{sub_data_frame} {a list of the \code{sub} levels each containing a dataframe of the \code{fact} and \code{respo} values}
}
\author{
Selina A. Ruzi
}
\seealso{
\code{\link{DataCleaner}}
}
\examples{
## -- find path
mypath <- system.file("extdata", "Example_data.csv", package="multipleComparisons")

## -- read in data
Example_data <- read.csv(mypath, header=TRUE)
names(Example_data)

## -- If do not want to subset the data, then should use the DataCleaner function
myCleanSubset <- SubsetCleaner(Example_data, fact="Plot", 
                               respo="Seed_removal_percent", sub="Season", 
                               warn=FALSE)
}