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
%- maybe also 'usage' for other objects documented here.
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Selina A. Ruzi
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{DataCleaner}}
}
\examples{
## -- Find location of package on computer
data_path <- system.file("extdata", "Example_data.csv", package="multiple_comparisons")

## -- Read in raw data
raw_data <- read.csv(data_path, header=TRUE)
names(raw_data)
# "Season"               "Plot"                 "Seed_species"        "Seed_removal_percent"

## -- If do not want to subset the data, then should use the DataCleaner function
myCleanSubset <- SubsetCleaner(raw_data, fact=Plot, 
respo=Seed_removal_percent, sub=Season, warn=FALSE)
str(myCleanSubset)

}