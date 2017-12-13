\name{Mult_T_Tests}
\alias{Mult_T_Tests}
\title{
Function to Conduct Multiple T-Tests
}
\description{
This function takes a list created by either \code{SubsetCleaner} or \code{DataCleaner} and conducts multiple t-test comparissons between the \code{fact} levels. 
}
\usage{
Mult_T_Tests(data, groups = FALSE, paired = FALSE, test = "bonferroni", 
             alpha_value = 0.05, ...)
}
\arguments{
  \item{data}{
Is a list output from either \code{SubsetCleaner} or \code{DataCleaner}
}
  \item{groups}{
Either \code{TRUE} or \code{FALSE}. If \code{data} came from \code{SubsetCleaner} then this value needs to be set to \code{TRUE}. If \code{data} came from \code{DataCleaner} then this value needs to be set to \code{FALSE}. The default is \code{FALSE}.
}
  \item{paired}{
Is an aurgement passed to \code{t.test} for whether the data being compared is paired or not. This defaults to \code{FALSE}.
}
  \item{test}{
Defaults to Bonferonni. Is passed to \code{p.adjust} to tell it what method to alter the p-value by. Look up appropriate methods to use here by looking at \code{?p.adjust()}.
}
  \item{alpha_value}{
Defaults to \code{0.05}. This is the orignal p-value that will be manipulated based on the \code{test} aurgument. 
}
  \item{\dots}{
Other arguments to be passed to \code{t.test}. 
}
}
\value{
\item{Output_table}{consisting of a table with the comparison, p-values, adjusted p-values, and whether the adjusted p-value is less than or equal to the alpha value}
\item{Summary_table}{a table with the \code{fact} levels and associated average \code{respo}, sorted from highest to lowest}
}
\author{
Selina A. Ruzi
}
\seealso{
\code{\link{SubsetCleaner}}
\code{\link{DataCleaner}}
\code{\link{t.test}}
\code{\link{p.adjust}}
\code{\link{data.table}}
}
\examples{
## -- find path
mypath <- system.file("extdata", "Example_data.csv", package="multipleComparisons")

## -- read in data
Example_data <- read.csv(mypath, header=TRUE)
names(Example_data)

## -- If do not want to further subset the data, do the following
myCleanData <- DataCleaner(Example_data, fact="Plot", 
respo="Seed_removal_percent", 
warn=FALSE)

## -- No Subsetting Example
myDataTests <- Mult_T_Tests(myCleanData, groups=FALSE)


## -- Subsetting Example

myCleanSubset <- SubsetCleaner(Example_data, fact="Plot", respo="Seed_removal_percent", 
                               sub ="Season", warn=FALSE)

mySubsetTests <- Mult_T_Tests(myCleanSubset, groups=TRUE)
}
