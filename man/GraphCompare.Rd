\name{GraphCompare}
\alias{GraphCompare}
\title{
Function to Produce Boxplots
}
\description{
This function takes a list created by either \code{SubsetCleaner} or \code{DataCleaner} and graphs boxplots for each of the \code{fact} levels. 
}
\usage{
GraphCompare(data, groups = FALSE, clean_bg = TRUE, x_label, y_label, bold_labels = FALSE, vert_facet = TRUE)
}
\arguments{
  \item{data}{
Is a list output from either \code{SubsetCleaner} or \code{DataCleaner}
}
  \item{groups}{
Either \code{TRUE} or \code{FALSE}. If \code{data} came from \code{SubsetCleaner} then this value needs to be set to \code{TRUE}. If \code{data} came from \code{DataCleaner} then this value needs to be set to \code{FALSE}. The default is \code{FALSE}.
}
  \item{clean_bg}{
Either \code{TRUE} or \code{FALSE}. IF \code{TRUE} then the background of the graph will be white. The default is \code{TRUE}.
}
  \item{x_label}{
X axis label.
}
  \item{y_label}{
Y axis label.
}
  \item{bold_labels}{
Either \code{TRUE} or \code{FALSE}. The default is \code{FALSE}. Bolds the axes labels.
}
  \item{vert_facet}{
Either \code{TRUE} or \code{FALSE}. The default is \code{TRUE}. If \code{groups=TRUE} then this determines whether the subgroupings are separated and graphed vertically (\code{TRUE}) or horizontally (\code{FALSE}).
}
}
\value{
A graph is returned.
}
\author{
Selina A. Ruzi
}
\seealso{
\code{\link{SubsetCleaner}}
\code{\link{DataCleaner}}
\code{\link{ggplot}}
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

MyDataGraph <- GraphCompare(myCleanData, groups=FALSE, clean_bg=TRUE, x_label="Plot", y_labe ="Seed Removal Percent", bold_labels=TRUE)
MyDataGraph


## -- Subsetting Example
myCleanSubset <- SubsetCleaner(Example_data, fact="Plot", respo="Seed_removal_percent", sub ="Season", warn=FALSE)

MySubsetGraph <- GraphCompare(myCleanSubset, groups=TRUE, clean_bg=TRUE, x_label="Plot", y_label="Seed Removal Percent", bold_labels=TRUE, vert_facet=TRUE)
MySubsetGraph

}
