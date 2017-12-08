\name{GraphCompare}
\alias{GraphCompare}
\title{
Function to Produce Boxplots
}
\description{
This function takes a list created by either \code{SubsetCleaner} or \code{DataCleaner} and graphs boxplots for each of the \code{fact} levels. 
}
\usage{
GraphCompare(data, groups = FALSE, clean_bg = TRUE, x_label = "Variable", y_label = "Response", bold_labels = FALSE, vert_facet = TRUE)
}
%- maybe also 'usage' for other objects documented here.
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
X axis label. Default is \code{"Variable"}.
}
  \item{y_label}{
Y axis label. Default is \code{"Response"}.
}
  \item{bold_labels}{
Either \code{TRUE} or \code{FALSE}. The default is \code{FALSE}. Bolds the axes labels.
}
  \item{vert_facet}{
Either \code{TRUE} or \code{FALSE}. The default is \code{TRUE}. If \code{groups=TRUE} then this determines whether the subgroupings are separated and graphed vertically (\code{TRUE}) or horizontally (\code{FALSE}).
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A graph is returned.
}
\author{
Selina A. Ruzi
}
%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{SubsetCleaner}}
\code{\link{DataCleaner}}
\code{\link{ggplot}}
}
\examples{
## -- Find location of package on computer
data_path <- system.file("extdata", "Example_data.csv", package="multiple_comparisons")

## -- Read in raw data
raw_data <- read.csv(data_path, header=TRUE)
names(raw_data)
# "Season"               "Plot"                 "Seed_species"        "Seed_removal_percent"

## -- No Subsetting Example
myCleanData <- DataCleaner(raw_data, fact="Plot", 
respo="Seed_removal_percent", 
warn=FALSE)

MyDataGraph <- GraphCompare(myCleanData, groups=FALSE,
             clean_bg=TRUE, x_label="Plot",
             y_label="% Seed Removal",
             bold_labels=TRUE)
MyDataGraph


## -- Subsetting Example
myCleanSubset <- SubsetCleaner(raw_data, fact="Plot", 
respo="Seed_removal_percent", sub="Season", 
warn=FALSE)

MySubsetGraph <- GraphCompare(myCleanSubset, groups=TRUE, 
             clean_bg=TRUE, x_label="Plot", y_label="% Seed Removal",
             bold_labels=TRUE, vert_facet=TRUE)
MySubsetGraph

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
