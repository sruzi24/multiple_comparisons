\name{Mult_T_Tests}
\alias{Mult_T_Tests}
\alias{DataCleaner}
\alias{SubsetCleaner}
\title{
Function to Conduct Multiple T-Tests
}
\description{
This function takes a list created by either \code{SubsetCleaner} or \code{DataCleaner} and conducts multiple t-test comparissons between the \code{fact} levels. 
}
\usage{
Mult_T_Tests(data, groups = FALSE, paired = FALSE, test = "bonferroni", alpha_value = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
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
\details{
The outupt of this function is a list consisting of a table with the comparison, p-values, adjusted p-values, and whether the adjusted p-value is less than or equal to the alpha value, and a table with the \code{fact} sorted from highest to lowest. 
}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Selina A. Ruzi
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{SubsetCleaner}}
\code{\link{DataCleaner}}
\code{\link{t.test}}
\code{\link{p.adjust}}
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

myDataTests <- Mult_T_Tests(myCleanData, groups=FALSE)

## -- Subsetting Example
myCleanSubset <- SubsetCleaner(raw_data, fact="Plot", 
respo="Seed_removal_percent", sub="Season", 
warn=FALSE)

MySubsetTests <- Mult_T_Tests(myCleanSubset, groups=TRUE)


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
