## test script 

setwd("/Users/Selina/Documents/GitHub/multiple_comparisons/R")
source("mult_compare_functions.R")

### test data
setwd("/Users/Selina/Dropbox/Seed_Removal_BCI/Data/Data May 2015")
raw_data<-read.csv("2013_aboveground_totals_may2015.csv")
names(raw_data)
testing<-DataCleaner(x=raw_data, fact="Plot",respo="Total.Seeds.Removed", warn=TRUE)
DataCleaner(x=raw_data, fact="y",respo="Total.Seeds.Removed") #should give an error
DataCleaner(x=raw_data, fact="Plot",respo="y") #should give an error
testing
names(testing)
testing$compare_list
names(testing$compare_list)
testing$data_frame
testing2 <- as.data.frame(testing$data_frame)
testing2
class(testing2)
