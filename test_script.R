## test script 

setwd("/Users/Selina/Documents/GitHub/multiple_comparisons")

setwd("/Users/Selina/Documents/GitHub/multiple_comparisons/R")
source("mult_compare_functions.R")
source("subset_cleaner_function.R")
source("mult_t_tests_function.R")

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
names(raw_data)

testing3 <- SubsetCleaner(x=raw_data, sub="Season", fact="y", respo="Total.Seeds.Removed", warn=TRUE)
testing3 #because warnings are on and they are not in the SubsetCleaner but in the DataCleaner
#the warnings aren't displayed when using this function. will need to add it to the SubsetCleaner
testing4 <- SubsetCleaner(x=raw_data, sub="Season", fact="Plot", respo="Total.Seeds.Removed", warn=TRUE)
testing4
testing4$sub_compare_list
length(testing4$sub_compare_list)
testing4$sub_data_frame
length(testing4$sub_data_frame)

trial <- testing[[1]]
trial2 <- names(trial)
trial3 <- combn(trial2, 2)
trial3[,1]
#so each column has a different pairing, use it to index? 

setwd("/Users/Selina/Documents/GitHub/multiple_comparisons/R")
source("mult_t_tests_function.R")
library(data.table)
Mult_T_Tests(testing, groups=FALSE, paired=FALSE)
source("mult_t_tests_function.R")
Mult_T_Tests(testing4, groups=TRUE, paired=FALSE)
