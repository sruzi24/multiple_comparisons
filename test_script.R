## test script 

setwd("/Users/Selina/Documents/GitHub/multiple_comparisons")

setwd("/Users/Selina/Documents/GitHub/multiple_comparisons/R")
source("mult_compare_functions.R")
source("subset_cleaner_function.R")
source("mult_t_tests_function.R")
source("graph_compare_function.R")

library(ggplot2)
### test data
setwd("/Users/Selina/Dropbox/Seed_Removal_BCI/Data/Data May 2015")
raw_data<-read.csv("2013_aboveground_totals_may2015.csv")
names(raw_data)
testing<-DataCleaner(x=raw_data, fact="Plot",respo="Total.Seeds.Removed", warn=TRUE)

GraphCompare(data=testing)
GraphCompare(data=testing, clean_bg=TRUE)
GraphCompare(data=testing, clean_bg=FALSE)
testing_graph <- GraphCompare(data=testing, clean_bg=TRUE, 
             bold_labels=TRUE, x_label="Plot",
             y_label="Seed Removal")
testing_graph
source("graph_compare_function.R")
GraphCompare(data=testing4, groups=TRUE, clean_bg=TRUE,
             bold_labels=TRUE, x_label="Plot",
             y_label="Seed Removal", vert_facet=TRUE)

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
Mult_T_Tests(testing, groups=FALSE, paired=FALSE, alpha_value=0.05) 
source("mult_t_tests_function.R")
x  <- Mult_T_Tests(testing4, groups=TRUE, paired=FALSE)
x
x2 <- Mult_T_Tests(testing4, groups=TRUE, paired=FALSE, alpha_value=0.5)
x2



testing4[[1]]

x[[2]]

y <- testing4$sub_compare_list
y_wet <- y$Wet
mean(y_wet$Pearson)
mean(y_wet$`25Ha`)
mean(y_wet$AVA)
mean(y_wet$Drayton)
mean(y_wet$Zetek)
x_wet <- x$Wet
x_wet_summary_table <- as.data.table(x_wet$Summary_table)
x_wet_summary_table

x_wet_output_table <- as.data.table(x_wet$Output_table)
x_wet_output_table
sort(x_wet_summary_table)
?sort()
 
z <- x_wet_summary_table[order(-Avg)]

comparison <- x_wet_output_table$Comparison

## i think this may need to be a recursive function so that
# i can get rid of a name each time 

## it also isn't looping through all the names...
for(i in 1:nrow(z)){ # z is the summary table
  temp_name1 <- z[i,1]
  print(temp_name1)
  temp_avg <- z[i,2]
  new_names <- z[-i,1]
  for(name in 1:length(new_names)){
    name_combo1 <- paste(temp_name1, new_names[name], sep="-")
    name_combo2 <- paste(new_names[name], temp_name1, sep="-")
    print(name_combo1)
    print(name_combo2)
    #if((name_combo1 %in% comparison) == TRUE){
    #  combo_location <- which(comparison == name_combo1)
    #  print(combo_location)
    #  print(name_combo1)
    #} else {
    #  combo_location <- which(comparison == name_combo2)
    #  print(combo_location)
    #  print(name_combo2)
    #}
  }
  print(new_names)
}
  

comparison2 <- t(as.data.table(strsplit(comparison, "-")))
comparison2
colnames(comparison2) <- c("name1","name2")
comparison2 <- as.data.table(comparison2)


z_ordered <- z[order(Name)]

## this also isn't working 
letters <- vector(mode="character", nrow(z))
#to get a list of the location for each of the comparisons
# and to name it with the two pairs compared in different columns
for(i in 1:nrow(z_ordered)){ # z is the summary table
  temp_name1 <- z_ordered[i,1]
  print(paste("temp name is", temp_name1))
  temp_avg <- z_ordered[i,2]
  new_names <- z_ordered[-i,1]
  for(n in new_names){
    temp_name2 <- n
    name_combo1 <- paste(temp_name1, temp_name2, sep="-")
    print(paste("name combo 1 is", name_combo1))
    index <- length(comparison)
    for(i in 1:index){
      if(comparison[i] == name_combo1){
        location <- index[i]
        print(location)
      }
    }
    
    
    
    
    
     #test1_location <- which(x_wet_output_table$Comparison==name_combo1)
    #test1 <- match(name_combo1, comparison)
    #print(test1)
    #print(test1_location)
    #for(i in 1:length(test1)){
     # num <- test1[i]
      #print(num)
      #if(num = "NA"){
       # print("There is an NA here")
      #}# else {
      #  T_or_F <- x_wet_output_table[num,Significant]
      #  print(T_or_F)
      #}
    #}
    }
  }


a <- data.table(z[,1], letters)
a
comparison

class(z_ordered)
ab <- unique(z_ordered$Name)
loc <- which(ab[] == "Zetek")
ab_minus <- ab[-loc]
ab_minus

  for(i in 1:nrow(z_ordered)){ # z is the summary table
    temp_name1 <- z_ordered[i,1]
    #print(paste("temp name is", temp_name1))
    temp_avg <- z_ordered[i,2]
    #new_names <- unique(z_ordered$Name)
    #print(new_names)
    #temp_name1_loc <- which(new_names[] == temp_name1)
    #print(paste("temp_location", temp_name1_loc))
    #new_names <- new_names[-temp_name1_loc]
    new_names <- z_ordered[-i,1]
    #print(paste(" this is ", class(new_names)))
    #print(paste("new_names is this long", length(new_names)))
    for(n in 1:nrow(new_names)){
      temp_name2 <- new_names[n,1]
      #print(paste("this is temp_name2", temp_name2))
      #print(length(temp_name2))
      name_combo1 <- paste(temp_name1, temp_name2, sep="-")
      #print(paste("name combo 1 is", name_combo1, sep="/n"))
      #pattern <- "(temp_name1)(temp_name2)(-)"
      name1_grep <- grep(temp_name1, comparison)
      name2_grep <- grep(temp_name2, comparison)
      #print(name1_grep)
      #print(name2_grep)
      location <- name1_grep[name1_grep %in% name2_grep]
      print(paste(name_combo1, location))# this is now working but now 
      #I have the problem that I'm getting double answers 
      #for everything because there are two ways to have the same 
      #combination
      }
    }

grep("(Zetek)(AVA)(-)", comparison)
grep("(AVA)(Zetek)(-)", comparison)
grep("(Zetek)(AVA)", comparison, ignore.case=TRUE)
grep("(AVA)(Zetek)", comparison)

grep("Zetek", comparison)
grep("c(Zetek,AVA)", comparison)
Zetek_grep <- grep("(Zetek)", comparison)
AVA_grep <- grep("AVA", comparison)
grep("(Zetek)", comparison) & grep("AVA", comparison)

(Zetek_grep, AVA_grep)

Zetek_grep
AVA_grep

Zetek_grep[Zetek_grep %in% AVA_grep]


for(i in 1:nrow(z)){ # z is the summary table
  temp_name1 <- z[i,1]
  #print(paste("temp name is", temp_name1))
  temp_avg <- z[i,2]
  #new_names <- unique(z_ordered$Name)
  #print(new_names)
  #temp_name1_loc <- which(new_names[] == temp_name1)
  #print(paste("temp_location", temp_name1_loc))
  #new_names <- new_names[-temp_name1_loc]
  new_names <- z[-i,1]
  #print(paste(" this is ", class(new_names)))
  #print(paste("new_names is this long", length(new_names)))
  for(n in 1:nrow(new_names)){
    temp_name2 <- new_names[n,1]
    #print(paste("this is temp_name2", temp_name2))
    #print(length(temp_name2))
    name_combo1 <- paste(temp_name1, temp_name2, sep="-")
    #print(paste("name combo 1 is", name_combo1, sep="/n"))
    #pattern <- "(temp_name1)(temp_name2)(-)"
    name1_grep <- grep(temp_name1, comparison)
    name2_grep <- grep(temp_name2, comparison)
    #print(name1_grep)
    #print(name2_grep)
    location <- name1_grep[name1_grep %in% name2_grep]
    print(paste(name_combo1, location))# this is now working but now 
    #I have the problem that I'm getting double answers 
    #for everything because there are two ways to have the same 
    #combination
  }
}


testing4[[2]] # subsets out the dataframes of the different subgroups




sample(1:100,10, replace=TRUE)
sample(0:20, 10, replace=TRUE)

?system.file

#system.file(package="multiple_comparisons") #will give the location where this
#package is saved on a computer -- will need this eventually for the checking
#of the package stuff in the R help if I want it to run with a fiile that is 
#installed with the package

#the file that I want to be installed with the package
setwd("/Users/Selina/Documents/GitHub/multiple_comparisons/inst/extdata")
test_data <- read.csv("Example_data.csv", header=TRUE)
head(test_data)
str(test_data)
names(test_data)

setwd("/Users/Selina/Documents/GitHub/multiple_comparisons/R")
source("mult_compare_functions.R")
source("subset_cleaner_function.R")
source("mult_t_tests_function.R")
source("graph_compare_function.R")

myCleanData <- DataCleaner(test_data, fact="Plot", respo="Seed_removal_percent", warn=FALSE)
myCleanSubset <- SubsetCleaner(test_data, fact="Plot", respo="Seed_removal_percent",
                               warn=FALSE, sub="Season")
myCleanSubset[[1]]
str(myCleanSubset)

library(data.table)

Subset_tests <- Mult_T_Tests(myCleanSubset, groups=TRUE)
Subset_tests
no_sub_tests <- Mult_T_Tests(myCleanData, groups=FALSE)
no_sub_tests

library(ggplot2)
GraphCompare(myCleanSubset, groups=TRUE, 
             clean_bg=TRUE, x_label="Plot", y_label="% Seed Removal",
             bold_labels=TRUE, vert_facet=TRUE)

GraphCompare(myCleanData, groups=FALSE,
             clean_bg=TRUE, x_label="Plot",
             y_label="% Seed Removal",
             bold_labels=TRUE)
