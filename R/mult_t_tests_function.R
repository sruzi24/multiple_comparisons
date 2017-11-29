Mult_T_Tests <- function(data, groups=FALSE, paired=FALSE, test="bonferroni", ...){
  #data is a list output from either DataCleaner or SubsetCleaner
  #groups defaults to FALSE, but if it is true it means that the output was originally
  #from SubsetCleaner, paired=defaults to FALSE but if true will conduct a paired
  # t-test instead, test is set to "bonferroni as a default but otherwise look at the 
  #methods from p.adjust.methods, should be lowercase
  
  temp_data <- data[[1]]
  
  
  #define a function to conduct the t-tests
  T_Compare <- function(y, paired=paired, test=test, ...){
    trial_names <- names(y) # to pull out the names of the comparisons 
    all_pairwise_combinations <- combn(trial_names, 2)
    p_value_outputs <- vector(mode="numeric", ncol(all_pairwise_combinations))
    comparisons <- vector(mode="character", ncol(all_pairwise_combinations))
    #can do vector math for the other two columns of what I will end up needing to output
    for(i in 1:ncol(all_pairwise_combinations)){
      name1 <- all_pairwise_combinations[1,i]
      name2 <- all_pairwise_combinations[2,i]
      name1_location <- which(names(y)==name1)
      name1_data <- y[[name1_location]]
      name2_location <- which(names(y)==name2)
      name2_data <- y[[name2_location]]
      p_value_outputs[i] <- t.test(as.numeric(name1_data), as.numeric(name2_data),
                                   paired=paired, ...)$p.value
      comparisons[i] <- paste(name1, name2, sep="-")
    }
    adjusted_pvalue <- p.adjust(p=p_value_outputs, method=test)
    
    T_F <- (adjusted_pvalue < 0.05) | (adjusted_pvalue == 0.05)
    output_table <- data.table(Comparison=comparisons, P_values=p_value_outputs, 
                               Ajusted_p_values=adjusted_pvalue,
                               Significant=T_F)
    
    #to make summary table
    avg_outputs <- vector(mode="numeric", length(trial_names))
    for(i in 1:length(trial_names)){
      ID <- trial_names[i]
      ID_location <- which(names(y)==ID)
      avg_outputs[i] <- mean(as.numeric(y[[ID_location]]))
    }
    summary_output <- data.table(Name=trial_names, Avg=avg_outputs)
    summary_output <- summary_output[order(-Avg)]
    
    #to return both outputs together
    compare_outputs <- append(list(Output_table=output_table),
                              list(Summary_table=summary_output))
    return(compare_outputs)
  }
  
  
  
  #to check if the input is a list
  if(is.list(data)==TRUE){
  
  #If there are subgroups will need to break them up into different sections to 
    #then compare little by little before comparing them the way that if groups
    # were false 
  if(groups == TRUE){
    num_groupings <- length(temp_data)
    group_names <- names(temp_data)
    #print(length(group_names))
    table_outputs <- list(length(group_names))
    for(i in 1:length(group_names)){
      temporary_data <- temp_data[[i]]
      print(temporary_data)
      t <- T_Compare(y=temporary_data, paired=paired, test=test)
      #print(t)
      table_outputs[[i]] <- t
    }
    
    names(table_outputs) <- group_names
    
    return(table_outputs)
    #need to break about by groupsing then pass to the function that does the t-test
    # then need to return a bunch of different tables 
    
      }
  }
  
  #If there aren't subgroupings then do this    
  if(groups == FALSE){
    
    temporary <- T_Compare(y=temp_data, paired=paired, test=test)
    
    return(temporary)
    
  }
  if(groups != TRUE && groups != FALSE){
    stop("Data should be a list that was an output from either DataCleaner or SubsetCleaner")
  }
}


#### also need to write it to give the letters so that can graph it with the letters
#### and need to get the average values I think to try to figure that out 
#### though that may end up needing to be a whole different table that is output for the values
### maybe I should just put that into the graphing function instead 