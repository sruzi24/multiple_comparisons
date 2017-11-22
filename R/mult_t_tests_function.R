Mult_T_Tests <- function(data, groups=FALSE, paired=FALSE, test="bonferroni", ...){
  #data is a list output from either DataCleaner or SubsetCleaner
  #groups defaults to FALSE, but if it is true it means that the output was originally
  #from SubsetCleaner, paired=defaults to FALSE but if true will conduct a paired
  # t-test instead, test is set to "bonferroni as a default but otherwise look at the 
  #methods from p.adjust.methods, should be lowercase
  
  temp_data <- data[[1]]
  
  
  #define a function to conduct the t-tests
  T_Compare <- function(y, paired=FALSE, ...){
    trial_names <- names(temp_data) # to pull out the names of the comparisons 
    all_pairwise_combinations <- combn(trial_names, 2)
    p_value_outputs <- vector(mode="numeric", ncol(all_pairwise_combinations))
    comparisons <- vector(mode="character", ncol(all_pairwise_combinations))
    #can do vector math for the other two columns of what I will end up needing to output
    for(i in 1:ncol(all_pairwise_combinations)){
      name1 <- all_pairwise_combinations[1,i]
      name2 <- all_pairwise_combinations[2,i]
      name1_location <- which(names(temp_data)==name1)
      name1_data <- temp_data[[name1_location]]
      name2_location <- which(names(temp_data)==name2)
      name2_data <- temp_data[[name2_location]]
      p_value_outputs[i] <- t.test(as.numeric(name1_data), as.numeric(name2_data),
                                   paired=paired, ...)$p.value
      comparisons[i] <- paste(name1, name2, sep="-")
    }
    list_output <- list(comparisons=comparisons, p_value_outputs=p_value_outputs)
    return(list_output)
  }
  
  
  
  
  
  
  #to check if the input is a list
  if(is.list(data)==TRUE){
  
  #If there are subgroups will need to break them up into different sections to 
    #then compare little by little before comparing them the way that if groups
    # were false 
  if(groups == TRUE){
    print("Groups set to TRUE")
      }
  }
  
  #If there aren't subgroupings then do this    
  if(groups == FALSE){
    
    temporary <- T_Compare(y=temp_data, paired=FALSE)
    
    comparisons <- temporary$comparisons
    p_value_outputs <- as.numeric(temporary$p_value_outputs)
    
    adjusted_pvalue <- p.adjust(p=p_value_outputs, method=test)
      #p_value_outputs * length(comparisons)
    
    T_F <- (adjusted_pvalue < 0.05) | (adjusted_pvalue == 0.05)
    output_table <- data.table(Comparison=comparisons, P_values=p_value_outputs, 
                               Ajusted_p_values=adjusted_pvalue,
                               Significant=T_F)
    return(output_table)
    
  } else {
    stop("Data should be a list that was an output from either DataCleaner or SubsetCleaner")
  }
}