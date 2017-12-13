GraphCompare <- function(data, groups=FALSE, clean_bg=TRUE, x_label, 
                         y_label, bold_labels=FALSE, vert_facet=TRUE){
  #data is a list output from either DataCleaner or SubsetCleaner
  #groups defaults to FALSE, but if it is true it means that the output was originally from SubsetCleaner
  #clean_bg either TRUE or FALSE to determine if the background will be white or grey respectively
  #x_label and y_label to alter the axes labels otherwise defaults to "Variable" and "Response"
  #bold_labels either TRUE or FALSE to have axes labeles bold or not
  #vert_facet either TRUE or FALSE. Argument is only used if groups=TRUE to facet the subgroups either vertically or horizonatally
  
  
  #pulls out the dataframe from the list generated from DataCleaner
  #if(groups == FALSE) { temp_data <- as.data.frame(data[["data_frame"]]) }

  #pulls out the dataframe from the list generated from SubsetCleaner
  #if(groups == TRUE) { temp_data <- as.data.frame(data[["sub_data_frame"]]) }
  
  #data_length <- length(data)
  
  temp_data <- data[[2]]
  
  # as though there are no subgroupings
  #base plot the graph
  if(groups == FALSE){
  a <- ggplot(temp_data, aes(x=temp_data$variable,y=temp_data$response))+ geom_boxplot()
  }
  
  if(groups == TRUE){
    #to determine how many subgroups
    num_groupings <- length(temp_data)
    #to pull out the subgroup names
    group_names <- names(temp_data)
    
    #loop through the subgroups to merge them all together
    #into one dataframe to graph from
    for(i in 1:length(group_names)){
      current_data <- temp_data[[i]]
      n_row <- nrow(current_data)
      sub_name <- rep(group_names[i], times=n_row)
      current_data_frame <- cbind(current_data, sub_name)
      #print(current_data_frame) #for debugging
      current_data_frame <- as.data.frame(current_data_frame)
      names(current_data_frame) <- c("variable", "response","sub_name")
      if(i == 1){
        temp_data2 <- current_data_frame
        #print(temp_data2) # for debugging
      } else {
        temp_data2 <- rbind(temp_data2, current_data_frame)
        #print(temp_data2) # for debugging
      }

    }
    
    #base graph for when groups=TRUE
    a <- ggplot(temp_data2, aes(x=temp_data2$variable, y=temp_data2$response))+geom_boxplot()
    
    if(vert_facet == TRUE){
      a <- a+ facet_grid(factor(sub_name) ~ .) # facet vertically
    } else {
      a <- a+ facet_grid(. ~ factor(sub_name)) # facet horizontally
    }
  }
  
  #to clean up the background
  if(clean_bg == TRUE){
    a <- a + theme(panel.grid.minor=element_blank(), #gets rid of grey and lines in the middle
                  panel.grid.major=element_blank(), #gets rid of grey and lines in the middle
                  panel.background=element_rect(fill="white"),#gets rid of grey and lines in the middle
                  panel.border=element_blank(), #gets rid of square going around the entire graph
                  axis.line = element_line(colour = 'black', size = 0.5),#sets the axis line size
                  axis.ticks=element_line(colour = 'black', size = 0.5)) #sets the tick lines
  } 
  
  #to alter the axes titles
  a <- a + xlab(x_label)+ ylab(y_label)
  
  #to change axes labels to bold
  if(bold_labels == TRUE){
  a <- a + theme(axis.title.x = element_text(face="bold", size=12, color="black"), #size of x-axis title
          axis.title.y = element_text(face="bold", size=12, color="black")) #size of y-axis title
          
  }
    
  return(a)
 }

