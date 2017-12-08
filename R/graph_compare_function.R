GraphCompare <- function(data, groups=FALSE, clean_bg=TRUE, x_label="Variable", y_label="Response", bold_labels=FALSE, vert_facet=TRUE){
  
  temp_data <- data[[2]]
  
  # as though there are no subgroupings
  #base plot the graph
  if(groups == FALSE){
  a <- ggplot(temp_data, aes(x=variable,y=response))+ geom_boxplot()
  }
  
  if(groups == TRUE){
    # need to get the different parts out of the 
    # data[[2]], then need to bind them together into
    # one dataframe, then need to make the base graph of it
    
    #temp_data2 will be the final dataframe that will be used in the graphing
    num_groupings <- length(temp_data)
    group_names <- names(temp_data)
    
    #need to initialize a dataframe here ---
    temp_data2 <- data.frame(variable=as.factor(character()),
                     response=numeric(),
                     sub_name=as.factor(character()))
    #index <- 1
    for(i in 1:length(group_names)){
      current_data <- temp_data[[i]]
      sub_name <- rep(group_names[i], nrow(current_data))
      # still working on this part here
      current_data_frame <- cbind(current_data, sub_name)
      #print(current_data_frame)
      names(current_data_frame) <- c("variable", "response","sub_name")
      if(i == 1){
        temp_data2 <- current_data_frame
        #print(temp_data2) for debugging
      } else {
        temp_data2 <- rbind(temp_data2, current_data_frame)
        #print(temp_data2) for debugging
      }

    }
    
    a <- ggplot(temp_data2, aes(x=variable, y=response))+geom_boxplot()
    
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

