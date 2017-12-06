GraphCompare <- function(data, groups=FALSE, clean_bg=TRUE, x_label="Variable", y_label="Response", bold_labels=FALSE, ...){
  
  temp_data <- data[[2]]
  
  # as though there are no subgroupings

  #base plot the graph
  a <- ggplot(temp_data, aes(x=variable,y=response))+
    geom_boxplot()
  
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

