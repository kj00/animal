##Plot categorized data.table by bar ggplot

library(ggplot2)
library(reshape2)
##dt = data.table, outcome = main varible you want to count, byval = variable you want to categorize "count"
##inv = if T, change order
byplot <- function(dt, outcome, byval, position = "stack", inv = F, type = "bar", binwidth = 100) {
  
  if (inv == T) {
    
    subdt <- dt[, .N, by = c(outcome, byval)][order(get(byval))]
    
  }   else {
    subdt <- dt[, .N, by = c(outcome, byval)][order(-get(byval))]
  }
  
  
  
  if (type == "bar") {
  
    ggdt <- ggplot(subdt, aes_string(x = outcome, y = "N", fill = byval))
    
    plot <- ggdt + geom_bar(stat = "identity", position = position)
  
   }

  if (type == "freq") {
  
    ggdt <- ggplot(subdt, aes_string(x = "N", color = byval)) 

    plot <- ggdt + geom_freqpoly(binwidth = binwidth)
    
  }
  
  if (type == "hist") {
    
    ggdt <- ggplot(subdt, aes_string(x = "N", fill = byval)) 
    
    plot <- ggdt + geom_histogram(binwidth = binwidth)
    
  }
  
  if (type == "line"){
    
    ggdt <- ggplot(subdt, aes_string(x = byval))
    plot <- ggdt + geom_line(aes_string(y = "N", colour = outcome))
    
  }

  if (type == "point"){
    
    ggdt <- ggplot(subdt, aes_string(x = byval))
    plot <- ggdt + geom_point(aes_string(y = "N", colour = outcome))
    
  }
  
  return(plot)    
  
  }