##Plot categorized data.table by bar ggplot

library(ggplot2)

##dt = data.table, outcome = main varible you want to count, byval = variable you want to categorize "count"
##inv = if T, change order
byplot <- function(dt, outcome, byval, position = "stack", inv = F, type = "bar", binwidth = 100) {
  
  if (inv == T) {
    
    ptr1 <- dt[, .N, by = c(outcome, byval)][order(get(byval))]
    
  }   else {
    ptr1 <- dt[, .N, by = c(outcome, byval)][order(-get(byval))]
  }
  
  
  
  if (type == "bar") {
  
    ggptr1 <- ggplot(ptr1, aes_string(x = outcome, y = "N", fill = byval))
    
    plot <- ggptr1 + geom_bar(stat = "identity", position = position)
  
   }

  if (type == "freq") {
  
    ggptr1 <- ggplot(ptr1, aes_string(x = "N", color = byval)) 

    plot <- ggptr1 + geom_freqpoly(binwidth = binwidth)
    
  }
  
  if (type == "hist") {
    
    ggptr1 <- ggplot(ptr1, aes_string(x = "N", fill = byval)) 
    
    plot <- ggptr1 + geom_histogram(binwidth = binwidth)
    
  }
  return(plot)    
  
  }