##Plot categorized data.table by bar ggplot

library(ggplot2)

##dt = data.table, count = main varible you want to count, byval = variable you want to categorize "count"
##inv = if T, change order
byplot <- function(dt, count, byval, stat = "identity", inv = F) {
  
  if (inv == T) {
    
    ptr1 <- train[, lapply(.SD, length), by = c(count, byval)][order(get(byval))]
    
  }   else {
    ptr1 <- train[, lapply(.SD, length), by = c(count, byval)][order(-get(byval))]
  }
  
  ggptr1 <- ggplot(ptr1, aes_string(x = count, y = "AnimalID"))
  
  plot <- ggptr1 + geom_bar(stat = stat) + aes_string(fill = byval)
  
  return(plot)    
}