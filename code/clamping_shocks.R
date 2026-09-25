total <- sum(processing_faf$thousand.tons.in.2023)
counter <- 1
processing_faf <- processing_faf[order(
  processing_faf$thousand.tons.in.2023,
  decreasing = TRUE
), ]
total_weight <- 0

while (total_weight<0.98*total) {
  total_weight <<- total_weight + processing_faf$thousand.tons.in.2023[counter]
  counter <- counter + 1
  
}
processing_faf <- processing_faf[1:counter,]
write.csv(processing_faf,paste(parent_direct,"data/cleaned_faf.csv",sep=""))