#Problem 6

#Our possibilities range from 0 to 15 black beans
beans <- c(0:15)
beans

#Full binomial distribution
pbeans <- dbinom(beans,15,.35)

results <- data.frame(beans,pbeans)
results
#
barplot(pbeans,names.arg = beans,ylab = "frequency",xlab = "number of dark beans")
