pnorm(70.9,mean = 63.83,sd=2.95)->q2c
pnorm(60.5,mean = 63.83,sd=2.95)->q2d
c-d

#HW 5: Questions 6 and 7

#####Problem 6: Height statistics
#str(hw5_data)
meanHeight = mean(hw5_data$height);meanHeight
sdHeight = sd(hw5_data$height);sdHeight
varHeight = var(hw5_data$height);varHeight
#mean of height = 65.81618; sd = 4.126638; var = 17.02914

boxplot(hw5_data$height)
hist(hw5_data$height, ylab = "frequency", xlab = "height (in.)", main = "Height of class", col = "green")
#
#
#
#
#####Problem 7: Hand Binomial Distribution
table(hw5_data$hand)
# 62 Right, 6 Left-handed

pRight = 62/68; pRight
pRight2 = 0.91
#0.91176

#7a
dbinom(3,10,pRight)
#7b
pbinom(3,10,pRight)
# 7a) 3.787267e-06
# 7b) 3.927693e-06
#
#
#
#7a
dbinom(3,10,pRight2)
#7b
pbinom(3,10,pRight2)
# 
# 4.325168e-06
# 4.48914e-06

# PROBLEM 6
#
# This is using the height data they collected in class.  

t.test(hw5_data$height, conf.level = 0.90)
# 90 percent confidence interval:
#   64.98150  66.65085
