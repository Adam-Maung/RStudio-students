# HOMEWORK 12

# We'll use R again, but for the problems they need to do by hand we'll go step by step.

# PROBLEM 1

# Reading data into R
height <- scan(nlines = 1)
60	67	62	70	66

weight <- scan(nlines = 1)
119	143	131	155	136

# (a) 
plot(height,weight,xlab = "height",ylab = "weight")

# (b):
mh <- mean(height)
mw <- mean(weight)
mh;mw
ssh <- var(height)*(length(height)-1)
ssw <- var(weight)*(length(weight)-1)
ssh;ssw

# Now let's get SScp:
shc <- height - mean(height)
swc <- weight - mean(weight)
cp <- shc*swc
# Here is SScp:
sscp <- sum(cp)
sscp


# (c) to get r:
r <- sum(cp)/sqrt(ssh*ssw)
r

# and we can verify this:
cor(height,weight)


# PROBLEM 2

t2 <- r*sqrt((length(height)-2)/(1-r^2))
t2

# To get our critical value (one sided):
qt(0.9,3) # for alpha = .10
qt(0.95,3) # for alpha = .05
qt(0.99,3) # for alpha = .01

# and we reject for all values of alpha

# We can verify this:

cor.test(height,weight,alternative = "greater")


# PROBLEM 3

# (a) Getting b0 and b1 is pretty trivial if they did everything right in (1):
b1 <- sscp/ssh # remember height is our x variable
b1

b0 <- mw - b1*mh
b0

# We can verify these by doing our regression with R
w <- lm(weight ~ height)
summary(w)

# (b)
# Since we've done our regression, we can just add the line to our plot from (1(a)):
abline(w)

# PROBLEM 4

# (a) We have everything except our residuals... So here's the hard way to get the residuals:
yhat <- b0+b1*height
yhat
resd <- weight - yhat
resd

# We can verify this by doing (using the regression we did above):
w$resid

# Now we get SEb1:
SE1 <- sqrt((sum(w$resid^2))/(length(height)-2)/ssh)

# And so our t-star is:
t4 <- b1/SE1
t4

# And we can verify this by looking at the t-value for the regression output above.

# Notice that since this is a one sided test, they need to use the same t-values as
# for correlation:

# To get our critical value (one sided):
qt(0.9,3) # for alpha = .10
qt(0.95,3) # for alpha = .05
qt(0.99,3) # for alpha = .01

# (They reject for all values of alpha)

# Note: in our regression above, we need to divide the p-value by 2 to get the correct value

# (b) If they did it correctly, the two values for t-star should be identical.


# PROBLEM 5

# Set up the problem:
slength <- iris$Sepal.Length[1:50]
swidth <- iris$Sepal.Width[1:50]
slength
swidth

# (a)
cor.test(slength,swidth,alternative = "greater")
# Yes, the test should be one sided - petal length should increase with petal width

# (b)
cor.test(swidth,slength,alternative = "greater")
# Obviously, they should be identical (the equation's the same!)

# (c)
plot(slength,swidth,xlab = "sepal length",ylab = "sepal width")

# PROBLEM 6

# Old commands to set up the problem:
# dbh <- runif(20)*50+10
# round(dbh,0)
# 10 20 30 40 50
# dbh <- round(dbh,0)
# dbh

# scan in the data:
dbh <- scan(nlines = 1)
26 49 52 32 15 15 39 41 20 30 46 42 21 40 17 38 13 26 49 26

# old command sto generate reasonable y's
#height <- 5.8 + 0.14*(dbh)+rnorm(20,0,1.5)
# round(height,0)

# scan in more data:
height6 <- scan(nlines = 2)
10.2 13.5 16.3 10.8 8.9 10.0 14.2 11.3 9.7 11.4 13.5
13.6 11.6 11.2 7.7 13.1 7.9 10.4 15.6 9.0

# (a) Most of this is just to see if they know how to use R by now:
mean(height6);mean(dbh)
var(height6)*(length(height6)-1);var(dbh)*(length(dbh)-1)

# (b) I won't bother writing out H0 and H1 (but do note H1 should be one sided):
tree <- lm(height6 ~ dbh)
summary(tree)
1.591e-07/2
# The test is highly significant (small p-value)

# (They don't need critical values from the table since they'r using R)

# (c)
# The least squares line uses the Intercept estimate (b0) and the dbh estimate (b1)
# From the summary of the regression.  This is just to make sure they know how to get
# the regression estimates from the output.


# PROBLEM 7

# preliminary (they don't really need this):
plot(dbh,height6)
abline(tree)

# (a) Here's the residual plot
plot(dbh,tree$resid,ylab = "residual",xlab = "dbh")
abline(0,0)

# No real problems with the residual plot

# (b) Here's the q-q plot
qqnorm(tree$residuals)
qqline(tree$resid)

# No real problems with the q-q plot.

# (c) R^2 is simply writing it down from the regression output above.


