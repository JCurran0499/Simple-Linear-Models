
# John Curran

# First, we will create linear models of the correlation between vote swings 
# from 2012 to 2016 and 1992 to 1996, and college attainment rates in each 
# New England county at the time of the given elections. 

library(readxl)
statedata <- read_excel("~/NESwings.xlsx", range='A1:E68')

model2016 <- lm(SWING2016~I(COLLEGE2018*100), data=statedata)
model1996 <- lm(SWING1996~I(COLLEGE1990*100), data=statedata)


# Next, we will plot and compare the results.
par(mfrow=c(1,2))

plot(statedata$COLLEGE2018*100, statedata$SWING2016, main="2012 > 2016", 
     xlab="College Attainment % in 2018", ylab="Vote Swing")
abline(model2016, col="red")

plot(statedata$COLLEGE1990*100, statedata$SWING1996, main="1992 > 1996", 
     xlab="College Attainment % in 1990", ylab="Vote Swing")
abline(model1996, col="red")


# From a preliminary analysis, it appears that there is a far stronger 
# correlation in the more recent election swing. We will now look at the 
# summary statistics and diagnostic plots for the 2016 model.

summary(model2016)

par(mfrow=c(2,2))
plot(model2016, 1)
plot(model2016, 2)
hist(model2016$residuals, main="Histogram of Residuals", xlab="Residuals")
plot(model2016$residuals, main="Distribution of Residuals", ylab="Residuals")


# We will be testing whether the test statistic is the critical region to the 
# left of t(0.025,65) and right of t(0.975,65) in order to reject the null hypothesis.

qt(0.975, 65)


# After concluding that a correlation exists, we will now develop a 95% confidence
# interval for what that correlation is.

confint(model2016)


# We will now move on to the 1992 > 1996 model in order to compare its findings
# with the 2012 > 2016 model. First, we will retrieve summary statistics and 
# diagnostic plots for this model.

summary(model1996)

par(mfrow=c(2,2))
plot(model1996, 1)
plot(model1996, 2)
hist(model1996$residuals, main="Histogram of Residuals", xlab="Residuals")
plot(model1996$residuals, main="Distribution of Residuals", ylab="Residuals")


# We will be testing whether the test statistic is the critical region to 
# the left of t(0.025,65) and right of t(0.975,65) in order to reject 
# the null hypothesis.

qt(0.025, 65)

