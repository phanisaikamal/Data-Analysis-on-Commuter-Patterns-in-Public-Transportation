knitr::opts_chunk$set(echo = TRUE)
# intalling packages
install.packages("ggplot2")
install.packages("dplyr")
# importing libraries
library("ggplot2")
library("dplyr")
# importing data from CSV to a Dataframe
myData <- read.csv(file = "dataset.csv")
head(myData)
# Entire collected data
myData
# Dataset Summary
summary(myData)
# Choosing a random seed number to introduce randomization in sampling process.
set.seed(7384)
# Sample data of 40 passengers using Cash for ticket payment (40 out of 42 data points).
cashData <- filter(myData, myData$ModeOfPayment == 'C')
cashDataSample <- cashData[sample(nrow(cashData), 40), ]
cashDataSample
# Cash Data Statistics
summary(cashDataSample)
# Choosing a random seed number to introduce randomization in sampling process. 
set.seed(4250)
# Sample data of 40 passengers using Pass for ticket payment (40 out of 108 data points).
passData <- filter(myData, myData$ModeOfPayment == 'P')
passDataSample <- passData[sample(nrow(passData), 40), ]
passDataSample
# Pass Data Statistics
summary(passDataSample)
# Sampled Data chosen for Statistical Analysis.
sampleData <- rbind(cashDataSample, passDataSample)
head(sampleData)
# Sampled Data
sampleData
# Sample Data Summary
summary(sampleData)
# graphs of all pairs of variables
pairs(sampleData)
ggplot(sampleData,aes(x=BoardingDuration,y=Gender,color=ModeOfPayment))+geom_point()+ylab('Gender')+xlab('Boarding Duration')+ ggtitle('Mode of Payment')
# Box plot for Mode of Payment vs Boarding Duration
ggplot(sampleData, aes(x = ModeOfPayment, y = BoardingDuration)) + geom_boxplot() + coord_flip()

# histogram with fill color
ggplot(sampleData, aes(x=BoardingDuration))+
  geom_histogram(color="darkblue", fill="lightblue")
# dashed line
ggplot(sampleData, aes(x=BoardingDuration))+
  geom_histogram(color="black", fill="lightblue",
                 linetype="dashed")
# histogram plot by groups
ggplot(sampleData, aes(x=BoardingDuration, color=ModeOfPayment)) +
  geom_histogram(fill="White")
# Overlaid histograms
ggplot(sampleData, aes(x=BoardingDuration, color=ModeOfPayment)) +
  geom_histogram(fill="White", alpha=0.5, position="identity")
# Boarding Duration vs Mode of Payment
ggplot(sampleData, aes(x=BoardingDuration))+
  geom_histogram(color="black", fill="white")+
  facet_grid(ModeOfPayment ~ .)
# Boarding Duration vs Age Groups
ggplot(sampleData, aes(x=BoardingDuration))+
  geom_histogram(color="black", fill="white")+
  facet_grid(AgeGroup ~ .)
# Boarding Duration vs Gender
ggplot(sampleData, aes(x=BoardingDuration))+
  geom_histogram(color="black", fill="white")+
  facet_grid(Gender ~ .)
# Boarding Duration vs GenderEmployment
ggplot(sampleData, aes(x=BoardingDuration))+
  geom_histogram(color="black", fill="white")+
  facet_grid(Employment ~ .)
# Boarding Duration vs Ethnicity
ggplot(sampleData, aes(x=BoardingDuration))+
  geom_histogram(color="black", fill="white")+
  facet_grid(Ethnicity ~ .)
# Mode of Payment vs Gender
ggplot(sampleData, aes(ModeOfPayment, ..count..)) + geom_bar(aes(fill = Gender), position = "dodge")
# Mode of Payment vs Employment
ggplot(sampleData, aes(ModeOfPayment, ..count..)) + geom_bar(aes(fill = Employment), position = "dodge")
# Mode of Payment vs Ethnicity
ggplot(sampleData, aes(ModeOfPayment, ..count..)) + geom_bar(aes(fill = Ethnicity), position = "dodge")
# Mode of Payment vs Age Group
ggplot(sampleData, aes(ModeOfPayment, ..count..)) + geom_bar(aes(fill = AgeGroup), position = "dodge")
# Q-Q Plot of Sample Data
qqnorm(sampleData$BoardingDuration)
qqline(sampleData$BoardingDuration)
# Q-Q Plot of Passengers using Cash in the Sample Data
qqnorm(sampleData$BoardingDuration[sampleData$ModeOfPayment == "C"])
qqline(sampleData$BoardingDuration[sampleData$ModeOfPayment == "C"])
# Q-Q Plot of Passengers using Pass in the Sample Data
qqnorm(sampleData$BoardingDuration[sampleData$ModeOfPayment == "P"])
qqline(sampleData$BoardingDuration[sampleData$ModeOfPayment == "P"])
# Two sided t-test
t.test(sampleData$BoardingDuration~sampleData$ModeOfPayment)
# Mean Boarding Time of Passengers using Cash
mu_c <- mean(sampleData$BoardingDuration[sampleData$ModeOfPayment == 'C'])
mu_c
# Mean Boarding Time of Passengers using Pass
mu_p <- mean(sampleData$BoardingDuration[sampleData$ModeOfPayment == 'P'])
mu_p
# Null Hypothesis
mu_0 <- 0
# Variance of Boarding Time of Passengers using Cash
var_c <- var(sampleData$BoardingDuration[sampleData$ModeOfPayment == 'C'])
var_c
# Variance of Boarding Time of Passengers using Pass
var_p <- var(sampleData$BoardingDuration[sampleData$ModeOfPayment == 'P'])
var_p
# Sample Size of Passengers using Cash
n_c <- length(sampleData$BoardingDuration[sampleData$ModeOfPayment == 'C'])
# Sample Size of Passengers using Pass
n_p <- length(sampleData$BoardingDuration[sampleData$ModeOfPayment == 'P'])
# t-value (test statistic)
t <- (mu_c - mu_p - mu_0)/sqrt(var_c/n_c + var_p/n_p)
t
# p-value for 2 sided t-test
p_value <- pt(q = t, df = min(n_c, n_p) - 1, lower.tail = FALSE)*2
p_value
# Lower Boundary of Confidence Interval
lowerBound <- mu_c - mu_p + qt(0.05, min(n_c, n_p) - 1)*sqrt(var_c/n_c + var_p/n_p)
lowerBound
# Upper Boundary of Confidence Interval
upperBound <- mu_c - mu_p + qt(0.95, min(n_c, n_p) - 1)*sqrt(var_c/n_c + var_p/n_p)
upperBound
# Sample Statistic
ss <- mu_c - mu_p
ss
# Upper t-test
t.test(sampleData$BoardingDuration[sampleData$ModeOfPayment == 'C'], sampleData$BoardingDuration[sampleData$ModeOfPayment == 'P'], alternative = "greater")
# Scatterplot of Data points representing Random Sampling and Randomness in data.
plot(sampleData$BoardingDuration)
# Histogram of Sampling Distribution
mu <- mean(sampleData$BoardingDuration)
sd <- sd(sampleData$BoardingDuration)
h <- hist(sampleData$BoardingDuration, xlim = c(0,12))
lb <- mu - 1.96*sd
ub <- mu + 1.96*sd
abline(v = c(mu, lb, ub), lty = 2)
xx <- seq(min(sampleData$BoardingDuration),max(sampleData$BoardingDuration),length=80)
yy <- dnorm(xx, mu, sd)*length(xx)
lines(xx, yy, col = "blue")
# Boarding Time Intervals for Both Categories
plot(sampleData$BoardingDuration~sampleData$ModeOfPayment)
# test statistic graph
n <- min(n_c, n_p)
tempX <- seq(-4, 4, .01)
tempY <- dt(tempX, n-1)
plot(tempX, tempY, type = 'l')
abline(v = c(t, -t),  col = "yellow")
abline(v = 0, col = "black")
# Confidence Intervals graph
plot(tempX, tempY, type = 'l')
abline(v = qnorm(0.975), col = "red")
abline(v = qnorm(0.025), col = "red")
abline(v = 0, col = "black")
plot(tempX, tempY, type = 'l')
abline(v = qnorm(0.975), col = "red")
abline(v = qnorm(0.025), col = "red")
abline(v = 0, col = "black")
abline(v = c(t, -t),  col = "yellow")
# confidence interval shading
# plot(tempX, tempY, type = 'l')
# polygon(c(tempX[tempX>=-1.96&tempX<=1.96]),c(tempY[tempX>=-1.96&tempX<=1.96]), col="red")
# T-distribution vs Normal distribution
plot(tempX, tempY, type = 'l')
lines(tempX, dnorm(tempX), col = "red")
sampleData