#These are all the codes of Chapter 3 Desciptive statistics

###Some cruicial steps need to be performed before running Ch. 3 new codes -----------
#set your working directory to be the folder that stores your data files
setwd("C:/Users/Dropbox/learnR") #replace with your own folder directory
data<-read.csv(file="psychlearnr_data.csv", header=TRUE) 

###Recall Packages that you commonly use
library(dplyr) #you need to run these codes every time you re-start R
library(Hmisc)

###Chapter 3.1 Basic Descriptive Statistics-----------------------------------------------
###Chapter Frenquencies 3.1.1--------
#gender distribution
describe(data$V240)
table(data$V240)
#pie charts
x<-c(608, 592)
labels<-c("male", "female")
pie(x,labels)
#income distribution
describe(data$V239)
#histograms
hist(data$V239) #basic histogram
hist(data$V239, breaks=10, col="gray", 
     xlab="Income scale", ylab = "no. of participants",
     main = "Distribution of Income Scale")
#histogram with normal curve
income<-data$V239
hist_normal<-hist(data$V239, breaks=10, col="gray", 
                  xlab="Income scale", ylab = "no. of participants",
                  main = "Distribution of Income Scale")
xfit<-seq(min(income),max(income),length=40) 
yfit<-dnorm(xfit,mean=mean(income),sd=sd(income)) 
yfit <- yfit*diff(hist_normal$mids[1:2])*length(income) 
lines(xfit, yfit, col="red", lwd=2)
#density plot
d <- density(data$V239)
plot(d)

###Chapter 3.1.2 Central tendency---------
mean(data$V239)
sd(data$V239)
median(data$V239)

CT_mode<-function(x){
  uni <- unique(x)
  uni[which.max(tabulate(match(x,uni)))]
}

CT_mode(data$V239)

table(data$V239)

#box plot
boxplot(data$V239, main="Income Scale", 
        xlab="Algerian participants", ylab="Income Scale")

###Chapter 3.1.2 Central tendency by groups-------
table(data$V239,data$V240)
ggplot(data, aes(x=V239, fill=V240)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")
ggplot(data, aes(x=V239, fill=V240)) +
  geom_histogram(binwidth=.5, alpha=.5, position="dodge") 

library(ggplot2)
aggregate(data$V239, by=list(data$V240), FUN=mean, na.rm=TRUE)
boxplot(data$V239~data$V240,data=data, main="Scale of Income by Sex", 
        xlab="Biological Sex", ylab="Scale of Income")
install.packages("ggpubr")
ggqqplot(data$V239)

###Chapter 3.2 Normal Distribution-----------------------------------
##Chapter 3.2.1 Visualizing Distribution------------
#Distribution plots
install.packages("ggpubr")
library(ggpubr)
ggdensity(data$V239, 
          main = "Distribution of Scale of Income", 
          xlab = "Scale of Income (V239)",
          add = "mean") #for a line to indicate group mean
#Distribution plots by groups
data$V240<-as.factor(data$V240)
ggdensity(data, x="V239", 
          main = "Distribution of Scale of Income by Sex", 
          xlab = "Scale of Income",
          ylab = "Density",
          facet.by = "V240",
          panel.labs = list(V240 = c("Male", "Female")),
          add = "mean") #for a line to indicate group mean

#QQ plots 
library(car)
ggqqplot(data$V239)

#scatterplots
plot(data$SACSECVAL)
plot(data$RESEMAVAL)
plot(data$SACSECVAL~data$RESEMAVAL, main="Scatterplot of Secular and Emancipative values", 
  	xlab="Overall Secular Values ", ylab="Overall Emanipative Values ")
#scatterplots by groups
library(car)
scatterplot(SACSECVAL~RESEMAVAL | V240, data=data, main="Scatterplot of Secular and Emancipative values", 
     xlab="Overall Secular Values ", ylab="Overall Emanipative Values ")

##Chapter 3.2.2 Test of Normality----------
shapiro.test(data$V239)

##Chapter3.3 Test of outliers---------------
library(psych)
boxplot(data$V239)
boxplot(data$SACSECVAL)
boxplot.stats(data$SACSECVAL) 

#further study http://r-statistics.co/Outlier-Treatment-With-R.html
#https://cran.r-project.org/web/packages/outliers/outliers.pdf 
