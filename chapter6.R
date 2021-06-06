#These are all the codes of Chapter 6 Regression

###Some cruicial steps need to be performed before running Ch. 4 new codes -----------
#set your working directory to be the folder that stores your data files
setwd("C:/Users/Dropbox/learnR") #replace with your own folder directory
data<-read.csv(file="psychlearnr_data.csv", header=TRUE) 

###Recall Packages that you commonly use
library(dplyr) #you need to run these codes every time you re-start R
library(Hmisc)

###Chapter 6.1 Linear Regression
#6.1.1 Simple Linear Regression
reg<- lm(SACSECVAL~RESEMAVAL, data=data)
summary(reg)
plot(reg)

#obtaining standardized coefficients
install.packages("QuantPsyc")
library(QuantPsyc)
coef(reg)

#6.1.2 Multiple linear regression
reg2<-lm(SACSECVAL~RESEMAVAL+V240, data=data)
summary(reg2)
plot(reg2)
#6.1.3 Hierarcial linear regression
reg<- lm(SACSECVAL~RESEMAVAL, data=data)
reg2<-lm(SACSECVAL~RESEMAVAL+V240, data=data)
anova(reg,reg2)
#6.1.4 Dummy-coded variables
data$V238[data$V238=="-2"]<-NA
reg3<-lm(SACSECVAL~RESEMAVAL+V240+factor(data$V238), data=data)
summary(reg3)

#alternative "manual" method
data$lower[data$V238=="5"]<- 1
data$lower[data$V238!="5"]<- 0
data$working[data$V238=="4"]<- 1
data$working[data$V238!="4"]<- 0
data$lowmiddle[data$V238=="3"]<- 1
data$lowmiddle[data$V238!="3"]<- 0
data$upmiddle[data$V238=="2"]<- 1
data$upmiddle[data$V238!="2"]<- 0
reg3<-lm(SACSECVAL~RESEMAVAL+V240+lower+working+lowmiddle+upmiddle, data=data)
summary(reg3)

###Chapter 6.2 Logistic regression, Logit model and Generalized Linear Models
#6.2.1 Binomial
data$V37<-as.factor(data$V37)
blg<-glm(V37~SACSECVAL, family="binomial", data=data)
summary(blg)
confint(blg) 
exp(coef(blg))

#6.2.2 Poisson
data$V58[data$V58=="-1"]<-NA
poissonglm<-glm(V58~SACSECVAL+V240+factor(data$V238), family="poisson", data=data)
summary(poissonglm)
#overdispersion when residual deviance is larger than degrees of freedom. 

#6.2.3 negative binomial (for overdispersed poisson)
install.packages("MASS")
library(MASS)
negbin<-glm.nb(V58~SACSECVAL+V240+factor(data$V238), data=data)
summary(negbin)


###Chapter 6.3 Bootstrapping

#bootstapping for linear regression
install.packages("boot")
library(boot)
f <- function(formula, data, indices) {
  d <- data[indices,]  
  reg <- lm(SACSECVAL~RESEMAVAL+V240, data=d)
  return(coef(reg))
} 
bootresult <- boot(data=data, statistic=f, 
                   R=2000, formula=SACSECVAL~RESEMAVAL+V240)
plot(bootresult, index=1) #intercept 
plot(bootresult, index=2) #RESEMAVAL (emancipative values)
plot(bootresult, index=3) #V240 (sex)

boot.ci(bootresult, type="bca", index=1)
boot.ci(bootresult, type="bca", index=2)
boot.ci(bootresult, type="bca", index=3)

#bootstrapping for logit model and other GLMs
f2 <- function(data, indices) {
  d <- data[indices,]  
  negbin <- glm.nb(V58~SACSECVAL+V240, data=d)
  return(coef(negbin))
}
bootresult2 <- boot(data=data, statistic=f2, R=2000)
boot.ci(bootresult2, type="bca", index=1)
boot.ci(bootresult2, type="bca", index=2)
