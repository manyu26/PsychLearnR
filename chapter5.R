#These are all the codes of Chapter 5 T-tests and ANOVAs

###Some cruicial steps need to be performed before running Ch. 4 new codes -----------
#set your working directory to be the folder that stores your data files
setwd("C:/Users/Dropbox/learnR") #replace with your own folder directory
data<-read.csv(file="psychlearnr_data.csv", header=TRUE) 

###Recall Packages that you commonly use
library(dplyr) #you need to run these codes every time you re-start R
library(Hmisc)

###Chapter 5.2 Test of homoscedasticity-------------------------------------
#test of equal variance
var.test(data$SACSECVAL~data$V240) #F-test
bartlett.test(data$SACSECVAL~data$V240, data) #bartlett test
#Levine test
library(car)
data$V240<-as.factor(data$V240)
leveneTest(data$SACSECVAL~data$V240, data)

###Chapter 5.3 t-tests---------------------------------------------------
#5.3.1 independent t-test
t.test(SACSECVAL~V240, data = data, var.equal=TRUE)

#default is two-sided test, if one-sided test or if changing confidence level
t.test(SACSECVAL~V240, data = data, alternative="less")
t.test(SACSECVAL~V240, data = data, alternative="greater", conf.level=.99)

#5.3.2 dependent t-test (paired t-test)
#note: we do not have a paired sample t-test example from the world value survey
#as always, put x first (dependent variable), then y (grouping variable)
#below is just an example of how the codes are written.
t.test(data$SACSECVAL,data$RESEMAVAL, paired=TRUE) #when pre and post scores are entered directly


###Chapter 5.4 ANOVA-----------------------------------------
#5.4.1 one-way ANOVA
anova <- aov(SACSECVAL~V240, data = data) #or
anova <- aov(data$SACSECVAL~data$V240) #same as above
summary(anova)
plot(anova) #diagnostic plots

#5.4.2 One-way ANOVA (with post-hoc test)
data$V238[data$V238=="-2"]<-NA
describe(data$V238)
data$V238<-as.factor(data$V238)
anova2 <- aov(data$SACSECVAL~data$V238)
summary(anova2)
plot(anova2)
#post-hoc - obtaining p-value
pairwise.t.test(data$SACSECVAL, data$V238, p.adj = "none")
pairwise.t.test(data$SACSECVAL, data$V238, p.adj = "bonf")
pairwise.t.test(data$SACSECVAL, data$V238, p.adj = "holm")
TukeyHSD(anova2)
#post-hoc - obtaining group means
aggregate(data$SACSECVAL, by=list(data$V238), FUN=mean, na.rm=TRUE)

#post-hoc emmeans
install.packages("emmeans")
library(emmeans)
emmeans(anova2, ~V238)

#5.4.3 ANCOVA
ancova<-aov(SACSECVAL~V238+RESEMAVAL, data=data)
summary(ancova)
plot(ancova)


#5.4.4 two-way ANOVA
twanova <-aov(SACSECVAL~V240+V238+V240:V238, data=data)
summary(twanova)
plot(twanova)

##two-way anova post-hoc Tukey
TukeyHSD(twanova, "V238")

##two-way ANOVA post-hoc bonf/holm
male<-subset(data, V240=="1")
female<-subset(data,V240=="2")
pairwise.t.test(male$SACSECVAL, male$V238, p.adj = "bonf")
pairwise.t.test(female$SACSECVAL, female$V238, p.adj = "bonf")
aggregate(male$SACSECVAL, by=list(data$V238), FUN=mean, na.rm=TRUE)
aggregate(female$SACSECVAL, by=list(data$V238), FUN=mean, na.rm=TRUE)

#5.4.5 Repeated ANOVA
#assuming V240 is the grouping variable of a repeated measure group (e.g. participant ID)
anova_within<-aov(SACSECVAL~V238+Error(V240/V238),data)
summary(anova_within)

#Chapter 5.5 non-parametric

wilcox.test(SACSECVAL~V240,data=data)
