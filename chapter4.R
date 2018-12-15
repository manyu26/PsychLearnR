#These are all the codes of Chapter 4 Chi-square tests and correlations

###Some cruicial steps need to be performed before running Ch. 4 new codes -----------
#set your working directory to be the folder that stores your data files
setwd("C:/Users/Dropbox/learnR") #replace with your own folder directory
data<-read.csv(file="psychlearnr_data.csv", header=TRUE) 

###Recall Packages that you commonly use
library(dplyr) #you need to run these codes every time you re-start R
library(Hmisc)
###Chapter 4.1 Chi-square tests of independence-------------------------------------------
#4.1.1 Simple Example 1. Marital Status x Sex
table<-table(data$V57, data$V240)
chisq<-chisq.test(table)
#4.1.2 Example 2. Marital Status x Employment Status (with small cells)
data$V229<-as.numeric(data$V229)
table2<-table(data$V57, data$V229)
chisq<-chisq.test(table2)
#warning message is due to small cell sizes
#Deleting/combining small cells
data$V229[data$V229=="8"]<-NA
table2<-table(data$V57, data$V229)
cbind(1,3:6)
table3<-cbind(table2[1,],table2[2,]+table2[3,]+table2[4,]+table2[5,])
chisq<-chisq.test(table3)
#4.1.3 post-hoc tests
#post-hoc chi-square test - pairwise comparison
install.packages("rcompanion")
library(rcompanion)
pairwiseNominalIndependence(table3,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "fdr")

#4.1.4 alternative post-hoc graphic method
install.packages("corrplot")
library(corrplot)
chisq<-chisq.test(table3)
corrplot(chisq$residuals, is.cor=FALSE)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)
corrplot(contrib, is.cor = FALSE)

###Chapter 4.2 Correlations--------------------------------------------------------------
#4.2.1 Pearson
#to learn how to do pearson correlations in R, let's create two continuous variables
openness <- select(data, V70,V73,V76)
openness[sapply(openness, function(x) x %in% "-2")] <- NA
psych::alpha(openness) #alpha is bad but let's ignore it for now
data$openness_m<-rowMeans(openness, na.rm=TRUE, dims=1)
conservation <- select(data, V72,V77,V79)
conservation[sapply(conservation, function(x) x %in% "-2")] <- NA
psych::alpha(conservation) #alpha is bad but let's ignore it for now
data$conservation_m<-rowMeans(conservation, na.rm=TRUE, dims=1)

#base R method
cor.table<-select(data,openness_m,conservation_m)
cor(cor.table, use="pairwise.complete.obs", method="pearson") 
cor(cor.table, use="complete.obs", method="pearson")
cor.test(data$openness_m, data$conservation_m, method="pearson") #for p value

#4.2.2 Spearman and Kendall
cor.table_sk<-select(data,openness_m,V238)
cor(cor.table_sk, use="pairwise.complete.obs", method="spearman") 
cor.test(data$openness_m, data$V238, method="spearman") #for p value
cor(cor.table_sk, use="pairwise.complete.obs", method="kendall") 
cor.test(data$openness_m, data$V238, method="kendall") #for p value


#4.2.3 Correlation Matrix: Using rcorr to compute correlations
cor.table<-select(data,openness_m,conservation_m)
rcorr(as.matrix(cor.table), type="pearson")

#more than two variables
cor.table2<-select(data, V70,V73,V76)
rcorr(as.matrix(cor.table2), type="spearman")

#4.2.4 partial correlation
install.packages("ppcor")
library(ppcor)
pcor.test(data$V70,data$V73,data$V76, method="spearman")
pcor.test(data$V70,data$V73, c(data$V76,data$V75), method="spearman")
