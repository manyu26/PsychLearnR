#This is all the codes of Chapter 2 Data Management and Manipulation

#set your working directory to be the folder that stores your data files
setwd("C:/Users/Dropbox/learnR") #replace with your own folder directory
getwd() #check working directory
##Chapter 2.1 Reading data into R programming-------------------------------------------

#Option 1 (the option described in the book)
#Download data from https://github.com/manyu26/PsychLearnR/tree/Data-sets-used-in-the-book
#the data file is labeled as "psychlearnr_data.csv"
#let's give the dataset a name "data"
data<-read.csv(file="psychlearnr_data.csv", header=TRUE) 

#Option 2: Download data from http://www.worldvaluessurvey.org/WVSDocumentationWV6.jsp 
#Here we use the dataset from Algeria 2014
#Download spss data
install.packages("foreign") #the package that you need to install if you import from SPSS
library(foreign) #after you install it, you need to tell R to draw the package from the library
data<-read.spss("WV6_Data_Algeria_2014_Spss_v20180912.sav",use.value.labels = FALSE)

###Chapter 2.2 installing packages to ease your tasks----------------------------------------
install.packages("dplyr") #you only need to install it once per computer
install.packages("Hmisc")
library(dplyr) #you need to run these codes every time you re-start R
library(Hmisc)
##Chapter 2.3 Recoding variables---------------------------------------------------------

###Chapter 2.3.1 Basic R codes for recoding variables 
#recoding variable into the same variable 
data$V1[data$V1=="6"]<-"Algeria"   #recoding "6" in the V1 column to "Algeria"
data$V1[data$V1=="Algeria"]<-"6"   #recoding "Algeria" in the V1 column back to "6
data$V1[data$V1=="6"]<-"1"         #recoding "6" in the V1 column to "1"

#check your recode to make sure everything looks alright
data$V1 #view the data of variable V1
glimpse(data$V1) #view the data with a transpose view
describe(data$V1) #view the descriptive statistics of the variable

#to recode the variable into a new variable, simply change the variable name
data$survey_country[data$V1=="6"]<-"1"

###Chapter 2.3.2 Recode missing values (NA)
#V4 is the variable about how important family is to the participants
#-1 (don't know) and -2 (no answer) are the missing variables
data$V4[data$V4=="-1"|data$V4=="-2"]<-NA
#alternatively, you can use <0 as the condition to recode the value into NA
data$V4[data$V4<0]<-NA

###Chapter 2.3.3 Recode numeric variables into categorical variables
#V241 is the variable "year of birth"
#we can recode year of birth in to 4 groups. 
data$V241[data$V241<1950]<-1                       #Group 1
data$V241[data$V241>=1950 & data$V241<=1969]<-2    #Group 2
data$V241[data$V241>=1970 & data$V241<=1989]<-3    #Group 3
data$V241[data$V241>=1990]<-4                      #Group 4

#You may also recode the variable into a new variable birthyear
data$birthyear[data$V241<1950]<-1                       #Group 1
data$birthyear[data$V241>=1950 & data$V241<=1969]<-2    #Group 2
data$birthyear[data$V241>=1970 & data$V241<=1989]<-3    #Group 3
data$birthyear[data$V241>=1990]<-4                      #Group 4

###Chapter 2.3.4 Recode reversed values
#you could code the reversed values one-by-one
data$familyvalue[data$V4=="1"]<-4
data$familyvalue[data$V4=="2"]<-3
data$familyvalue[data$V4=="3"]<-2
data$familyvalue[data$V4=="4"]<-1
#alternatively, you can use only one line of code to do the same task
data$familyvalue<-5-data$V4
#regardless of which one your use, make sure you recode missing value first. 

###Chapter 2.4 Calculating Scale Scores-------------------------------------------------
data$conservation<- data$V72+data$V77+data$V79

#More effective way
#1. Group interested variables into a data frame. Any of the codes below serve the same purpose
conservation <- data.frame(data$V72,data$V77,data$V79)
conservation <- select(data, V72,V77,V79)
#2. 
conservation[sapply(conservation, function(x) x %in% "-2")] <- NA
#3.
data$conservation_sum<-rowSums(conservation)
data$conservation_mean<-rowMeans(conservation)

#4. Calculate internal consistency (reliability/Cronbach's alpha)
install.packages("psych")  #install the package first if you have never done so before
library(psych) 
psych::alpha(conservation)


###Chapter 2.5 Types of Variables----------------------------------------------------
class(data$V240)
is.factor(data$V240) #ask R whether the variable is categorical
is.numeric(data$V240) #ask R whether the variable is numeric
is.integer(data$V240) #ask R whether the variable is integer

data$V240<-as.factor(data$V240) #Change V240 into categorical
data$V240<-as.numeric(data$V240) #Change V240 into numeric
data$V240<-as.integer(data$V240) #Change V240 into integer

###Chapter 2.6 Saving data---------------------------------------------------------
write.csv(data, "psychlearnr_data_ch2.csv")