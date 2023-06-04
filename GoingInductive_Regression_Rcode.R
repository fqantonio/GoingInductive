#INDEX
# RESULTS - 729
  # REGRESSION ANALYSIS - 2546
# Regression discontinuity design (RDD) - 2668

# libraries 
library(dplyr) #work with data frames
#library(tidyverse) #include dplyr
library(ggplot2) # graphs
library(gridExtra) #arrange graphs in rows

#Use this step before any of the analysis bellow
#perhaps you need to set your set working directory: setwd: setwd("/Users/novo/Desktop/Doutoramento/GoingInductive")
setwd("/Users/novo/Desktop/Doutoramento/GoingInductivePaper/GoingInductive/DATA")
data<-read.csv("./DATA_4CID_2023.csv") #the date 2023 on this csv file means that the data is used for 2023 paper going indcutive
data<-data %>% mutate(CLASS=0.5*TEST+0.3*LAB+0.2*BEHAV)
colnames(data) <- c("ID","DATE" ,"SCHOOL" ,"GENDER" ,"TEST" ,"LAB" ,"BEHAV" ,"GRADE" ,"M4CID","CLASS")
head(data)
summary(data)
# ID              DATE          SCHOOL          GENDER         
# Min.   :   1.0   Min.   :2003   Min.   :0.0000   Length:1414       
# 1st Qu.: 354.2   1st Qu.:2006   1st Qu.:0.0000   Class :character  
# Median : 707.5   Median :2011   Median :1.0000   Mode  :character  
# Mean   : 707.5   Mean   :2010   Mean   :0.6634                     
# 3rd Qu.:1060.8   3rd Qu.:2014   3rd Qu.:1.0000                     
# Max.   :1414.0   Max.   :2017   Max.   :1.0000                     
# TEST             LAB             BEHAV           GRADE      
# Min.   :  0.00   Min.   :  0.00   Min.   : 27.0   Min.   :0.000  
# 1st Qu.: 49.00   1st Qu.: 47.00   1st Qu.: 66.0   1st Qu.:1.000  
# Median : 60.00   Median : 60.00   Median : 77.0   Median :1.000  
# Mean   : 60.72   Mean   : 59.92   Mean   : 76.1   Mean   :1.767  
# 3rd Qu.: 73.75   3rd Qu.: 73.00   3rd Qu.: 87.0   3rd Qu.:2.000  
# Max.   :100.00   Max.   :100.00   Max.   :100.0   Max.   :6.000  
# M4CID            CLASS       
# Min.   :0.0000   Min.   : 14.20  
# 1st Qu.:0.0000   1st Qu.: 53.70  
# Median :0.0000   Median : 63.00  
# Mean   :0.4144   Mean   : 63.55  
# 3rd Qu.:1.0000   3rd Qu.: 73.20  
# Max.   :1.0000   Max.   :100.00 
# sample = 1414
# since mean and medians are very close could be a sight that the data is normal
# Table 1: summary data

########## REGRESSION ANALYSIS

#Lm, GRADE regression, DUMMY variable
lmGRADE_DUMMY = lm(TEST~GRADE+M4CID, data = data) #Create the linear regression
summary(lmGRADE_DUMMY) #Review the results
plot(lmGRADE_DUMMY)


p72<-ggplot(data,aes(GRADE,TEST))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="lm")
p73<-ggplot(data,aes(GRADE,LAB))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="lm")
p74<-ggplot(data,aes(GRADE,BEHAV))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="lm")
p75<-ggplot(data,aes(GRADE,CLASS))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="lm")
grid.arrange(p72, p73, p74, p75,  nrow = 2, top="Scatterplots with regressions",bottom="Figure 16: GRADE Regression analysis by 4C/ID treatment effect")

#Loess, GRADE regression
ggplot(data,aes(GRADE,TEST))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="loess")
ggplot(data,aes(GRADE,LAB))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="loess")
ggplot(data,aes(GRADE,BEHAV))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="loess")
ggplot(data,aes(GRADE,CLASS))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="loess")

#dummy variable 4C/ID
?geom_smooth
#lm
ggplot(data_SCHOOL1,aes(CLASS,TEST))+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="lm")+geom_vline(xintercept=c(70,45),col="red")
ggplot(data_SCHOOL1,aes(CLASS,LAB))+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="lm")+geom_vline(xintercept=c(70,45),col="red")
ggplot(data_SCHOOL1,aes(CLASS,BEHAV))+geom_point()+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="lm")+geom_vline(xintercept=c(70,45),col="red")

#loess
ggplot(data_SCHOOL1,aes(CLASS,TEST))+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="loess")+geom_vline(xintercept=c(70,45),col="red")
ggplot(data_SCHOOL1,aes(CLASS,LAB))+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="loess")+geom_vline(xintercept=c(70,45),col="red")
ggplot(data_SCHOOL1,aes(CLASS,BEHAV))+geom_point()+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="loess")+geom_vline(xintercept=c(70,45),col="red")

#lm
ggplot(data_SCHOOL1,aes(GRADE,TEST))+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="lm")
ggplot(data_SCHOOL1,aes(GRADE,LAB))+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="lm")
ggplot(data_SCHOOL1,aes(GRADE,BEHAV))+geom_point()+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="lm")
ggplot(data_SCHOOL1,aes(GRADE,CLASS))+geom_point()+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="lm")

#loess
ggplot(data_SCHOOL1,aes(GRADE,TEST))+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="loess")
ggplot(data_SCHOOL1,aes(GRADE,LAB))+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="loess")
ggplot(data_SCHOOL1,aes(GRADE,BEHAV))+geom_point()+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="loess")
ggplot(data_SCHOOL1,aes(GRADE,CLASS))+geom_point()+geom_point(aes(color=factor(M4CID)))+geom_smooth(se=FALSE,aes(group=M4CID,linetype=factor(M4CID)),method="loess")

# Regression discontinuity design (RDD)
#"The regression discontinuity model was created to deal with one very specific type 
#of selection - cases where people have to qualify for the program, and the criteria 
#for qualification is a measure that is highly correlated with performance. 
#Some threshold is created and all people that score above the threshold are accepted 
#into the program, and all people below are not allowed to participate, or vice-versa."
# Regression discontinuity is used to solve this problem and assess whether or not we 
# observe a treatment effect. It does so by accounting for the relationship between 
#the outcome and the rating variable and then calculating the difference between the 
#ntwo groups.
#SOURCE: https://ds4ps.org/pe4ps-textbook/docs/p-060-reg-discontinuity.html

data_SCHOOL1<-filter(data,data$SCHOOL=="1")  %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_JUNIOR_SCHOOL1<-filter(data,data$SCHOOL=="1",GRADE <=2)  %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

# scatterplots with regressions over time bu with a 4C/ID factor
p64<-ggplot(data_SCHOOL1,aes(factor(DATE),TEST))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="lm")
p65<-ggplot(data_SCHOOL1,aes(factor(DATE),LAB))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="lm")
p66<-ggplot(data_SCHOOL1,aes(factor(DATE),BEHAV))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="lm")
p67<-ggplot(data_SCHOOL1,aes(factor(DATE),CLASS))+geom_point(aes(shape=factor(M4CID)))+geom_smooth(aes(group=factor(M4CID),color=factor(M4CID)),se=FALSE,method="lm")
grid.arrange(p64, p65, p66, p67,  nrow = 2, top="Scatterplots with regressions",bottom="Figure 16: Regression analysis by 4C/ID treatment effect")

#Figure 16: Regression analysis by 4C/ID treatment effect 
# looking to the plots (see bellow) its clear that there is no linear model that explinais the data
# RDD s just to verify discontinuities in 2013. At least for the BEAHV and CLASS variable there a discontinuity
# looking at all plots it seems thta there is a change around 2013

# scatterplots with regressions over time
p68<-ggplot(data_JUNIOR_SCHOOL1,aes(DATE,TEST))+geom_point()+geom_smooth(se=FALSE,method="loess")+geom_vline(xintercept=2013,col="red")
p69<-ggplot(data_JUNIOR_SCHOOL1,aes(DATE,LAB))+geom_point()+geom_smooth(se=FALSE,method="loess")+geom_vline(xintercept=2013,col="red")
p70<-ggplot(data_JUNIOR_SCHOOL1,aes(DATE,BEHAV))+geom_point()+geom_smooth(se=FALSE,method="loess")+geom_vline(xintercept=2013,col="red")
p71<-ggplot(data_JUNIOR_SCHOOL1,aes(DATE,CLASS))+geom_point()+geom_smooth(se=FALSE,method="loess")+geom_vline(xintercept=2013,col="red")
grid.arrange(p68, p69, p70, p71,  nrow = 2, top="Scatterplots with regressions",bottom="Figure 16: Regression analysis by 4C/ID treatment effect")



library(tidyverse)
data<-filter(data,GRADE >="0" & GRADE <="2" & SCHOOL=="1")
data0<-filter(data,GRADE >="0" & GRADE <="2" & SCHOOL=="1" & M4CID=="0")
data1<-filter(data,GRADE >="0" & GRADE <="2" & SCHOOL=="1" & M4CID=="1")

head(data)
summary(data)
ggplot(data, aes(ID,TEST)) + geom_point(col=factor(data$M4CID+1))
attach(data)
#NOTE: change TEST, LAB, BEHAV and CLASS
ggplot(data) + geom_boxplot(aes(x=DATE,y=TEST, color = factor(SCHOOL), linetype = factor(M4CID)))
#recategorizing using 00,01,10,11,20,21: meaning: first number of the binary means GRADE, second means with or withou 4CID
data$SUBSET <-
  ifelse(data$GRADE ==0 & data$M4CID==0, "00",
         ifelse(data$GRADE ==0 & data$M4CID==1, "01",
                ifelse(data$GRADE ==1 & data$M4CID==0, 10,ifelse(data$GRADE ==1 & data$M4CID==1, 11,ifelse(data$GRADE ==2 & data$M4CID==0, 20,ifelse(data$GRADE ==2 & data$M4CID==1, 21,NA))))))
head(data)
ggplot(data,aes(CLASS,factor(SUBSET)))+geom_boxplot(aes(colour = M4CID))
ggplot(data,aes(TEST,factor(SUBSET)))+geom_boxplot(aes(colour = M4CID))
ggplot(data,aes(LAB,factor(SUBSET)))+geom_boxplot(aes(colour = M4CID))
ggplot(data,aes(BEHAV,factor(SUBSET)))+geom_boxplot(aes(colour = M4CID))+
  xlab("Grade") +
  ylab("Student Classification: transfer test") +
  ggtitle("Effect of 4CID methodology")
mean(data$BEHAV[data$GRADE==1 & data$M4CID==0])
attach(data)
ggplot(data) + geom_point(aes(x=M4CID,y=TEST, color = factor(M4CID))) + stat_smooth(aes(x=M4CID,y=TEST), method = lm, formula = y ~ x)
library(graphics)
# Load Package "OLSRR"
install.packages("olsrr")
library("olsrr")
model <- lm(TEST ~ M4CID, data = data)
summary(model)
ks.test(residuals(model),dnorm(mean(residuals(model)),sd(residuals(model))))
shapiro.test(residuals(model))
mean(residuals(model))

plot(model)
ols_plot_resid_fit(model)
geom_line(model)
ols_plot_resid_qq(model)

########## Non-parametric Regression discontinuity design inference
summary(data)
?geom_smooth
data_SCHOOL1<-filter(data,data$SCHOOL=="1")  %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_JUNIOR_SCHOOL1<-filter(data,data$SCHOOL=="1",GRADE <=2)  %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

library(rdrobust)
x<-data_SCHOOL1$DATE
y<-data_SCHOOL1$TEST

plot(x,y)
rdrobust(y,x)
rdplot(y,x)
