#INDEX
# GOALS - 36
# Exploratory data analysis (EDA) - 47
  # DATA summary - 58
  # Resume - 160
  # DATA variables description - 187
  # DATA structure - 202
# NORMALITY - 413
# INFERENCE - 590
  ## Global - 591
  ## Differences between junior school 0 and 1, without 4CID - 691
  ## school effect - 774
  ## School 1, global sample (include junior high for school 1) - 773 
  ## Computed sample junior with and without 4C/ID, between school 0 and school 1 (with 4C/ID)-865
  ## Inference for computed samples
      ### junior differences with and without 4C/ID, between school 0 and school 1 (only with 4C/ID) - 953
      ### global sample differences with and without 4CID - 1044
  ## Inference junior and junior high, school 1 - 1137
  ## Junior sample for school 1 only - 1262
  ## Inference SCHOOL 1, Grade 7  - 1383
  ## Inference SCHOOL 1, Grade 8 - 1501
  ## Inference SCHOOL 1, Grade 9 -1618
  ## RANK - 1753
  ## GENDER - 2129
# REGRESSION ANALYSIS -2317
# Regression discontinuity design (RDD) - 2355
# METHODOLOGY - 2451
  ## Hypothesis test discussion
  ## Independent samples
# SUMMARY results and observations/notes - 2458
  ## NOTES and OBSERVATIONS
  ## Policy implications
  ## Future  arch
  ## Tie loose hands together
# DOUBTS AND DISCUSSIONS - 2610
# REFERENCIES - 2616

# libraries 
library(dplyr) #work with data frames
#library(tidyverse) #include dplyr
library(ggplot2) # graphs
library(gridExtra) #arrange graphs in rows

##########. GOALS
# The main goal is to understand the impact of the use of Inductive Methodology 4C/ID by answering the questions: 
#   1 - changing the learning methodologies from one mainly deductive (Direct Instruction[^1]) to another one mostly inductive (4C/ID [^2]), means what for your students? Who benefits? What are the pros and cons about this methodological decision?
#   2 - Should you, as a teacher of science and tech, move to implement the inductive strategy as your methodology for learning? 
#   3 - And if you do, what are the outcomes? What are the risks? What were the consequences for the student's academic results? 
#   4 - Is there a different impact between female and male? 
#   5 - And within the different grades?
#   6 - Student more adapted to school have benefit more with the use of 4CID methodology?

########## HYPOTHESIS
# In overall, 4C/ID improves the learning process because it anchors on BBL, CLT, MI and IM

######### DATA - exploratory data analysis - EDA

#AT this point I will do an exploratory data analysis (EDA)
#Use this step before any of the analysis bellow
#perhaps you need to set your set working directory: setwd: setwd("/Users/novo/Desktop/Doutoramento/GoingInductive")
setwd("/Users/novo/Desktop/Doutoramento/GoingInductivePaper/GoingInductive/DATA")
data<-read.csv("./DATA_4CID_2023.csv")
data<-data %>% mutate(CLASS=0.5*TEST+0.3*LAB+0.2*BEHAV)
colnames(data) <- c("ID","DATE" ,"SCHOOL" ,"GENDER" ,"TEST" ,"LAB" ,"BEHAV" ,"GRADE" ,"M4CID","CLASS")
head(data)
summary(data)
# ID            DATE          SCHOOL          GENDER         
# Min.   :   1   Min.   :2003   Min.   :0.0000   Length:1625       
# 1st Qu.: 407   1st Qu.:2006   1st Qu.:0.0000   Class :character  
# Median : 813   Median :2011   Median :1.0000   Mode  :character  
# Mean   : 813   Mean   :2011   Mean   :0.7071                     
# 3rd Qu.:1219   3rd Qu.:2016   3rd Qu.:1.0000                     
# Max.   :1625   Max.   :2019   Max.   :1.0000                     
# TEST             LAB            BEHAV            GRADE      
# Min.   :  0.00   Min.   :  0.0   Min.   :  0.00   Min.   :0.000  
# 1st Qu.: 48.00   1st Qu.: 46.0   1st Qu.: 65.00   1st Qu.:0.000  
# Median : 60.00   Median : 60.0   Median : 76.00   Median :1.000  
# Mean   : 60.07   Mean   : 59.1   Mean   : 74.95   Mean   :2.036  
# 3rd Qu.: 74.00   3rd Qu.: 73.0   3rd Qu.: 87.00   3rd Qu.:2.000  
# Max.   :100.00   Max.   :100.0   Max.   :100.00   Max.   :6.000  
# M4CID            CLASS       
# Min.   :0.0000   Min.   :  0.00  
# 1st Qu.:0.0000   1st Qu.: 53.00  
# Median :0.0000   Median : 62.50  
# Mean   :0.4271   Mean   : 62.75  
# 3rd Qu.:1.0000   3rd Qu.: 72.90  
# Max.   :1.0000   Max.   :100.00  
# sample = 1625
# Table 1: summary data
# sample differences between school 0 and school 1
summary(data)
# Median and average very close, seems to have some normality structure, except for the BEHAV variable
length(data$ID)
length(which(data$SCHOOL=="0"))
length(which(data$SCHOOL=="0"))/length(data$ID)
length(which(data$SCHOOL=="1"))
length(which(data$SCHOOL=="1"))/length(data$ID)
# school 0, 476, 29% and school 1, 1149, 71%

# Global sample Gender
length(which(data$GENDER=="F"))
length(which(data$GENDER=="F"))/length(data$ID)
length(which(data$GENDER=="M"))
length(which(data$GENDER=="M"))/length(data$ID)
# female/male sample: 641/984, 39%/61%
# no balanced GENDER

#with and without 4CID
length(data$ID)
length(which(data$M4CID=="0"))
length(which(data$M4CID=="0"))/length(data$ID)
length(which(data$M4CID=="1"))
length(which(data$M4CID=="1"))/length(data$ID)
# sample without 4CID: 931, 57%; with 4CID:694, 43%
# more data with 4CID methodology, unbalanced but not critical
length(which(data$GRADE==0))
length(which(data$GRADE==0))/length(data$ID)
length(which(data$GRADE==1))
length(which(data$GRADE==1))/length(data$ID)
length(which(data$GRADE==2))
length(which(data$GRADE==2))/length(data$ID)
length(which(data$GRADE==3))
length(which(data$GRADE==3))/length(data$ID)
length(which(data$GRADE==4))
length(which(data$GRADE==4))/length(data$ID)
length(which(data$GRADE==5))
length(which(data$GRADE==5))/length(data$ID)
length(which(data$GRADE==6))
length(which(data$GRADE==6))/length(data$ID)
# very low sample entries levels 10/11 grade general edu path
# junior school, 7 to 9th grade
data_JUNIOR <- filter(data,GRADE <="2") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_JUNIOR$ID)
summary(data_JUNIOR)
length(which(data_JUNIOR$SCHOOL=="0"))
length(which(data_JUNIOR$SCHOOL=="0"))/length(data_JUNIOR$ID)
length(which(data_JUNIOR$SCHOOL=="1"))
length(which(data_JUNIOR$SCHOOL=="1"))/length(data_JUNIOR$ID)
# junior school, 4CID, 7 to 9 grade
data_JUNIOR_SCHOOL1 <- filter(data_JUNIOR,SCHOOL =="1") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_JUNIOR_SCHOOL1$ID)
length(which(data_JUNIOR_SCHOOL1$M4CID=="1"))
length(which(data_JUNIOR_SCHOOL1$M4CID=="1"))/length(data_JUNIOR$ID)
length(which(data_JUNIOR_SCHOOL1$M4CID=="0"))
length(which(data_JUNIOR_SCHOOL1$M4CID=="0"))/length(data_JUNIOR$ID)
#Gender
length(data_JUNIOR$ID)
length(which(data_JUNIOR$GENDER=="F"))
length(which(data_JUNIOR$GENDER=="F"))/length(data_JUNIOR$ID)
length(which(data_JUNIOR$GENDER=="M"))
length(which(data_JUNIOR$GENDER=="M"))/length(data_JUNIOR$ID)

#data length high junior, secondary (sec) grades ()
lsec<-length(which(data$GRADE==3))+length(which(data$GRADE==4))+length(which(data$GRADE==5))+length(which(data$GRADE==6))
lsec
lsec/length(data$ID)
# sample entries secondary 390, 24% of the global sample
#it seems good to use secondary grade data compared with the other parts
data_sec <- filter(data,GRADE >="3" & GRADE <="6") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
summary(data_sec)
length(data_sec$ID)
length(which(data_sec$M4CID==0))
length(which(data_sec$M4CID==0))/length(data_sec$ID)
length(which(data_sec$M4CID==1))
length(which(data_sec$M4CID==1))/length(data_sec$ID)
# Higher Junior (secondary), no 4CID, 61, 16%;
# Higher Junior (secondary), 4CID, 329, 84%

########## Resume
# The student data came from two different school: until 2008 school 0, after that, school 1, until 2019;
# School 0 has junior grades, 7 to 9th grade
# School 1 has junior grades and junior high with general courses and professional/technical coourses
# Male is represented by M and female by F
# with 4CID, varable M4CID = 1, otherwise M4CID = 0;
# Grades 7,8,9, 10, 11, 10P (technical), 11P (Tecnical) are categorizes by 0 to 6, respectively;
# Total sample data lenght 1625;
# female/male sample: 641/984, 39%/61%: no balanced GENDER; JUNIOR balanced: 49% Female, 51% male;
# Junior - 1235
# SCHOOL 0 Junior - 476 (29% for the global sample, 39% for the junior all sample)
# SCHOOL 0, 476, 29%
# SCHOOL 1 - 1149, 71%
# SCHOOL 1 Junior - 759 (61% for the junior all sample) 
# Junior High represents 390 entries, 24% all data sample
# Global Sample without 4CID: 931, 57%; 
# Sample with 4CID:694, 43%: more data with 4CID methodology: unbalanced but not critical
# SCHOOL 0 Junior , 7 to the 9th grade: - with 4CID 0 (0%); without 4CID 476 (39%) ???
# SCHOOL 1 Junior , 7 to the 9th grade: - with 4CID 365 (30%); without 4CID 394 (32%) ???
# SCHOOL 1 Junior High, 10 to the 11th grade: - with 4CID 329 (84%); without 4CID 61 (16%) 
# Table 2: Resume

# Conclusion: From this analysis it seems to be possible comparing not only all the sample but also,
# just the Junior or just the secondary, but in the secondary there is only 61 entrances without 4CID;
# as well as by each grade on the junior level
# conclusion: Junior sample entries and gender percentagem well balanced

#variables description
#ID: Identification entry row data;
#DATE: year of the sample: it belongs, actually to 2003/2004, lective year;
#SCHOOL: there are two schools, identified by 0 (till 2008) and 1 (after 2009);
# GENDER: F and M
#TEST, LAB, BEHAV: variables of the score in the tests and observational forms at LAB (Laboratory practical skills assessment)
# and BEHAV related to accomplishement school rules behavior
# LAB work and Behavior, normally if they follow the rules stablished at school
# GRADE: categories 0 to 6, representing, respectively, 7,8,9,10,11, 10p(techical) and 11p (Technical) 
# M4CID, category 0 and 1, respectively, without 4C/ID and with 4CID;
# CLASS, 0 to 100, height average, CLASS=0.5*TEST+0.3*LAB+0.2*BEHAV: 
#this will be used in a variable to be defined below, RANK: a study for student addaptation to the school system

#Table 3: Variable description

##########DATA structure

# graph analysis, all grades, global sample

#Normality: tests, histograms and QQ plots
p1<-ggplot(data,aes(TEST,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p2<-ggplot(data,aes(LAB,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p3<-ggplot(data,aes(BEHAV,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p4<-ggplot(data,aes(CLASS,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
# figure 1: Histograms with kernel density estimate for TEST, LAB, BEHAV and CLASS variables 
grid.arrange(p1, p2, p3, p4,  nrow = 2, top="Normality analysis on graphs",bottom="Figure 1: Histograms with kernel density estimate for variables TEST, LAB, BEHAV and CLASS")

#Normality
# H0 means normal distribution
shapiro.test(data$TEST)
# Shapiro-Wilk normality test
# data:  data$TEST
# W = 0.99013, p-value = 4.927e-09
#Reject the H0, not normal
shapiro.test(data$LAB)
# Shapiro-Wilk normality test
# data:  data$LAB
# W = 0.97869, p-value = 8.224e-15
#Reject the H0, not normal
shapiro.test(data$BEHAV)
# Shapiro-Wilk normality test
# data:  data$BEHAV
# W = 0.9704, p-value < 2.2e-16
#Reject the H0, not normal
shapiro.test(data$CLASS)
# Shapiro-Wilk normality test
# data:  data$CLASS
# W = 0.9942, p-value = 5.631e-06
#Reject the H0, not normal

# table 4: Shapiro test results

#Conclusion: samples not normal

# box plots
#global 2003 to 2019 global variable boxplots
summary(data)
?color
p5<-ggplot(data,aes(DATE,TEST)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=TEST),se=FALSE)+ geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p6<-ggplot(data,aes(DATE,LAB)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=LAB),se=FALSE)+ geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p7<-ggplot(data,aes(DATE,BEHAV)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=BEHAV),se=FALSE)+ geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p8<-ggplot(data,aes(DATE,CLASS)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=CLASS),se=FALSE)+geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
# figure 2: Boxplot time series for TEST, LAB, BEHAV and CLASS variables  
grid.arrange(p5, p6, p7, p8,  nrow = 2, top="Boxplot time series",bottom="Figure 2: all data sample")

# Junior, school 0 and 1
data_junior<-filter(data,GRADE <="2") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
p9<-ggplot(data_junior,aes(DATE,TEST)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=TEST),se=FALSE)+ geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p10<-ggplot(data_junior,aes(DATE,LAB)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=LAB),se=FALSE)+ geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p11<-ggplot(data_junior,aes(DATE,BEHAV)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=BEHAV),se=FALSE)+ geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=25,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p12<-ggplot(data_junior,aes(DATE,CLASS)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=CLASS),se=FALSE)+geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=25,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
# figure 3: Boxplot time series for TEST, LAB, BEHAV and CLASS variables junior grade  
grid.arrange(p9, p10, p11, p12,  nrow = 2, top="Boxplot time series",bottom="Figure 3: data sample junior grade for school 0 and 1")

# Junior, School 1
data_junior_SCHOOL1<-filter(data,GRADE <="2" & SCHOOL =="1") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
p13<-ggplot(data_junior_SCHOOL1,aes(DATE,TEST)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=TEST),se=FALSE)+ geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p14<-ggplot(data_junior_SCHOOL1,aes(DATE,LAB)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=LAB),se=FALSE)+geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p15<-ggplot(data_junior_SCHOOL1,aes(DATE,BEHAV)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=BEHAV),se=FALSE)+geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=25,label="School 1",col="red")+geom_text(x=2015+2,y=25,label="4C/ID Methodology",col="blue")
p16<-ggplot(data_junior_SCHOOL1,aes(DATE,CLASS)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=CLASS),se=FALSE)+geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=20,label="School 1",col="red")+geom_text(x=2015+2,y=20,label="4C/ID Methodology",col="blue")
# figure 4: junior grade sample for school 1  
grid.arrange(p13, p14, p15, p16,  nrow = 2, top="Boxplot time series",bottom="figure 4: junior grade sample for school 1")

# junior high, school 1
data_juniorHigh_SCHOOL1<-filter(data,GRADE >"2" & SCHOOL =="1") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
p17<-ggplot(data_junior_SCHOOL1,aes(DATE,TEST)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=TEST),se=FALSE)+geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p18<-ggplot(data_junior_SCHOOL1,aes(DATE,LAB)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=LAB),se=FALSE)+ geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p19<-ggplot(data_junior_SCHOOL1,aes(DATE,BEHAV)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=BEHAV),se=FALSE)+geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p20<-ggplot(data_junior_SCHOOL1,aes(DATE,CLASS)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=CLASS),se=FALSE)+geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
# figure 5: junior high grade sample for school 1  
grid.arrange(p17, p18, p19, p20,  nrow = 2, top="Boxplot time series",bottom="Figure 5: junior high grade sample for school 1 ")

#p21<-ggplot(data) + geom_boxplot(aes(x=DATE,y=TEST, linetype = factor(SCHOOL))) + geom_smooth(aes(x=DATE,y=TEST, color = factor(M4CID)),se=FALSE)
p22<-ggplot(data) + geom_boxplot(aes(x=DATE,y=TEST, color = factor(SCHOOL), linetype = factor(M4CID))) #geom_smooth(aes(x=DATE,y=TEST, color = factor(M4CID)),se=FALSE)
#p23<-ggplot(data) + geom_boxplot(aes(x=DATE,y=LAB, linetype = factor(SCHOOL))) + geom_smooth(aes(x=DATE,y=LAB, color = factor(M4CID)),se=FALSE)
p24<-ggplot(data) + geom_boxplot(aes(x=DATE,y=LAB, color = factor(SCHOOL), linetype = factor(M4CID))) #geom_smooth(aes(x=DATE,y=LAB, color = factor(M4CID)),se=FALSE)
#p25<-ggplot(data) + geom_boxplot(aes(x=DATE,y=BEHAV, linetype = factor(SCHOOL))) + geom_smooth(aes(x=DATE,y=BEHAV, color = factor(M4CID)),se=FALSE)
p26<-ggplot(data) + geom_boxplot(aes(x=DATE,y=BEHAV, color = factor(SCHOOL), linetype = factor(M4CID))) # geom_smooth(aes(x=DATE,y=BEHAV, color = factor(M4CID)),se=FALSE)
p27<-ggplot(data) + geom_boxplot(aes(x=DATE,y=CLASS, color = factor(SCHOOL), linetype = factor(M4CID))) # geom_smooth(aes(x=DATE,y=BEHAV, color = factor(M4CID)),se=FALSE)
# figure 6: box plot grouped by school and 4C/ID methodology 
grid.arrange( p22,p24,p26,p27,nrow = 2, top="boxplots",bottom="Figure 6: boxplot grouped by school and 4C/ID methodology")

p28<-ggplot(data,aes(x=DATE,y=TEST,group = M4CID)) + geom_boxplot() + aes(factor(M4CID)) + stat_summary(fun = mean, geom="point",col="red")
p29<-ggplot(data,aes(x=DATE,y=LAB,group = M4CID)) + geom_boxplot() + aes(factor(M4CID)) + stat_summary(fun = mean, geom="point",col="red")
p30<-ggplot(data,aes(x=DATE,y=BEHAV,group = M4CID)) + geom_boxplot() + aes(factor(M4CID)) + stat_summary(fun = mean, geom="point",col="red")
p31<-ggplot(data,aes(x=DATE,y=CLASS,group = M4CID)) + geom_boxplot() + aes(factor(M4CID)) + stat_summary(fun = mean, geom="point",col="red")
# figure 7: boxplots grouped by 4C/ID methodology 
grid.arrange(p28, p29, p30, p31, nrow = 2, top="All sample boxplots" , bottom = "Figure 7: boxplots grouped by 4C/ID methodology showing the mean by the red dot")

# conclusion: 
# It seems that the 4CID effect is not positive for all the variables neither for the overall perspective 
# it seems that school 0 as better TEST grades them school 2, while there a jump when the 4CID is implemented;
# That is not so obvious for the LAB variable. Clearly that the BEHAV variable suuffers a loss with the implementation of 4CID. 
# for the CLASS variable, after a positive effect it goes down, perhaps following the BEHAV variable
# Only the LAB variable show an increase. The red dot and the skewness showed in figure 7, 
# show that could be no positive overall 4C/ID effect and the sample are not normal, which corroborate 
# upper analysis

# Scatterplots and Correlations
# time series regression and Local Polynomial Regression Fitting
p32<-ggplot(data,aes(DATE, TEST)) + geom_point() + geom_smooth(se=FALSE,color="blue") + geom_smooth(method=lm,se=FALSE,color="red",linetype=2)+geom_vline(xintercept=2013,col="green",linetype=2)+ geom_vline(xintercept=2008,col="green",linetype=2)+geom_text(x=2004,y=10,label="SCHOOL 0")+geom_text(x=2009+1,y=10,label="SCHOOL 1",col="red")+geom_text(x=2013+2,y=10,label="4C/ID",col="red")
summary(lm(DATE~TEST,data))$adj.r.squared
summary(lm(DATE~TEST,data))
# R^2=0.05
?geom_smooth
p33<-ggplot(data,aes(DATE,LAB)) + geom_point() + geom_smooth(se=FALSE,color="blue") + geom_smooth(method=lm,se=FALSE,color="red",linetype=2)+geom_vline(xintercept=2013,col="green",linetype=2)+ geom_vline(xintercept=2008,col="green",linetype=2)+geom_text(x=2004,y=10,label="SCHOOL 0")+geom_text(x=2009+1,y=10,label="SCHOOL 1",col="red")+geom_text(x=2013+2,y=10,label="4C/ID",col="red")
summary(lm(DATE~LAB,data))$adj.r.squared
# R^2= 0.0005
p34<-ggplot(data,aes(DATE, BEHAV)) + geom_point() + geom_smooth(se=FALSE,color="blue") + geom_smooth(method=lm,se=FALSE,color="red",linetype=2)+geom_vline(xintercept=2013,col="green",linetype=2)+ geom_vline(xintercept=2008,col="green",linetype=2)+geom_text(x=2004,y=10,label="SCHOOL 0")+geom_text(x=2009+1,y=10,label="SCHOOL 1",col="red")+geom_text(x=2013+2,y=10,label="4C/ID",col="red")
summary(lm(DATE~BEHAV,data))$adj.r.squared
# R^2= 0.05
p35<-ggplot(data,aes(DATE, CLASS)) + geom_point() + geom_smooth(se=FALSE,color="blue") + geom_smooth(method=lm,se=FALSE,color="red",linetype=2)+geom_vline(xintercept=2013,col="green",linetype=2)+ geom_vline(xintercept=2008,col="green",linetype=2)+geom_text(x=2004,y=10,label="SCHOOL 0")+geom_text(x=2009+1,y=10,label="SCHOOL 1",col="red")+geom_text(x=2013+2,y=10,label="4C/ID",col="red")
summary(lm(DATE~CLASS,data))$adj.r.squared
# R^2 = 0.04
#Figure 8: time series regression scatterplots
grid.arrange(p32, p33, p34, p35,  nrow = 2, top="Time series regression scatterplots", bottom="Figure 8: Local Polynomial Regression Fitting in red")

#Conclusion: overall, over time, it seems that there is a negative 4C/ID effect. 
#However, R^2 values are very weak. It can be seen that a school effect could be undeway

#global data sample variable correlations
# first attempt to make the indeoendency analysis to the variables: just to check if there is a 
# the levell of dependency of the variables, or to see if the assessement was equal tecnhique
p36<-ggplot(data,aes(TEST, LAB)) + geom_point() + geom_smooth(method=lm,se=FALSE)+geom_text(x=10,y=90,label="R^2 = 0.25; tau = 36%")
summary(lm(TEST~LAB,data))$adj.r.squared
#R2=0.25
#Not normal so use Kendall correlation test
cor.test(data$TEST, data$LAB,  method="kendall")
# Kendall's rank correlation tau
# data:  data$TEST and data$LAB
# z = 21.626, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau, 0.3648018
p37<-ggplot(data,aes(TEST, BEHAV)) + geom_point() + geom_smooth(method=lm)+geom_text(x=10,y=90,label="R^2 = 0.15; tau=28%")
summary(lm(TEST~BEHAV,data))$adj.r.squared
# R2=0.15
cor.test(data$TEST, data$BEHAV,  method="kendall")
# Kendall's rank correlation tau
# data:  data$TEST and data$BEHAV
# z = 16.882, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.2844868 
p38<-ggplot(data,aes(LAB, BEHAV)) + geom_point() + geom_smooth(method=lm)+geom_text(x=10,y=90,label="R^2 = 0.19; tau = 30%")
summary(lm(LAB~BEHAV,data))$adj.r.squared
#R2=0.19
cor.test(data$LAB, data$BEHAV,  method="kendall")
# Kendall's rank correlation tau
# data:  data$LAB and data$BEHAV
# z = 17.757, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.3001458 
p39<-ggplot(data,aes(TEST, CLASS)) + geom_point() + geom_smooth(method=lm)+geom_text(x=10,y=90,label="R^2 = 0.80; tau=72%")
summary(lm(TEST~CLASS,data))$adj.r.squared
#R2 0.80
cor.test(data$TEST, data$CLASS,  method="kendall")
# Kendall's rank correlation tau
# data:  data$TEST and data$CLASS
# z = 43.577, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.7277266 
#Figure 9: variable dependency analysis
grid.arrange(p36, p37, p38,p39,  nrow = 2,bottom="Figure 9: Variable dependency analyss", top="All sample variable correlations analysis")

ggplot(data,aes(LAB, CLASS)) + geom_point() + geom_smooth(method=lm)
ggplot(data,aes(BEHAV, CLASS)) + geom_point() + geom_smooth(method=lm)

# conclusion: these R2 low doesn't mean that the variables are dependent nor the 
# There is some degree of correlation that doesn't permit to conclude of the variable dependency
# The clear correlation between TEST and CLASS variable show that they are dependent and
# Kendal correlation test corroborates that variables are dependent but with a levell degree 
#arround 20 to 40%: not high; meaning that the way the variables are assessed are different. They correlate,
# but at a low levell, showing a bad levell, which means that they were assessed with different techniques.
# the class test correlation is just a refence that we know, in advance, that there is a math relationship
# Table 5: kendall's correlation
# 7,8,9
# limit the grade 0,1,2, meaning 7,8 and 9 grade 
data_junior<-filter(data,GRADE <="2") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
data_junior_M4CID0<-filter(data_junior,M4CID =="0") 
data_junior_M4CID1<-filter(data_junior,M4CID =="1") 
summary(data_junior)
summary(data_junior_M4CID0)
summary(data_junior_M4CID1)
p40<-ggplot(data_junior,aes(TEST,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p41<-ggplot(data_junior,aes(LAB,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p42<-ggplot(data_junior,aes(BEHAV,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p43<-ggplot(data_junior,aes(CLASS,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
#Figue 10: normality testing for the junior grade sample
grid.arrange(p40, p41, p42, p43,  nrow = 2, top="Junior grade histograms", bottom="Figure 10: normality testing for the junior grade sample")

# (secondary) junior high
# limit the grade 3, 4,5,6, meaning 10,11, 10p,11p grades 
data_sec<-filter(data,GRADE>="3") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
data_sec_M4CID0<-filter(data_sec,M4CID =="0") 
data_sec_M4CID1<-filter(data_sec,M4CID =="1") 
summary(data_sec)
summary(data_sec_M4CID0)
summary(data_sec_M4CID1)
p44<-ggplot(data_sec,aes(TEST,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p45<-ggplot(data_junior,aes(LAB,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p46<-ggplot(data_junior,aes(BEHAV,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p47<-ggplot(data_junior,aes(CLASS,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
#Figure 11: normality testing for the junior high grade sample
grid.arrange(p44, p45, p46, p47,  nrow = 2,top="Junior high grade histograms", bottom="Figure 11: normality testing for the junior high grade sample")

#conclusion: data seems to be not normal, except the CLASS variable

########## normality
# Global sample
#TEST
hist(data$TEST)
?qqplot
?stat_qq
qqplot(data$LAB,data$TEST)
p48<-ggplot(data, aes(sample = TEST)) + stat_qq() + stat_qq_line()+labs(y="TEST",x="Theoretical quantiles")
#qqnorm(data$TEST);qqline(data$TEST)
ks.test(data$TEST,dnorm(mean(data$TEST),sd(data$TEST)))
shapiro.test(data$TEST)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data$TEST and dnorm(mean(data$TEST), sd(data$TEST))
# D = 0.99385, p-value = 0.0123
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data$TEST
# W = 0.99013, p-value = 4.927e-09

hist(data$LAB)
p49<-ggplot(data, aes(sample = LAB)) + stat_qq() + stat_qq_line()+labs(y="LAB",x="Theoretical quantiles")
#qqnorm(data$LAB);qqline(data$LAB)
ks.test(data$LAB,dnorm(mean(data$LAB),sd(data$LAB)))
shapiro.test(data$LAB)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data$LAB and dnorm(mean(data$LAB), sd(data$LAB))
# D = 0.98769, p-value = 0.02583
# alternative hypothesis: two-sided
# # Shapiro-Wilk normality test
# data:  data$LAB
# W = 0.97869, p-value = 8.224e-15

hist(data$BEHAV)
p50<-ggplot(data, aes(sample = BEHAV)) + stat_qq() + stat_qq_line()+labs(y="BEHAV",x="Theoretical quantiles")
#qqnorm(data$BEHAV);qqline(data$BEHAV)
ks.test(data$BEHAV,dnorm(mean(data$BEHAV),sd(data$BEHAV)))
shapiro.test(data$BEHAV)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data$BEHAV and dnorm(mean(data$BEHAV), sd(data$BEHAV))
# D = 0.99938, p-value = 0.00123
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data$BEHAV
# W = 0.96819, p-value < 2.2e-16

hist(data$CLASS)
p51<-ggplot(data, aes(sample = CLASS)) + stat_qq() + stat_qq_line()+labs(y="CLASS",x="Theoretical quantiles")
#qqnorm(data$CLASS);qqline(data$CLASS)
ks.test(data$CLASS,dnorm(mean(data$CLASS),sd(data$CLASS)))
shapiro.test(data$CLASS)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data$CLASS and dnorm(mean(data$CLASS), sd(data$CLASS))
# D = 0.99938, p-value = 0.00246
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data$CLASS
# W = 0.9942, p-value = 5.631e-06

#School 0
data_JUNIOR_SCHOOL0<-filter(data,GRADE <="2" & SCHOOL=="0") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
head(data_JUNIOR_SCHOOL0)
hist(data_JUNIOR_SCHOOL0$TEST)
p52<-ggplot(data_JUNIOR_SCHOOL0, aes(sample = TEST)) + stat_qq() + stat_qq_line()+labs(y="TEST",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL0$TEST);qqline(data_JUNIOR_SCHOOL0$TEST)
ks.test(data_JUNIOR_SCHOOL0$TEST,dnorm(mean(data_JUNIOR_SCHOOL0$TEST),sd(data_JUNIOR_SCHOOL0$TEST)))
shapiro.test(data_JUNIOR_SCHOOL0$TEST)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL0$TEST and dnorm(mean(data_JUNIOR_SCHOOL0$TEST), sd(data_JUNIOR_SCHOOL0$TEST))
# D = 1, p-value = 0.004193
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL0$TEST
# W = 0.98662, p-value = 0.0002309
hist(data_JUNIOR_SCHOOL0$LAB)
p53<-ggplot(data_JUNIOR_SCHOOL0, aes(sample = LAB)) + stat_qq() + stat_qq_line()+labs(y="LAB",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL0$LAB);qqline(data_JUNIOR_SCHOOL0$LAB)
ks.test(data_JUNIOR_SCHOOL0$LAB,dnorm(mean(data_JUNIOR_SCHOOL0$LAB),sd(data_JUNIOR_SCHOOL0$LAB)))
shapiro.test(data_JUNIOR_SCHOOL0$LAB)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL0$LAB and dnorm(mean(data_JUNIOR_SCHOOL0$LAB), sd(data_JUNIOR_SCHOOL0$LAB))
# D = 1, p-value = 0.002096
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL0$LAB
# W = 0.97672, p-value = 6.809e-07
hist(data_JUNIOR_SCHOOL0$BEHAV)
p54<-ggplot(data_JUNIOR_SCHOOL0, aes(sample = BEHAV)) + stat_qq() + stat_qq_line()+labs(y="BEHAV",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL0$BEHAV);qqline(data_JUNIOR_SCHOOL0$BEHAV)
ks.test(data_JUNIOR_SCHOOL0$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL0$BEHAV),sd(data_JUNIOR_SCHOOL0$BEHAV)))
shapiro.test(data_JUNIOR_SCHOOL0$BEHAV)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL0$BEHAV and dnorm(mean(data_JUNIOR_SCHOOL0$BEHAV), sd(data_JUNIOR_SCHOOL0$BEHAV))
# D = 1, p-value = 0.002096
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL0$BEHAV
# W = 0.9822, p-value = 1.414e-05
hist(data_JUNIOR_SCHOOL0$CLASS)
p55<-ggplot(data_JUNIOR_SCHOOL0, aes(sample = CLASS)) + stat_qq() + stat_qq_line()+labs(y="CLASS",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL0$CLASS);qqline(data_JUNIOR_SCHOOL0$CLASS)
ks.test(data_JUNIOR_SCHOOL0$CLASS,dnorm(mean(data_JUNIOR_SCHOOL0$CLASS),sd(data_JUNIOR_SCHOOL0$CLASS)))
shapiro.test(data_JUNIOR_SCHOOL0$CLASS)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL0$CLASS and dnorm(mean(data_JUNIOR_SCHOOL0$CLASS), sd(data_JUNIOR_SCHOOL0$CLASS))
# D = 1, p-value = 0.004193
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL0$CLASS
# W = 0.99272, p-value = 0.02054

#School 1
data_JUNIOR_SCHOOL1<-filter(data,GRADE <="2" & SCHOOL=="1") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
hist(data_JUNIOR_SCHOOL1$TEST)
p56<-ggplot(data_JUNIOR_SCHOOL1, aes(sample = TEST)) + stat_qq() + stat_qq_line()+labs(y="TEST",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL1$TEST);qqline(data_JUNIOR_SCHOOL1$TEST)
ks.test(data_JUNIOR_SCHOOL1$TEST,dnorm(mean(data_JUNIOR_SCHOOL1$TEST),sd(data_JUNIOR_SCHOOL1$TEST)))
shapiro.test(data_JUNIOR_SCHOOL1$TEST)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$TEST and dnorm(mean(data_JUNIOR_SCHOOL1$TEST), sd(data_JUNIOR_SCHOOL1$TEST))
# D = 0.99605, p-value = 0.01053
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$TEST
# W = 0.98944, p-value = 2.827e-05
hist(data_JUNIOR_SCHOOL1$LAB)
p57<-ggplot(data_JUNIOR_SCHOOL1, aes(sample = LAB)) + stat_qq() + stat_qq_line()+labs(y="LAB",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL1$LAB);qqline(data_JUNIOR_SCHOOL1$LAB)
ks.test(data_JUNIOR_SCHOOL1$LAB,dnorm(mean(data_JUNIOR_SCHOOL1$LAB),sd(data_JUNIOR_SCHOOL1$LAB)))
shapiro.test(data_JUNIOR_SCHOOL1$LAB)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$LAB and dnorm(mean(data_JUNIOR_SCHOOL1$LAB), sd(data_JUNIOR_SCHOOL1$LAB))
# D = 0.98287, p-value = 0.03684
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$LAB
# W = 0.97816, p-value = 3.19e-09
hist(data_JUNIOR_SCHOOL1$BEHAV)
p58<-ggplot(data_JUNIOR_SCHOOL1, aes(sample = BEHAV)) + stat_qq() + stat_qq_line()+labs(y="BEHAV",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL1$BEHAV);qqline(data_JUNIOR_SCHOOL1$BEHAV)
ks.test(data_JUNIOR_SCHOOL1$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1$BEHAV),sd(data_JUNIOR_SCHOOL0$BEHAV)))
shapiro.test(data_JUNIOR_SCHOOL1$BEHAV)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$BEHAV and dnorm(mean(data_JUNIOR_SCHOOL1$BEHAV), sd(data_JUNIOR_SCHOOL0$BEHAV))
# D = 1, p-value = 0.001316
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$BEHAV
# W = 0.96139, p-value = 2.975e-13
hist(data_JUNIOR_SCHOOL1$CLASS)
p59<-ggplot(data_JUNIOR_SCHOOL1, aes(sample = CLASS)) + stat_qq() + stat_qq_line()+labs(y="CLASS",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL1$CLASS);qqline(data_JUNIOR_SCHOOL1$CLASS)
ks.test(data_JUNIOR_SCHOOL1$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1$CLASS),sd(data_JUNIOR_SCHOOL1$CLASS)))
shapiro.test(data_JUNIOR_SCHOOL1$CLASS)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$CLASS and dnorm(mean(data_JUNIOR_SCHOOL1$CLASS), sd(data_JUNIOR_SCHOOL1$CLASS))
# D = 1, p-value = 0.002632
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$CLASS
# W = 0.99364, p-value = 0.002669

#Figure 12: Normality QQ-plots for all sample
grid.arrange(p48,p49,p50,p51, nrow = 2,top="Normal QQ plot", bottom="Figure 12: normality testing for all sample")
#table 6: ks and shapiro p-values all sample

#Figure 13: Normality QQ-plots school 0
grid.arrange(p52, p53, p54, p55,  nrow = 2,top="Normal QQ plot", bottom="Figure 13: normality testing for SCHOOL 0")
#table 7: Kolmogorov-Smirnov and shapiro p-values for all sample

#Figure 14: Normality QQ-plots school 1
grid.arrange(p56, p57, p58, p59,  nrow = 2,top="Normal QQ plot", bottom="Figure 14: normality testing for SCHOOL 1")
#table 8: ks and shapiro p-values school 1

#conclusion: sample data don't behav as a normal distribution under both tests, nor if we
#do the same analysis for the school 0 and shcool 1. Meaning that we should use a non-parametric 
#hyphotesis testing stats

########### Inference (and independency analysis)
#Inference for the global differences in M4CID, no matter which school, for each data variables: TEST, LAB and BEHAV
head(data)
summary(data)
data_M4CID0<-filter(data,M4CID =="0") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
data_M4CID1<-filter(data,M4CID =="1") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
length(data_M4CID0$ID)#931
length(data_M4CID1$ID)#694
# Along the way, sample independency will be tested and non normality will be considered

#TEST
## Independency
sample1<-data.frame(sample(data_M4CID0$TEST,600))
sample2<-data.frame(sample(data_M4CID1$TEST,600))
#Kendall correlation test needs same sample length
cor.test(sample1$sample.data_M4CID0.TEST..600.,sample2$sample.data_M4CID1.TEST..600.,method = "kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_M4CID0.TEST..600. and sample2$sample.data_M4CID1.TEST..600.
# z = -0.1701, p-value = 0.8649
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.004717915 
# samples are independent, so its possible to use Wilcoxon inference test
ggplot(data) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
wilcox.test(data_M4CID0$TEST,data_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0$TEST and data_M4CID1$TEST
# W = 367208, p-value = 1.183e-06
# alternative hypothesis: true location shift is greater than 0

#LAB
sample1<-data.frame(sample(data_M4CID0$LAB,600))
sample2<-data.frame(sample(data_M4CID1$LAB,600))
#Kendall correlation test
cor.test(sample1$sample.data_M4CID0.LAB..600.,sample2$sample.data_M4CID1.LAB..600.,method = "kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_M4CID0.LAB..600. and sample2$sample.data_M4CID1.LAB..600.
# z = -0.67817, p-value = 0.4977
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01897084
# samples are independent, so its possible to use Wilcoxon inference test
ggplot(data) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
wilcox.test(data_M4CID0$LAB,data_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0$LAB and data_M4CID1$LAB
# W = 281090, p-value = 3.606e-06
# alternative hypothesis: true location shift is less than 0

#BEHAV
sample1<-data.frame(sample(data_M4CID0$BEHAV,600))
sample2<-data.frame(sample(data_M4CID1$BEHAV,600))
#Kendall's rank correlation tau
cor.test(sample1$sample.data_M4CID0.BEHAV..600.,sample2$sample.data_M4CID1.BEHAV..600.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_M4CID0.BEHAV..600. and sample2$sample.data_M4CID1.BEHAV..600.
# z = 1.1873, p-value = 0.2351
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.0330701
# samples are independent, so its possible to use Wilcoxon inference test
ggplot(data) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
wilcox.test(data_M4CID0$BEHAV,data_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0$BEHAV and data_M4CID1$BEHAV
# W = 393301, p-value = 2.976e-14
# alternative hypothesis: true location shift is greater than 0

#CLASS
sample1<-data.frame(sample(data_M4CID0$CLASS,600))
sample2<-data.frame(sample(data_M4CID1$CLASS,600))
#Kendall correlation test
cor.test(sample1$sample.data_M4CID0.CLASS..600.,sample2$sample.data_M4CID1.CLASS..600., method="kendall")
# Kendall's rank correlation ta
# data:  sample1$sample.data_M4CID0.CLASS..600. and sample2$sample.data_M4CID1.CLASS..600.
# z = 0.10071, p-value = 0.9198
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.002753148 
# samples are independent, so its possible to use Wilcoxon inference test
ggplot(data) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
wilcox.test(data_M4CID0$CLASS,data_M4CID1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0$CLASS and data_M4CID1$CLASS
# W = 349682, p-value = 0.002217
# alternative hypothesis: true location shift is greater than 0

#Inference conclusion for the 4C/ID treatment effect for all sample, 
#no matter which school, for each data variables: TEST, LAB and BEHAV

# Samples are independent, Wilcoxon inference non-parametric test
# for the global TEST, BEHAV and CLASS variables, negative 4CID effect;
# LAB variable, positive effect

#Table 9: independency and Wilcoxon rank sum test with continuity correction

#Hypothesis: does the patterns showned in the last graphs where there was a negative bump during the transition to the second 
#between the schools showed earlier in the graphs should be considered?
#Is there a school effect on the results?
#should the data between schools in the same 4CID conditions 0 be assessed? yes,
#but only the junior school should be compared
#we should use the CLASS to study the differences between schools and not the other
#variables; if we use the variable because it is a math relation qith the others
# just with class variable

#Inference for the differences in schools, without 4CID, for each variable: TEST, LAB, BEHAV and CLASS
## DATA
summary(data)
data_M4CID0<-filter(data,data$M4CID=="0")  %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
summary(data_M4CID0)
length(which(data_M4CID0$SCHOOL=="0"))#476 entrances
length(which(data_M4CID0$SCHOOL=="1"))#455 entrances

data_M4CID0_SCHOOL0 <- filter(data,SCHOOL == "0") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
data_M4CID0_SCHOOL1 <- filter(data,M4CID=="0" & SCHOOL == "1") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
summary(data_M4CID0_SCHOOL0)
summary(data_M4CID0_SCHOOL1)

# TEST
# Normality
hist(data_M4CID0$TEST)
qqnorm(data_M4CID0$TEST)
qqline(data_M4CID0$TEST)
ks.test(data_M4CID0$TEST,dnorm(mean(data_M4CID0$TEST),sd(data_M4CID0$TEST)))
shapiro.test(data_M4CID0$TEST)
# p-value << 0,05 so, conclusion: not normal
# The indeoendency is not measured because we are talking about two different schools in diffrent towns
ggplot(data_M4CID0) + geom_boxplot(aes(y=TEST, x=factor(SCHOOL)))
wilcox.test(data_M4CID0_SCHOOL0$TEST,data_M4CID0_SCHOOL1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$TEST and data_M4CID0_SCHOOL1$TEST
# W = 128852, p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0

# LAB
hist(data_M4CID0$LAB)
qqnorm(data_M4CID0$LAB)
qqline(data_M4CID0$LAB)
ks.test(data_M4CID0$LAB,dnorm(mean(data_M4CID0$LAB),sd(data_M4CID0$LAB)))
shapiro.test(data_M4CID0$LAB)
#p-value << 0,05 so, conclusion: not normal
ggplot(data_M4CID0) + geom_boxplot(aes(y=LAB, x=factor(SCHOOL)))
wilcox.test(data_M4CID0_SCHOOL0$LAB,data_M4CID0_SCHOOL1$LAB,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$LAB and data_M4CID0_SCHOOL1$LAB
# W = 140672, p-value = 1.345e-15
# alternative hypothesis: true location shift is greater than 0

# BEHAV
hist(data_M4CID0$BEHAV)
qqnorm(data_M4CID0$BEHAV)
qqline(data_M4CID0$BEHAV)
ks.test(data_M4CID0$BEHAV,dnorm(mean(data_M4CID0$BEHAV),sd(data_M4CID0$BEHAV)))
shapiro.test(data_M4CID0$BEHAV)
# p-vaue << 0,05, so conclusion: not normal
ggplot(data_M4CID0) + geom_boxplot(aes(y=BEHAV, x=factor(SCHOOL)))
wilcox.test(data_M4CID0_SCHOOL0$BEHAV,data_M4CID0_SCHOOL1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$BEHAV and data_M4CID0_SCHOOL1$BEHAV
# W = 90906, p-value = 1.119e-05
# alternative hypothesis: true location shift is less than 0

# CLASS
hist(data_M4CID0$CLASS)
qqnorm(data_M4CID0$CLASS)
qqline(data_M4CID0$CLASS)
ks.test(data_M4CID0$CLASS,dnorm(mean(data_M4CID0$CLASS),sd(data_M4CID0$CLASS)))
shapiro.test(data_M4CID0$CLASS)
#p-value < 0,05, so conclusion: not normal
ggplot(data_M4CID0) + geom_boxplot(aes(y=CLASS, x=factor(SCHOOL)))
wilcox.test(data_M4CID0_SCHOOL0$CLASS,data_M4CID0_SCHOOL1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$CLASS and data_M4CID0_SCHOOL1$CLASS
# W = 126821, p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0

# table 10 : Inference for the differences in school

#Conclusion
#Variable TEST, LAB, CLASS greater in school 0 then in school 1
#Variable BEHAV is less for the School 0 then in the school 1
# So, this pivotal moment, take this research to consider that 
#there is a school effect, that biased all variables, in the negative way except
#for the BEHAV variable, in the positive way for school 1. So this is going to be restricted to school 1:
# it is considered an analysis for the school 1 as a global data sample,
# and an analysis only at the junior level in school 1. Secondary school wil 
#not be analized because there is no scondary sample with no 4CID.  

## school effect

# calculus
summary(data)
data_SCHOOL0<-filter(data,SCHOOL=="0")  %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
data_JUNIOR_SCHOOL1_M4CID0<-filter(data,GRADE<="2",SCHOOL=="1",M4CID=="0")  %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
dif_mean_TEST <- mean(data_SCHOOL0$TEST)-mean(data_JUNIOR_SCHOOL1_M4CID0$TEST)
dif_median_TEST <- median(data_SCHOOL0$TEST)-median(data_JUNIOR_SCHOOL1_M4CID0$TEST)
dif_mean_TEST
dif_median_TEST
dif_mean_LAB <- mean(data_SCHOOL0$LAB)-mean(data_JUNIOR_SCHOOL1_M4CID0$LAB)
dif_median_LAB <- median(data_SCHOOL0$LAB)-median(data_JUNIOR_SCHOOL1_M4CID0$LAB)
dif_mean_LAB
dif_median_LAB
dif_mean_BEHAV <- mean(data_SCHOOL0$BEHAV)-mean(data_JUNIOR_SCHOOL1_M4CID0$BEHAV)
dif_median_BEHAV <- median(data_SCHOOL0$BEHAV)-median(data_JUNIOR_SCHOOL1_M4CID0$BEHAV)
dif_mean_BEHAV
dif_median_BEHAV
dif_mean_CLASS <- mean(data_SCHOOL0$CLASS)-mean(data_JUNIOR_SCHOOL1_M4CID0$CLASS)
dif_median_CLASS <- median(data_SCHOOL0$CLASS)-median(data_JUNIOR_SCHOOL1_M4CID0$CLASS)
dif_mean_CLASS
dif_median_CLASS

# table 11 : computed school differences for the junior sample school 0 and 1 without 4C/ID methodology

# prepararing the sample to take into account the differences; in the case of TEST, LAB and CLASS, it will
#be added to the sample of school 1 the differences computed in this variables. In the case of the variable BEHAV, 
# it will be added to school 0, in order to not create negative numbers, consequently, to keep the variables between 0 and 100;

# sample with only junior grade for both schools
data_junior<-filter(data,GRADE <= "2") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_computed<-filter(data,GRADE <= "2") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
# add to school 1
data_junior_computed$TEST<-ifelse(data_junior$SCHOOL=="1",data_junior$TEST+dif_mean_TEST,data_junior_computed$TEST)
data_junior_computed$LAB<-ifelse(data_junior$SCHOOL=="1",data_junior$LAB+dif_mean_LAB,data_junior_computed$LAB)
data_junior_computed$CLASS<-ifelse(data_junior$SCHOOL=="1",data_junior$CLASS+dif_mean_CLASS,data_junior_computed$CLASS)
data_junior_computed$BEHAV<-ifelse(data_junior$SCHOOL=="1",data_junior$BEHAV+dif_mean_BEHAV,data_junior_computed$BEHAV)
summary(data)
summary(data_junior)
summary(data_junior_computed)
# NOTE: the values of TEST, LAB and CLASS are over 100%: I think its not a problem because tehre is a need to compare
# the change: there is no limitation to 100% for the inference analysis: DOUBT/DISCUSSION!?!?!?!

#compare data_junior_computed
#compare samples after the computation explained just now:
#data_JUNIOR_SCHOOL0 and data_junior_computed_SCHOOL1_M4CID0: are they from the same distribution?

data_SCHOOL0<-filter(data,SCHOOL=="0") %>% select(ID,GENDER,BEHAV,M4CID,SCHOOL,GRADE,TEST,LAB, CLASS)
data_junior_computed_SCHOOL1_M4CID0<-filter(data_junior_computed,GRADE <= "2" & M4CID =="0" & SCHOOL=="1") %>% select(ID,GENDER,BEHAV,M4CID,SCHOOL,GRADE,TEST,LAB, CLASS)
summary(data_SCHOOL0)
summary(data_junior_computed_SCHOOL1_M4CID0)
#TEST
length(data_SCHOOL0$ID)#476
length(data_junior_computed_SCHOOL1_M4CID0$ID)#394
median(data_SCHOOL0$TEST)
median(data_junior_computed_SCHOOL1_M4CID0$TEST)
wilcox.test(data_SCHOOL0$TEST,data_junior_computed_SCHOOL1_M4CID0$TEST,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL0$TEST and data_junior_computed_SCHOOL1_M4CID0$TEST
# W = 97434, p-value = 0.321
# alternative hypothesis: true location shift is not equal to 0
#LAB
median(data_SCHOOL0$LAB)
median(data_junior_computed_SCHOOL1_M4CID0$LAB)
wilcox.test(data_SCHOOL0$LAB,data_junior_computed_SCHOOL1_M4CID0$LAB,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL0$LAB and data_junior_computed_SCHOOL1_M4CID0$LAB
# W = 92456, p-value = 0.7213
# alternative hypothesis: true location shift is not equal to 0
#BEHAV
median(data_SCHOOL0$BEHAV)
median(data_junior_computed_SCHOOL1_M4CID0$BEHAV)
hist(data_SCHOOL0$BEHAV)
hist(data_junior_computed_SCHOOL1_M4CID0$BEHAV)
wilcox.test(data_SCHOOL0$BEHAV,data_junior_computed_SCHOOL1_M4CID0$BEHAV,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL0$BEHAV and data_junior_computed_SCHOOL1_M4CID0$BEHAV
# W = 92320, p-value = 0.694
# alternative hypothesis: true location shift is not equal to 0

#CLASS
median(data_SCHOOL0$CLASS)
median(data_junior_computed_SCHOOL1_M4CID0$CLASS)
wilcox.test(data_SCHOOL0$CLASS,data_junior_computed_SCHOOL1_M4CID0$CLASS,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL0$CLASS and data_junior_computed_SCHOOL1_M4CID0$CLASS
# W = 95629, p-value = 0.6148
# alternative hypothesis: true location shift is not equal to 0

#Conclusion: yes, they are from the same sample distribution

# Inference for computed junior differences with and without 4C/ID, between school 0 and school 1 (with 4C/ID)
data_junior_computed_SCHOOL0_M4CID0<-filter(data_junior_computed,GRADE <= "2" & M4CID =="0",SCHOOL=="0") %>% select(ID,GENDER,BEHAV,M4CID,SCHOOL,GRADE,TEST,LAB, CLASS)
data_junior_computed_SCHOOL1_M4CID1<-filter(data_junior_computed,GRADE <= "2" & M4CID =="1",SCHOOL=="1") %>% select(ID,GENDER,BEHAV,M4CID,SCHOOL,GRADE,TEST,LAB, CLASS)
length(data_junior_computed_SCHOOL0_M4CID0$ID)#476
length(data_junior_computed_SCHOOL1_M4CID1$ID)#365

#TEST
summary(data_junior_computed)
hist(data_junior_computed$TEST)
qqnorm(data_junior_computed$TEST)
qqline(data_junior_computed$TEST)
ks.test(data_junior_computed$TEST,dnorm(mean(data_junior_computed$TEST),sd(data_junior_computed$TEST)))
shapiro.test(data_junior_computed$TEST)
#conclusion: not normal and the samples are indedependent considering that they are from differents schools
ggplot(data_junior_computed) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_SCHOOL0_M4CID0$TEST)#68
median(data_junior_computed_SCHOOL1_M4CID1$TEST)#70
wilcox.test(data_junior_computed_SCHOOL0_M4CID0$TEST,data_junior_computed_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_SCHOOL0_M4CID0$TEST and data_junior_computed_SCHOOL1_M4CID1$TEST
# W = 81215, p-value = 0.05266
# alternative hypothesis: true location shift is less than 0

#LAB
summary(data_junior_computed)
hist(data_junior_computed$LAB)
qqnorm(data_junior_computed$LAB)
qqline(data_junior_computed$LAB)
ks.test(data_junior_computed$LAB,dnorm(mean(data_junior_computed$LAB),sd(data_junior_computed$LAB)))
shapiro.test(data_junior_computed$LAB)
#conclusion: not normal
ggplot(data_junior_computed) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_SCHOOL0_M4CID0$LAB)#62
median(data_junior_computed_SCHOOL1_M4CID1$LAB)#77
wilcox.test(data_junior_computed_SCHOOL0_M4CID0$LAB,data_junior_computed_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_M4CID0_SCHOOL0$LAB and data_junior_computed_M4CID1_SCHOOL1$LAB
# W = 50056, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
summary(data_junior_computed)
hist(data_junior_computed$BEHAV)
qqnorm(data_junior_computed$BEHAV)
qqline(data_junior_computed$BEHAV)
ks.test(data_junior_computed$BEHAV,dnorm(mean(data_junior_computed$BEHAV),sd(data_junior_computed$BEHAV)))
shapiro.test(data_junior_computed$BEHAV)
#conclusion: not normal
ggplot(data_junior_computed) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_SCHOOL0_M4CID0$BEHAV)#77
median(data_junior_computed_SCHOOL1_M4CID1$BEHAV)#71
wilcox.test(data_junior_computed_SCHOOL0_M4CID0$BEHAV,data_junior_computed_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_SCHOOL0_M4CID0$BEHAV and data_junior_computed_SCHOOL1_M4CID1$BEHAV
# W = 106915, p-value = 4.693e-09
# alternative hypothesis: true location shift is greater than 0

# CLASS
summary(data_junior_computed)
hist(data_junior_computed$CLASS)
qqnorm(data_junior_computed$CLASS)
qqline(data_junior_computed$CLASS)
ks.test(data_junior_computed$CLASS,dnorm(mean(data_junior_computed$CLASS),sd(data_junior_computed$CLASS)))
shapiro.test(data_junior_computed$CLASS)
#conclusion: normal
ggplot(data_junior_computed) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_SCHOOL0_M4CID0$CLASS)#69
median(data_junior_computed_SCHOOL1_M4CID1$CLASS)#70
wilcox.test(data_junior_computed_SCHOOL0_M4CID0$CLASS,data_junior_computed_SCHOOL1_M4CID1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_SCHOOL0_M4CID0$CLASS and data_junior_computed_SCHOOL1_M4CID1$CLASS
# W = 74037, p-value = 0.9999
# alternative hypothesis: true location shift is greater than 0

# Inference conclusion for computed junior differences with and without 4C/ID, 
# between school 0 and school 1 with only 4C/ID
# TEST and CLASS no effect
# negative on BEHAV
# Positive effect for LAB

# Inference for computed junior differences between school 0 and global school 1 junior and junior high with 4CID
# Excluding School 1 with no 4C/ID
# use the calculus about the computed differences already made 

# see Table 11: Differences computed for junior school 0 and 1

# sample with only school 0 and school 1 junior and junior high with 4C/ID: exclude school 1 without 4C/ID
data_computed<-filter(data,!(SCHOOL == "1" & M4CID == "0")) %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

# add to school 1
data_computed$TEST<-ifelse(data_computed$M4CID==1,data_computed$TEST+dif_mean_TEST,data_computed$TEST)
data_computed$LAB<-ifelse(data_computed$M4CID==1,data_computed$LAB+dif_mean_LAB,data_computed$LAB)
data_computed$CLASS<-ifelse(data_computed$M4CID==1,data_computed$CLASS+dif_mean_CLASS,data_computed$CLASS)
data_computed$BEHAV<-ifelse(data_computed$SCHOOL==1,data_computed$BEHAV+dif_mean_BEHAV,data_computed$BEHAV)

data_computed_SCHOOL0_M4CID0<-filter(data,SCHOOL == "0" & M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_SCHOOL1_M4CID1<-filter(data,SCHOOL == "1" & !M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

length(data_computed_SCHOOL0_M4CID0$ID)#476
length(data_computed_SCHOOL1_M4CID1$ID)#694

#TEST
summary(data_computed)
hist(data_computed$TEST)
qqnorm(data_computed$TEST)
qqline(data_computed$TEST)
ks.test(data_computed$TEST,dnorm(mean(data_computed$TEST),sd(data_computed$TEST)))
shapiro.test(data_computed$TEST)
#conclusion: not normal
ggplot(data_computed) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))#not accurate, it includes school 1 and M4CID0
median(data_computed_SCHOOL0_M4CID0$TEST)#68
median(data_computed_SCHOOL1_M4CID1$TEST)#57
wilcox.test(data_computed_SCHOOL0_M4CID0$TEST,data_computed_SCHOOL1_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_SCHOOL0_M4CID0$TEST and data_computed_SCHOOL1_M4CID1$TEST
# W = 216666, p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0

#LAB
summary(data_computed)
hist(data_computed$LAB)
qqnorm(data_computed$LAB)
qqline(data_computed$LAB)
ks.test(data_computed$LAB,dnorm(mean(data_computed$LAB),sd(data_computed$LAB)))
shapiro.test(data_computed$LAB)
#conclusion: not normal
ggplot(data_computed) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))#not accurate, it includes school 1 and M4CID0
median(data_computed_SCHOOL0_M4CID0$LAB)#62
median(data_computed_SCHOOL1_M4CID1$LAB)#53
wilcox.test(data_computed_SCHOOL0_M4CID0$LAB,data_computed_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_SCHOOL0_M4CID0$LAB and data_computed_SCHOOL1_M4CID1$LAB
# W = 167673, p-value = 0.6703
# alternative hypothesis: true location shift is less than 0

# BEHAV
summary(data_computed)
hist(data_computed$BEHAV)
qqnorm(data_computed$BEHAV)
qqline(data_computed$BEHAV)
ks.test(data_computed$BEHAV,dnorm(mean(data_computed$BEHAV),sd(data_computed$BEHAV)))
shapiro.test(data_computed$BEHAV)
#conclusion: not normal
ggplot(data_computed) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))#not accurate, it includes school 1 and M4CID0
median(data_computed_SCHOOL0_M4CID0$TEST)#68
median(data_computed_SCHOOL1_M4CID1$TEST)#57
wilcox.test(data_computed_SCHOOL0_M4CID0$BEHAV,data_computed_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_SCHOOL0_M4CID0$BEHAV and data_computed_SCHOOL1_M4CID1$BEHAV
# W = 190144, p-value = 5.429e-06
# alternative hypothesis: true location shift is greater than 0

# CLASS
summary(data_computed)
hist(data_computed$CLASS)
qqnorm(data_computed$CLASS)
qqline(data_computed$CLASS)
ks.test(data_computed$CLASS,dnorm(mean(data_computed$CLASS),sd(data_computed$CLASS)))
shapiro.test(data_computed$CLASS)
#conclusion: not normal
ggplot(data_computed) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))#not accurate, it includes school 1 and M4CID0
median(data_computed_SCHOOL0_M4CID0$CLASS)#69
median(data_computed_SCHOOL1_M4CID1$CLASS)#61
wilcox.test(data_computed_SCHOOL0_M4CID0$CLASS,data_computed_SCHOOL1_M4CID1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_SCHOOL0_M4CID0$CLASS and data_computed_SCHOOL1_M4CID1$CLASS
# W = 205124, p-value = 9.848e-13
# alternative hypothesis: true location shift is greater than 0

# Conclusion computed diferences for junior and junior high school 0 and 1 
#(exclude school 1 without 4C/ID)
# TEST, BEHAV and CLASS negative
# No effect for LAB 

# Table 12: Inference for computed diferences for junior and junior high school 0 and 1 
# (excluding school 1 without 4C/ID)

# Inference for computed global sample differences with and without 4CID
# use the calculus about the computed differences already made 

# see Table 11: Differences computed for junior school 0 and 1

# sample with all sample
data_computed_all<-data # G for all sample

# values added to variables only of school 1
data_compute_alld$TEST<-ifelse(data_computed_all$M4CID==1,data_computed_all$TEST+dif_mean_TEST,data_computed_all$TEST)
data_computed_all$LAB<-ifelse(data_computed_all$M4CID==1,data_computed_all$LAB+dif_mean_LAB,data_computed_all$LAB)
data_computed_all$CLASS<-ifelse(data_computed_all$M4CID==1,data_computed_all$CLASS+dif_mean_CLASS,data_computed_all$CLASS)
data_computed_all$BEHAV<-ifelse(data_computed_all$SCHOOL==1,data_computed_all$BEHAV+dif_mean_BEHAV,data_computed_all$BEHAV)

#sample with and without 4C/ID, no matter which school
data_computed_all_M4CID0<-filter(data,M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_all_M4CID1<-filter(data,M4CID == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_computed_all)

length(data_computed_all_M4CID0$ID)#931
length(data_computed_all_M4CID1$ID)#694

#TEST
hist(data_computed_all$TEST)
qqnorm(data_computed_all$TEST)
qqline(data_computed_all$TEST)
ks.test(data_computed_all$TEST,dnorm(mean(data_computed_all$TEST),sd(data_computed_all$TEST)))
shapiro.test(data_computed_all$TEST)
#conclusion: not normal
ggplot(data_computed_all) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_all_M4CID0$TEST)#62
median(data_computed_all_M4CID1$TEST)#57
wilcox.test(data_computed_all_M4CID0$TEST,data_computed_all_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_all_M4CID0$TEST and data_computed_all_M4CID1$TEST
# W = 367208, p-value = 1.183e-06
# alternative hypothesis: true location shift is greater than 0

#LAB
hist(data_computed_all$LAB)
qqnorm(data_computed_all$LAB)
qqline(data_computed_all$LAB)
ks.test(data_computed_all$LAB,dnorm(mean(data_computed_all$LAB),sd(data_computed_all$LAB)))
shapiro.test(data_computed_all$LAB)
#conclusion: not normal
ggplot(data_computed_all) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_all_M4CID0$LAB)#60
median(data_computed_all_M4CID1$LAB)#53
wilcox.test(data_computed_all_M4CID0$LAB,data_computed_all_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_all_M4CID0$LAB and data_computed_all_M4CID1$LAB
# W = 281090, p-value = 3.606e-06
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_computed_all$BEAHV)
qqnorm(data_computed_all$BEHAV)
qqline(data_computed_all$BEHAV)
ks.test(data_computed_all$BEHAV,dnorm(mean(data_computed_all$BEHAV),sd(data_computed_all$BEHAV)))
shapiro.test(data_computed_all$BEHAV)
#conclusion: not normal
ggplot(data_computed_all) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_all_M4CID0$BEHAV)#79
median(data_computed_all_M4CID1$BEHAV)#73
wilcox.test(data_computed_all_M4CID0$BEHAV,data_computed_all_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_all_M4CID0$BEHAV and data_computed_all_M4CID1$BEHAV
# W = 393301, p-value = 2.976e-14
# alternative hypothesis: true location shift is greater than 0

# CLASS
hist(data_computed_all$CLASS)
qqnorm(data_computed_all$CLASS)
qqline(data_computed_all$CLASS)
ks.test(data_computed_all$CLASS,dnorm(mean(data_computed_all$CLASS),sd(data_computed_all$CLASS)))
shapiro.test(data_computed_all$CLASS)
#conclusion: normal
ggplot(data_computed_all) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_all_M4CID0$CLASS)#64
median(data_computed_all_M4CID1$CLASS)#61
wilcox.test(data_computed_all_M4CID0$CLASS,data_computed_all_M4CID1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_all_M4CID0$CLASS and data_computed_all_M4CID1$CLASS
# W = 349682, p-value = 0.002217
# alternative hypothesis: true location shift is greater than 0

# Conclusions 4C/ID treatment effect with computed samples for the entire sample
# TEST, BEHAV and CLASS negative
# LAB positive

# Table 13: Inference for computed differences for the entire sample

# Inference only for School 1, global sample (include junior high for school 1): sample not computed
summary(data)
data_SCHOOL1<-filter(data,data$SCHOOL=="1")  %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
length(which(data_SCHOOL1$M4CID=="0"))#455 entrances
length(which(data_SCHOOL1$M4CID=="1"))#694 entrances
data_SCHOOL1_M4CID0 <- filter(data_SCHOOL1,M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_SCHOOL1_M4CID1 <- filter(data_SCHOOL1,M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_SCHOOL1)
hist(data_SCHOOL1$TEST)
qqnorm(data_SCHOOL1$TEST)
qqline(data_SCHOOL1$TEST)
ks.test(data_SCHOOL1$TEST,dnorm(mean(data_SCHOOL1$TEST),sd(data_SCHOOL1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1$TEST and dnorm(mean(data_SCHOOL1$TEST), sd(data_SCHOOL1$TEST))
# D = 0.9913, p-value = 0.01652
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1$TEST
# W = 0.99139, p-value = 2.963e-06
#conclusion: Samples not normal, so use Kendall non-parametric correlation test

# TEST
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$TEST,400))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$TEST,400))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_SCHOOL1_M4CID0.TEST..400.,sample2$sample.data_SCHOOL1_M4CID1.TEST..400.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.TEST..400. and sample2$sample.data_SCHOOL1_M4CID1.TEST..400.
# z = 0.42391, p-value = 0.6716
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01441653 
# sample independency, so it is possible to use Wilcoxon non-parametric inference test
ggplot(data_SCHOOL1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_M4CID0$TEST)#59
median(data_SCHOOL1_M4CID1$TEST)#61
wilcox.test(data_SCHOOL1_M4CID0$TEST,data_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_M4CID0$TEST and data_SCHOOL1_M4CID1$TEST
# W = 150543, p-value = 0.09097
# alternative hypothesis: true location shift is less than 0

# LAB
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$LAB,400))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$LAB,400))
#Not normal, Kendall non-parametric correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.LAB..400.,sample2$sample.data_SCHOOL1_M4CID1.LAB..400.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.LAB..400. and sample2$sample.data_SCHOOL1_M4CID1.LAB..400.
# z = -0.36747, p-value = 0.7133
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01253338 
hist(data_SCHOOL1$LAB)
qqnorm(data_SCHOOL1$LAB)
qqline(data_SCHOOL1$LAB)
ks.test(data_SCHOOL1$LAB,dnorm(mean(data_SCHOOL1$LAB),sd(data_SCHOOL1$LAB)))
shapiro.test(data_SCHOOL1$LAB)
#conclusion: not normal
ggplot(data_SCHOOL1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_M4CID0$LAB)#53
median(data_SCHOOL1_M4CID1$LAB)#63
wilcox.test(data_SCHOOL1_M4CID0$LAB,data_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_M4CID0$LAB and data_SCHOOL1_M4CID1$LAB
# W = 113418, p-value = 3.1e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$BEHAV,400))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$BEHAV,400))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.BEHAV..400.,sample2$sample.data_SCHOOL1_M4CID1.BEHAV..400.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.BEHAV..400. and sample2$sample.data_SCHOOL1_M4CID1.BEHAV..400.
# z = -0.31395, p-value = 0.7536
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01074669 
hist(data_SCHOOL1$BEHAV)
qqnorm(data_SCHOOL1$BEHAV)
qqline(data_SCHOOL1$BEHAV)
ks.test(data_SCHOOL1$BEHAV,dnorm(mean(data_SCHOOL1$BEHAV),sd(data_SCHOOL1$BEHAV)))
shapiro.test(data_SCHOOL1$BEHAV)
#conclusion: not normal and independent
ggplot(data_SCHOOL1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_M4CID0$BEHAV)#81
median(data_SCHOOL1_M4CID1$BEHAV)#73
wilcox.test(data_SCHOOL1_M4CID0$BEHAV,data_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_M4CID0$BEHAV and data_SCHOOL1_M4CID1$BEHAV
# W = 206272, p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0

# CLASS
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$CLASS,400))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$CLASS,400))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.CLASS..400.,sample2$sample.data_SCHOOL1_M4CID1.CLASS..400.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.CLASS..400. and sample2$sample.data_SCHOOL1_M4CID1.CLASS..400.
# z = -0.50868, p-value = 0.611
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01705669 
hist(data_SCHOOL1$CLASS)
qqnorm(data_SCHOOL1$CLASS)
qqline(data_SCHOOL1$CLASS)
ks.test(data_SCHOOL1$CLASS,dnorm(mean(data_SCHOOL1$CLASS),sd(data_SCHOOL1$CLASS)))
shapiro.test(data_SCHOOL1$CLASS)
#conclusion: not normal and independent samples
ggplot(data_SCHOOL1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_M4CID0$CLASS)#59
median(data_SCHOOL1_M4CID1$CLASS)#61
wilcox.test(data_SCHOOL1_M4CID0$CLASS,data_SCHOOL1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_M4CID0$CLASS and data_SCHOOL1_M4CID1$CLASS
# W = 144558, p-value = 0.007708
# alternative hypothesis: true location shift is less than 0

# Table 14: Inference only for junior and junior high school 1

#Conclusions for sample junior and junior high school 1
# sample independent, no rejection of h0
# 4CID as no impact on TEST Variable;
# 4CID as positive impact in LAB and CLASS Variables;
# 4CID as a negative impact in BEHAV variable

# inference junior, school 1
data_JUNIOR_SCHOOL1<-filter(data,GRADE >="0" & GRADE <="2" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_JUNIOR_SCHOOL1)
summary(data_JUNIOR_SCHOOL1)

length(which(data_JUNIOR_SCHOOL1$M4CID=="0"))#394 entrances
length(which(data_JUNIOR_SCHOOL1$M4CID=="1"))#365 entrances

data_JUNIOR_SCHOOL1_M4CID0 <- filter(data_JUNIOR_SCHOOL1,M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_JUNIOR_SCHOOL1_M4CID1 <- filter(data_JUNIOR_SCHOOL1,M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_JUNIOR_SCHOOL1_M4CID0)
summary(data_JUNIOR_SCHOOL1_M4CID1)

# TEST
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$TEST,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$TEST,300))
#Kendall's correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.TEST..300.,sample2$sample.data_SCHOOL1_M4CID1.TEST..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.TEST..300. and sample2$sample.data_SCHOOL1_M4CID1.TEST..300.
# z = 0.023051, p-value = 0.9816
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.000907225 
hist(data_JUNIOR_SCHOOL1$TEST)
qqnorm(data_JUNIOR_SCHOOL1$TEST)
qqline(data_JUNIOR_SCHOOL1$TEST)
ks.test(data_JUNIOR_SCHOOL1$TEST,dnorm(mean(data_JUNIOR_SCHOOL1$TEST),sd(data_JUNIOR_SCHOOL1$TEST)))
shapiro.test(data_JUNIOR_SCHOOL1$TEST)
#conclusion: not normal but independent sample
ggplot(data_JUNIOR_SCHOOL1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_M4CID0$TEST)#54
median(data_JUNIOR_SCHOOL1_M4CID1$TEST)#58
wilcox.test(data_JUNIOR_SCHOOL1_M4CID0$TEST,data_JUNIOR_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_M4CID0$TEST and data_JUNIOR_SCHOOL1_M4CID1$TEST
# W = 65400, p-value = 0.01556
# alternative hypothesis: true location shift is less than 0

# LAB
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$LAB,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$LAB,300))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.LAB..300.,sample2$sample.data_SCHOOL1_M4CID1.LAB..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.LAB..300. and sample2$sample.data_SCHOOL1_M4CID1.LAB..300.
# z = -1.0773, p-value = 0.2813
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.04251206 
hist(data_JUNIOR_SCHOOL1$LAB)
qqnorm(data_JUNIOR_SCHOOL1$LAB)
qqline(data_JUNIOR_SCHOOL1$LAB)
ks.test(data_JUNIOR_SCHOOL1$LAB,dnorm(mean(data_JUNIOR_SCHOOL1$LAB),sd(data_JUNIOR_SCHOOL1$LAB)))
shapiro.test(data_JUNIOR_SCHOOL1$LAB)
#conclusion: not normal
ggplot(data_JUNIOR_SCHOOL1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_M4CID0$LAB)#53
median(data_JUNIOR_SCHOOL1_M4CID1$LAB)#66
wilcox.test(data_JUNIOR_SCHOOL1_M4CID0$LAB,data_JUNIOR_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_M4CID0$LAB and data_JUNIOR_SCHOOL1_M4CID1$LAB
# W = 47353, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$BEHAV,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$BEHAV,300))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.BEHAV..300.,sample2$sample.data_SCHOOL1_M4CID1.BEHAV..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.BEHAV..300. and sample2$sample.data_SCHOOL1_M4CID1.BEHAV..300.
# z = -0.10606, p-value = 0.9155
# alternative hypothesis: true tau is not equal to 0
# sample estimates:tau -0.004193422 
hist(data_JUNIOR_SCHOOL1$BEHAV)
qqnorm(data_JUNIOR_SCHOOL1$BEHAV)
qqline(data_JUNIOR_SCHOOL1$BEHAV)
ks.test(data_JUNIOR_SCHOOL1$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1$BEHAV),sd(data_JUNIOR_SCHOOL1$BEHAV)))
shapiro.test(data_JUNIOR_SCHOOL1$BEHAV)
#conclusion: not normal but independent
ggplot(data_JUNIOR_SCHOOL1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_M4CID0$BEHAV)#81
median(data_JUNIOR_SCHOOL1_M4CID1$BEHAV)#74
wilcox.test(data_JUNIOR_SCHOOL1_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_M4CID0$BEHAV and data_JUNIOR_SCHOOL1_M4CID1$BEHAV
# W = 88862, p-value = 9.536e-09
# alternative hypothesis: true location shift is greater than 0

# CLASS
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$CLASS,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$CLASS,300))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.CLASS..300.,sample2$sample.data_SCHOOL1_M4CID1.CLASS..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.CLASS..300. and sample2$sample.data_SCHOOL1_M4CID1.CLASS..300.
# z = -0.12555, p-value = 0.9001
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.004868571
hist(data_JUNIOR_SCHOOL1$CLASS)
qqnorm(data_JUNIOR_SCHOOL1$CLASS)
qqline(data_JUNIOR_SCHOOL1$CLASS)
ks.test(data_JUNIOR_SCHOOL1$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1$CLASS),sd(data_JUNIOR_SCHOOL1$CLASS)))
shapiro.test(data_JUNIOR_SCHOOL1$CLASS)
#conclusion: not normal but independent
ggplot(data_JUNIOR_SCHOOL1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_M4CID0$CLASS)#59
median(data_JUNIOR_SCHOOL1_M4CID1$CLASS)#62
wilcox.test(data_JUNIOR_SCHOOL1_M4CID0$CLASS,data_JUNIOR_SCHOOL1_M4CID1$CLASS,alternative = "less")
#Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_M4CID0$CLASS and data_JUNIOR_SCHOOL1_M4CID1$CLASS
# W = 60609, p-value = 9.099e-05
# alternative hypothesis: true location shift is less than 0

# Conclusions for sample junior School 1
# No rejection under H0, sample independent
# 4CID as a positive impact on TEST, LAB and CLASS Variable;
# 4CID as a negative impact in BEHAV variable

# Table 15: Inference for junior school 1

# Inference SCHOOL 1, Grade 7
summary (data)
data_7_SCHOOL1<-filter(data,GRADE =="0" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_7_SCHOOL1)
summary(data_7_SCHOOL1)

length(which(data_7_SCHOOL1$M4CID=="0"))#123 entrances
length(which(data_7_SCHOOL1$M4CID=="1"))#175 entrances

data_7_SCHOOL1_M4CID0 <- filter(data_7_SCHOOL1,M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_7_SCHOOL1_M4CID1 <- filter(data_7_SCHOOL1,M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_7_SCHOOL1_M4CID0)
summary(data_7_SCHOOL1_M4CID1)

# TEST
hist(data_7_SCHOOL1$TEST)
qqnorm(data_7_SCHOOL1$TEST)
qqline(data_7_SCHOOL1$TEST)
ks.test(data_7_SCHOOL1$TEST,dnorm(mean(data_7_SCHOOL1$TEST),sd(data_7_SCHOOL1$TEST)))
shapiro.test(data_7_SCHOOL1$TEST)
#conclusion: (almost) not normal
sample1<-data.frame(sample(data_7_SCHOOL1_M4CID0$TEST,100))
sample2<-data.frame(sample(data_7_SCHOOL1_M4CID1$TEST,100))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_M4CID0.TEST..100.,sample2$sample.data_7_SCHOOL1_M4CID1.TEST..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_M4CID0.TEST..100. and sample2$sample.data_7_SCHOOL1_M4CID1.TEST..100.
# z = 0.96893, p-value = 0.3326
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.06680381 
# Independet sample
ggplot(data_7_SCHOOL1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_7_SCHOOL1_M4CID0$TEST)#54
median(data_7_SCHOOL1_M4CID1$TEST)#56
wilcox.test(data_7_SCHOOL1_M4CID0$TEST,data_7_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_M4CID0$TEST and data_7_SCHOOL1_M4CID1$TEST
# W = 10222, p-value = 0.2304
# alternative hypothesis: true location shift is less than 0

# LAB
hist(data_7_SCHOOL1$LAB)
qqnorm(data_7_SCHOOL1$LAB)
qqline(data_7_SCHOOL1$LAB)
ks.test(data_7_SCHOOL1$LAB,dnorm(mean(data_7_SCHOOL1$LAB),sd(data_7_SCHOOL1$LAB)))
shapiro.test(data_7_SCHOOL1$LAB)
#conclusion: not normal
sample1<-data.frame(sample(data_7_SCHOOL1_M4CID0$LAB,100))
sample2<-data.frame(sample(data_7_SCHOOL1_M4CID1$LAB,100))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_M4CID0.LAB..100.,sample2$sample.data_7_SCHOOL1_M4CID1.LAB..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_M4CID0.LAB..100. and sample2$sample.data_7_SCHOOL1_M4CID1.LAB..100.
# z = 0.38462, p-value = 0.7005
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.02652684
# independent samples
ggplot(data_7_SCHOOL1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_7_SCHOOL1_M4CID0$LAB)#56
median(data_7_SCHOOL1_M4CID1$LAB)#64
wilcox.test(data_7_SCHOOL1_M4CID0$LAB,data_7_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_M4CID0$LAB and data_7_SCHOOL1_M4CID1$LAB
# W = 9501.5, p-value = 0.04258
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_7_SCHOOL1$BEHAV)
qqnorm(data_7_SCHOOL1$BEHAV)
qqline(data_7_SCHOOL1$BEHAV)
ks.test(data_7_SCHOOL1$BEHAV,dnorm(mean(data_7_SCHOOL1$BEHAV),sd(data_7_SCHOOL1$BEHAV)))
shapiro.test(data_7_SCHOOL1$BEHAV)
#conclusion: not normal
sample1<-data.frame(sample(data_7_SCHOOL1_M4CID0$BEHAV,100))
sample2<-data.frame(sample(data_7_SCHOOL1_M4CID1$BEHAV,100))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_M4CID0.BEHAV..100.,sample2$sample.data_7_SCHOOL1_M4CID1.BEHAV..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_M4CID0.BEHAV..100. and sample2$sample.data_7_SCHOOL1_M4CID1.BEHAV..100.
# z = 0.28353, p-value = 0.7768
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01971209 
# sample independent
ggplot(data_7_SCHOOL1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_7_SCHOOL1_M4CID0$BEHAV)#77
median(data_7_SCHOOL1_M4CID1$BEHAV)#71
wilcox.test(data_7_SCHOOL1_M4CID0$BEHAV,data_7_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_M4CID0$BEHAV and data_7_SCHOOL1_M4CID1$BEHAV
# W = 13290, p-value = 0.0002782
# alternative hypothesis: true location shift is greater than 0

# CLASS
hist(data_7_SCHOOL1$CLASS)
qqnorm(data_7_SCHOOL1$CLASS)
qqline(data_7_SCHOOL1$CLASS)
ks.test(data_7_SCHOOL1$CLASS,dnorm(mean(data_7_SCHOOL1$CLASS),sd(data_7_SCHOOL1$CLASS)))
shapiro.test(data_7_SCHOOL1$CLASS)
#conclusion: shapir report normal, while KS no
sample1<-data.frame(sample(data_7_SCHOOL1_M4CID0$CLASS,100))
sample2<-data.frame(sample(data_7_SCHOOL1_M4CID1$CLASS,100))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_M4CID0.CLASS..100.,sample2$sample.data_7_SCHOOL1_M4CID1.CLASS..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_M4CID0.CLASS..100. and sample2$sample.data_7_SCHOOL1_M4CID1.CLASS..100.
# z = 0.25613, p-value = 0.7978
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01739483 
# Independent samples
ggplot(data_7_SCHOOL1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_7_SCHOOL1_M4CID0$CLASS)#60
median(data_7_SCHOOL1_M4CID1$CLASS)#62
wilcox.test(data_7_SCHOOL1_M4CID0$CLASS,data_7_SCHOOL1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_M4CID0$CLASS and data_7_SCHOOL1_M4CID1$CLASS
# W = 10375, p-value = 0.2986
# alternative hypothesis: true location shift is less than 0

# Inference SCHOOL 1, Grade 8
summary (data)
data_8_SCHOOL1<-filter(data,GRADE =="1" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_8_SCHOOL1)
summary(data_8_SCHOOL1)

length(which(data_8_SCHOOL1$M4CID=="0"))#168 entrances
length(which(data_8_SCHOOL1$M4CID=="1"))#71 entrances, note less then 100 entrances

data_8_SCHOOL1_M4CID0 <- filter(data_8_SCHOOL1,M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_8_SCHOOL1_M4CID1 <- filter(data_8_SCHOOL1,M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_8_SCHOOL1_M4CID0)
summary(data_8_SCHOOL1_M4CID1)

# TEST
hist(data_8_SCHOOL1$TEST)
qqnorm(data_8_SCHOOL1$TEST)
qqline(data_8_SCHOOL1$TEST)
ks.test(data_8_SCHOOL1$TEST,dnorm(mean(data_8_SCHOOL1$TEST),sd(data_8_SCHOOL1$TEST)))
shapiro.test(data_8_SCHOOL1$TEST)
#conclusion: not normal
sample1<-data.frame(sample(data_8_SCHOOL1_M4CID0$TEST,70))
sample2<-data.frame(sample(data_8_SCHOOL1_M4CID1$TEST,70))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_M4CID0.TEST..70.,sample2$sample.data_8_SCHOOL1_M4CID1.TEST..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_M4CID0.TEST..70. and sample2$sample.data_8_SCHOOL1_M4CID1.TEST..70.
# z = 0.82758, p-value = 0.4079
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.06873291 
#samples are independent
ggplot(data_8_SCHOOL1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_8_SCHOOL1_M4CID0$TEST)#54
median(data_8_SCHOOL1_M4CID1$TEST)#58
wilcox.test(data_8_SCHOOL1_M4CID0$TEST,data_8_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_M4CID0$TEST and data_8_SCHOOL1_M4CID1$TEST
# W = 4805.5, p-value = 0.008855
# alternative hypothesis: true location shift is less than 0

# LAB
hist(data_8_SCHOOL1$LAB)
qqnorm(data_8_SCHOOL1$LAB)
qqline(data_8_SCHOOL1$LAB)
ks.test(data_8_SCHOOL1$LAB,dnorm(mean(data_8_SCHOOL1$LAB),sd(data_8_SCHOOL1$LAB)))
shapiro.test(data_8_SCHOOL1$LAB)
#conclusion: not normal by shapiro analysis
sample1<-data.frame(sample(data_8_SCHOOL1_M4CID0$LAB,70))
sample2<-data.frame(sample(data_8_SCHOOL1_M4CID1$LAB,70))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_M4CID0.LAB..70.,sample2$sample.data_8_SCHOOL1_M4CID1.LAB..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_M4CID0.LAB..70. and sample2$sample.data_8_SCHOOL1_M4CID1.LAB..70.
# z = -0.88109, p-value = 0.3783
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.07393898 
# sample are independent
ggplot(data_8_SCHOOL1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_8_SCHOOL1_M4CID0$LAB)#55
median(data_8_SCHOOL1_M4CID1$LAB)#68
wilcox.test(data_8_SCHOOL1_M4CID0$LAB,data_8_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_M4CID0$LAB and data_8_SCHOOL1_M4CID1$LAB
# W = 3190.5, p-value = 6.628e-09
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_8_SCHOOL1$BEHAV)
qqnorm(data_8_SCHOOL1$BEHAV)
qqline(data_8_SCHOOL1$BEHAV)
ks.test(data_8_SCHOOL1$BEHAV,dnorm(mean(data_8_SCHOOL1$BEHAV),sd(data_8_SCHOOL1$BEHAV)))
shapiro.test(data_8_SCHOOL1$BEHAV)
#conclusion: not normal
sample1<-data.frame(sample(data_8_SCHOOL1_M4CID0$BEHAV,70))
sample2<-data.frame(sample(data_8_SCHOOL1_M4CID1$BEHAV,70))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_M4CID0.BEHAV..70.,sample2$sample.data_8_SCHOOL1_M4CID1.BEHAV..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_M4CID0.BEHAV..70. and sample2$sample.data_8_SCHOOL1_M4CID1.BEHAV..70.
# z = -0.22859, p-value = 0.8192
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01905233 
ggplot(data_8_SCHOOL1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_8_SCHOOL1_M4CID0$BEHAV)#85
median(data_8_SCHOOL1_M4CID1$BEHAV)#60
wilcox.test(data_8_SCHOOL1_M4CID0$BEHAV,data_8_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_M4CID0$BEHAV and data_8_SCHOOL1_M4CID1$BEHAV
# W = 9820, p-value = 1.419e-15
# alternative hypothesis: true location shift is greater than 0

# CLASS
hist(data_8_SCHOOL1$CLASS)
qqnorm(data_8_SCHOOL1$CLASS)
qqline(data_8_SCHOOL1$CLASS)
ks.test(data_8_SCHOOL1$CLASS,dnorm(mean(data_8_SCHOOL1$CLASS),sd(data_8_SCHOOL1$CLASS)))
shapiro.test(data_8_SCHOOL1$CLASS)
#conclusion: not normal
sample1<-data.frame(sample(data_8_SCHOOL1_M4CID0$CLASS,70))
sample2<-data.frame(sample(data_8_SCHOOL1_M4CID1$CLASS,70))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_M4CID0.CLASS..70.,sample2$sample.data_8_SCHOOL1_M4CID1.CLASS..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_M4CID0.CLASS..70. and sample2$sample.data_8_SCHOOL1_M4CID1.CLASS..70.
# z = -1.2016, p-value = 0.2295
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.09825871 
# Sample independent
ggplot(data_8_SCHOOL1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_8_SCHOOL1_M4CID0$CLASS)#60
median(data_8_SCHOOL1_M4CID1$CLASS)#61
wilcox.test(data_8_SCHOOL1_M4CID0$CLASS,data_8_SCHOOL1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_M4CID0$CLASS and data_8_SCHOOL1_M4CID1$CLASS
# W = 5180.5, p-value = 0.05445
# alternative hypothesis: true location shift is less than 0

# Inference for SCHOOL 1, Grade 9
summary (data)
data_9_SCHOOL1<-filter(data,GRADE =="2" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_9_SCHOOL1)
summary(data_9_SCHOOL1)

length(which(data_9_SCHOOL1$M4CID=="0"))#103 entrances
length(which(data_9_SCHOOL1$M4CID=="1"))#119 entrances, note less then 100 entrances

data_9_SCHOOL1_M4CID0 <- filter(data_9_SCHOOL1,M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_9_SCHOOL1_M4CID1 <- filter(data_9_SCHOOL1,M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_9_SCHOOL1_M4CID0)
summary(data_9_SCHOOL1_M4CID1)

# TEST
hist(data_9_SCHOOL1$TEST)
qqnorm(data_9_SCHOOL1$TEST)
qqline(data_9_SCHOOL1$TEST)
ks.test(data_9_SCHOOL1$TEST,dnorm(mean(data_9_SCHOOL1$TEST),sd(data_9_SCHOOL1$TEST)))
shapiro.test(data_9_SCHOOL1$TEST)
#conclusion: not normal
sample1<-data.frame(sample(data_9_SCHOOL1_M4CID0$TEST,100))
sample2<-data.frame(sample(data_9_SCHOOL1_M4CID1$TEST,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_M4CID0.TEST..100.,sample2$sample.data_9_SCHOOL1_M4CID1.TEST..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_M4CID0.TEST..100. and sample2$sample.data_9_SCHOOL1_M4CID1.TEST..100.
# z = -0.38448, p-value = 0.7006
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.02643173 
# sample independent
ggplot(data_9_SCHOOL1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_9_SCHOOL1_M4CID0$TEST)#56
median(data_9_SCHOOL1_M4CID1$TEST)#58
wilcox.test(data_9_SCHOOL1_M4CID0$TEST,data_9_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_9_SCHOOL1_M4CID0$TEST and data_9_SCHOOL1_M4CID1$TEST
# W = 5568.5, p-value = 0.1205
# alternative hypothesis: true location shift is less than 0

# LAB
hist(data_9_SCHOOL1$LAB)
qqnorm(data_9_SCHOOL1$LAB)
qqline(data_9_SCHOOL1$LAB)
ks.test(data_9_SCHOOL1$LAB,dnorm(mean(data_9_SCHOOL1$LAB),sd(data_9_SCHOOL1$LAB)))
shapiro.test(data_9_SCHOOL1$LAB)
#conclusion: not normal
sample1<-data.frame(sample(data_9_SCHOOL1_M4CID0$LAB,100))
sample2<-data.frame(sample(data_9_SCHOOL1_M4CID1$LAB,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_M4CID0.LAB..100.,sample2$sample.data_9_SCHOOL1_M4CID1.LAB..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_M4CID0.LAB..100. and sample2$sample.data_9_SCHOOL1_M4CID1.LAB..100.
# z = -0.5785, p-value = 0.5629
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.0400083 
ggplot(data_9_SCHOOL1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_9_SCHOOL1_M4CID0$LAB)#44
median(data_9_SCHOOL1_M4CID1$LAB)#66
wilcox.test(data_9_SCHOOL1_M4CID0$LAB,data_9_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_9_SCHOOL1_M4CID0$LAB and data_9_SCHOOL1_M4CID1$LAB
# W = 2193.5, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_9_SCHOOL1$BEHAV)
qqnorm(data_9_SCHOOL1$BEHAV)
qqline(data_9_SCHOOL1$BEHAV)
ks.test(data_9_SCHOOL1$BEHAV,dnorm(mean(data_9_SCHOOL1$BEHAV),sd(data_9_SCHOOL1$BEHAV)))
shapiro.test(data_9_SCHOOL1$BEHAV)
#conclusion: not normal
sample1<-data.frame(sample(data_9_SCHOOL1_M4CID0$BEHAV,100))
sample2<-data.frame(sample(data_9_SCHOOL1_M4CID1$BEHAV,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_M4CID0.BEHAV..100.,sample2$sample.data_9_SCHOOL1_M4CID1.BEHAV..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_M4CID0.BEHAV..100. and sample2$sample.data_9_SCHOOL1_M4CID1.BEHAV..100.
# z = -0.068661, p-value = 0.9453
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.004794302 
# samples independent
ggplot(data_9_SCHOOL1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_9_SCHOOL1_M4CID0$BEHAV)#80
median(data_9_SCHOOL1_M4CID1$BEHAV)#85
wilcox.test(data_9_SCHOOL1_M4CID0$BEHAV,data_9_SCHOOL1_M4CID1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_9_SCHOOL1_M4CID0$BEHAV and data_9_SCHOOL1_M4CID1$BEHAV
# W = 5249.5, p-value = 0.03274
# alternative hypothesis: true location shift is less than 0

# CLASS
hist(data_9_SCHOOL1$CLASS)
qqnorm(data_9_SCHOOL1$CLASS)
qqline(data_9_SCHOOL1$CLASS)
ks.test(data_9_SCHOOL1$CLASS,dnorm(mean(data_9_SCHOOL1$CLASS),sd(data_9_SCHOOL1$CLASS)))
shapiro.test(data_9_SCHOOL1$CLASS)
#conclusion: normal by Shapiro
sample1<-data.frame(sample(data_9_SCHOOL1_M4CID0$CLASS,100))
sample2<-data.frame(sample(data_9_SCHOOL1_M4CID1$CLASS,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_M4CID0.CLASS..100.,sample2$sample.data_9_SCHOOL1_M4CID1.CLASS..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_M4CID0.CLASS..100. and sample2$sample.data_9_SCHOOL1_M4CID1.CLASS..100.
# z = -0.86966, p-value = 0.3845
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.05907344 
ggplot(data_9_SCHOOL1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_9_SCHOOL1_M4CID0$CLASS)#55
median(data_9_SCHOOL1_M4CID1$CLASS)#64
wilcox.test(data_9_SCHOOL1_M4CID0$CLASS,data_9_SCHOOL1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_9_SCHOOL1_M4CID0$CLASS and data_9_SCHOOL1_M4CID1$CLASS
# W = 4003.5, p-value = 4.264e-06
# alternative hypothesis: true location shift is less than 0

# Table 16: Inference for junior school 1, GRADES 7, 8 AND 9TH

#Conclusion
#samples not normal but independent
# GRADE 7
# 4CID has no impact on TEST and CLASS Variables;
# 4CID has positive impact in LAB Variable;
# 4CID has a negative impact in BEHAV variable

#GRADE 8
# Note: M4CID=="1",#71 entrances, less then 100 entrances
# 4CID has a positive impact on TEST, LAB, 
# 4CID negative on BEHAV Variable;
# 4CID has no impact on CLASS

#Grade 9
# 4CID has no impact on TEST Variable;
# 4CID has positive impact in LAB, BEHAV and CLASS Variable;

########## RANK
#This analysis uses school 1 and junior grade sample only
summary(data)
data_JUNIOR_SCHOOL1<-filter(data,GRADE <="2" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_JUNIOR_SCHOOL1)
summary(data_JUNIOR_SCHOOL1)
data_JUNIOR_SCHOOL1$RANK <-
  ifelse(data_JUNIOR_SCHOOL1$CLASS <=45, 1,
         ifelse((data_JUNIOR_SCHOOL1$CLASS >45 & data_JUNIOR_SCHOOL1$CLASS<70), 2,
                ifelse(data_JUNIOR_SCHOOL1$CLASS >=70, 3,NA)))
summary(data_JUNIOR_SCHOOL1)
length(data_JUNIOR_SCHOOL1$ID)#759 entrances
length(which(data_JUNIOR_SCHOOL1$RANK=="1"))#98
length(which(data_JUNIOR_SCHOOL1$RANK=="1"))/length(data_JUNIOR_SCHOOL1$ID)#13%
length(which(data_JUNIOR_SCHOOL1$RANK=="2"))#453
length(which(data_JUNIOR_SCHOOL1$RANK=="2"))/length(data_JUNIOR_SCHOOL1$ID)#60%
length(which(data_JUNIOR_SCHOOL1$RANK=="3"))#208
length(which(data_JUNIOR_SCHOOL1$RANK=="3"))/length(data_JUNIOR_SCHOOL1$ID)#27%

#RANK 1, 98,13%
#RANK 2, 453, 60%
#RANK 3, 208, 27% 

#RANK 1
data_JUNIOR_SCHOOL1_RANK1 <- filter(data_JUNIOR_SCHOOL1,RANK=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK1_M4CID0 <- filter(data_JUNIOR_SCHOOL1_RANK1, M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK1_M4CID1 <- filter(data_JUNIOR_SCHOOL1_RANK1, M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_JUNIOR_SCHOOL1_RANK1_M4CID0)
length(data_JUNIOR_SCHOOL1_RANK1_M4CID0$ID)#57
length(data_JUNIOR_SCHOOL1_RANK1_M4CID1$ID)#41, note: under 100 sample entrances

# TEST
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID0$TEST,40))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID1$TEST,40))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.TEST..40.,sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.TEST..40.,method="kendall")
#Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.TEST..40. and sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.TEST..40.
# z = -1.4512, p-value = 0.1467
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1642386
# independent!

hist(data_JUNIOR_SCHOOL1_RANK1$TEST)
qqnorm(data_JUNIOR_SCHOOL1_RANK1$TEST)
qqline(data_JUNIOR_SCHOOL1_RANK1$TEST)
ks.test(data_JUNIOR_SCHOOL1_RANK1$TEST,dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$TEST),sd(data_JUNIOR_SCHOOL1_RANK1$TEST)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK1$TEST)
#conclusion: not normal
ggplot(data_JUNIOR_SCHOOL1_RANK1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK1_M4CID0$TEST)#34
median(data_JUNIOR_SCHOOL1_RANK1_M4CID1$TEST)#29
wilcox.test(data_JUNIOR_SCHOOL1_RANK1_M4CID0$TEST,data_JUNIOR_SCHOOL1_RANK1_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK1_M4CID0$TEST and data_JUNIOR_SCHOOL1_RANK1_M4CID1$TEST
# W = 1513.5, p-value = 0.006511
# alternative hypothesis: true location shift is greater than 0

# LAB
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID0$LAB,40))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID1$LAB,40))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.LAB..40.,sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.LAB..40.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.LAB..40. and sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.LAB..40.
# z = -0.28178, p-value = 0.7781
# alternative hypothesis: true tau is not equal to 0
# sample estimates:tau -0.03221523 
hist(data_JUNIOR_SCHOOL1_RANK1$LAB)
qqnorm(data_JUNIOR_SCHOOL1_RANK1$LAB)
qqline(data_JUNIOR_SCHOOL1_RANK1$LAB)
ks.test(data_JUNIOR_SCHOOL1_RANK1$LAB,dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$LAB),sd(data_JUNIOR_SCHOOL1_RANK1$LAB)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK1$LAB)
#conclusion: not normal
ggplot(data_JUNIOR_SCHOOL1_RANK1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK1_M4CID0$LAB)#26
median(data_JUNIOR_SCHOOL1_RANK1_M4CID1$LAB)#35
wilcox.test(data_JUNIOR_SCHOOL1_RANK1_M4CID0$LAB,data_JUNIOR_SCHOOL1_RANK1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK1_M4CID0$LAB and data_JUNIOR_SCHOOL1_RANK1_M4CID1$LAB
# W = 720, p-value = 0.0006179
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID0$BEHAV,40))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID1$BEHAV,40))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.BEHAV..40.,sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.BEHAV..40.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.BEHAV..40. and sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.BEHAV..40.
# z = 0.36358, p-value = 0.7162
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04125851 
hist(data_JUNIOR_SCHOOL1_RANK1$BEHAV)
qqnorm(data_JUNIOR_SCHOOL1_RANK1$BEHAV)
qqline(data_JUNIOR_SCHOOL1_RANK1$BEHAV)
ks.test(data_JUNIOR_SCHOOL1_RANK1$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$BEHAV),sd(data_JUNIOR_SCHOOL1_RANK1$BEHAV)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK1$BEHAV)
#conclusion: normal
ggplot(data_JUNIOR_SCHOOL1_RANK1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK1_M4CID0$BEHAV)#65
median(data_JUNIOR_SCHOOL1_RANK1_M4CID1$BEHAV)#50
wilcox.test(data_JUNIOR_SCHOOL1_RANK1_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_RANK1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK1_M4CID0$BEHAV and data_JUNIOR_SCHOOL1_RANK1_M4CID1$BEHAV
# W = 1757, p-value = 1.125e-05
# alternative hypothesis: true location shift is greater than 0

#CLASS
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID0$CLASS,40))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID1$CLASS,40))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.CLASS..40.,sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.CLASS..40.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.CLASS..40. and sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.CLASS..40.
# z = -0.86253, p-value = 0.3884
# alternative hypothesis: true tau is not equal to 0
# sample estimates:tau -0.09523841
#independent
hist(data_JUNIOR_SCHOOL1_RANK1$CLASS)
qqnorm(data_JUNIOR_SCHOOL1_RANK1$CLASS)
qqline(data_JUNIOR_SCHOOL1_RANK1$CLASS)
ks.test(data_JUNIOR_SCHOOL1_RANK1$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$CLASS),sd(data_JUNIOR_SCHOOL1_RANK1$CLASS)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK1$CLASS)
#conclusion: not normal
ggplot(data_JUNIOR_SCHOOL1_RANK1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK1_M4CID0$CLASS)#39
median(data_JUNIOR_SCHOOL1_RANK1_M4CID1$CLASS)#37
wilcox.test(data_JUNIOR_SCHOOL1_RANK1_M4CID0$CLASS,data_JUNIOR_SCHOOL1_RANK1_M4CID1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK1_M4CID0$CLASS and data_JUNIOR_SCHOOL1_RANK1_M4CID1$CLASS
# W = 1443.5, p-value = 0.02402
# alternative hypothesis: true location shift is greater than 0

#RANK 2
data_JUNIOR_SCHOOL1_RANK2 <- filter(data_JUNIOR_SCHOOL1,RANK=="2") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK2_M4CID0 <- filter(data_JUNIOR_SCHOOL1,RANK=="2"& M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK2_M4CID1 <- filter(data_JUNIOR_SCHOOL1,RANK=="2"& M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
length(data_JUNIOR_SCHOOL1_RANK2_M4CID0$ID)#250
length(data_JUNIOR_SCHOOL1_RANK2_M4CID1$ID)#203

# TEST
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID0$TEST,200))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID1$TEST,200))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.TEST..200.,sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.TEST..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.TEST..200. and sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.TEST..200.
# z = 0.72906, p-value = 0.466
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.03554116 
# independent

hist(data_JUNIOR_SCHOOL1_RANK2$TEST)
qqnorm(data_JUNIOR_SCHOOL1_RANK2$TEST)
qqline(data_JUNIOR_SCHOOL1_RANK2$TEST)
ks.test(data_JUNIOR_SCHOOL1_RANK2$TEST,dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$TEST),sd(data_JUNIOR_SCHOOL1_RANK2$TEST)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK2$TEST)
#conclusion: normal 
ggplot(data_JUNIOR_SCHOOL1_RANK2) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK2_M4CID0$TEST)#53
median(data_JUNIOR_SCHOOL1_RANK2_M4CID1$TEST)#52
wilcox.test(data_JUNIOR_SCHOOL1_RANK2_M4CID0$TEST,data_JUNIOR_SCHOOL1_RANK2_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK2_M4CID0$TEST and data_JUNIOR_SCHOOL1_RANK2_M4CID1$TEST
# W = 25922, p-value = 0.3467
# alternative hypothesis: true location shift is greater than 0

# LAB
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID0$LAB,200))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID1$LAB,200))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.LAB..200.,sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.LAB..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.LAB..200. and sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.LAB..200.
# z = -0.74621, p-value = 0.4555
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.0364125 
#independent
hist(data_JUNIOR_SCHOOL1_RANK2$LAB)
qqnorm(data_JUNIOR_SCHOOL1_RANK2$LAB)
qqline(data_JUNIOR_SCHOOL1_RANK2$LAB)
ks.test(data_JUNIOR_SCHOOL1_RANK2$LAB,dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$LAB),sd(data_JUNIOR_SCHOOL1_RANK2$LAB)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK2$LAB)
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK2) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK2_M4CID0$LAB)#51
median(data_JUNIOR_SCHOOL1_RANK2_M4CID1$LAB)#62
wilcox.test(data_JUNIOR_SCHOOL1_RANK2_M4CID0$LAB,data_JUNIOR_SCHOOL1_RANK2_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK2_M4CID0$LAB and data_JUNIOR_SCHOOL1_RANK2_M4CID1$LAB
# W = 15035, p-value = 4.166e-14
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID0$BEHAV,200))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID1$BEHAV,200))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.BEHAV..200.,sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.BEHAV..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.BEHAV..200. and sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.BEHAV..200.
# z = 0.25505, p-value = 0.7987
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01243584 
# independent
hist(data_JUNIOR_SCHOOL1_RANK2$BEHAV)
qqnorm(data_JUNIOR_SCHOOL1_RANK2$BEHAV)
qqline(data_JUNIOR_SCHOOL1_RANK2$BEHAV)
ks.test(data_JUNIOR_SCHOOL1_RANK2$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$BEHAV),sd(data_JUNIOR_SCHOOL1_RANK2$BEHAV)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK2$BEHAV)
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK2) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK2_M4CID0$BEHAV)#80
median(data_JUNIOR_SCHOOL1_RANK2_M4CID1$BEHAV)#67
wilcox.test(data_JUNIOR_SCHOOL1_RANK2_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_RANK2_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK2_M4CID0$BEHAV and data_JUNIOR_SCHOOL1_RANK2_M4CID1$BEHAV
# W = 35570, p-value = 9.215e-14
# alternative hypothesis: true location shift is greater than 0

#CLASS
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID0$CLASS,200))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID1$CLASS,200))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.CLASS..200.,sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.CLASS..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.CLASS..200. and sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.CLASS..200.
# z = -0.75983, p-value = 0.4474
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.03623354 
hist(data_JUNIOR_SCHOOL1_RANK2$CLASS)
qqnorm(data_JUNIOR_SCHOOL1_RANK2$CLASS)
qqline(data_JUNIOR_SCHOOL1_RANK2$CLASS)
ks.test(data_JUNIOR_SCHOOL1_RANK2$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$CLASS),sd(data_JUNIOR_SCHOOL1_RANK2$CLASS)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK2$CLASS)
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK2) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK2_M4CID0$CLASS)#57
median(data_JUNIOR_SCHOOL1_RANK2_M4CID1$CLASS)#58
wilcox.test(data_JUNIOR_SCHOOL1_RANK2_M4CID0$CLASS,data_JUNIOR_SCHOOL1_RANK2_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK2_M4CID0$CLASS and data_JUNIOR_SCHOOL1_RANK2_M4CID1$CLASS
# W = 23192, p-value = 0.05766
# alternative hypothesis: true location shift is less than 0

#RANK 3
data_JUNIOR_SCHOOL1_RANK3 <- filter(data_JUNIOR_SCHOOL1,RANK=="3") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK3_M4CID0 <- filter(data_JUNIOR_SCHOOL1,RANK=="3"& M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK3_M4CID1 <- filter(data_JUNIOR_SCHOOL1,RANK=="3"& M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
length(data_JUNIOR_SCHOOL1_RANK3_M4CID0$ID)#87
length(data_JUNIOR_SCHOOL1_RANK3_M4CID1$ID)#121

# TEST
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID0$TEST,80))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID1$TEST,80))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.TEST..80.,sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.TEST..80.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.TEST..80. and sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.TEST..80.
# z = -0.37059, p-value = 0.7109
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.02895722 
# independent
hist(data_JUNIOR_SCHOOL1_RANK3$TEST)
qqnorm(data_JUNIOR_SCHOOL1_RANK3$TEST)
qqline(data_JUNIOR_SCHOOL1_RANK3$TEST)
ks.test(data_JUNIOR_SCHOOL1_RANK3$TEST,dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$TEST),sd(data_JUNIOR_SCHOOL1_RANK3$TEST)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK3$TEST)
#conclusion: normal 
ggplot(data_JUNIOR_SCHOOL1_RANK3) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK3_M4CID0$TEST)#79
median(data_JUNIOR_SCHOOL1_RANK3_M4CID1$TEST)#80
wilcox.test(data_JUNIOR_SCHOOL1_RANK3_M4CID0$TEST,data_JUNIOR_SCHOOL1_RANK3_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK3_M4CID0$TEST and data_JUNIOR_SCHOOL1_RANK3_M4CID1$TEST
# W = 5029.5, p-value = 0.2927
# alternative hypothesis: true location shift is less than 0

# LAB
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID0$LAB,80))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID1$LAB,80))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.LAB..80.,sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.LAB..80.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.LAB..80. and sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.LAB..80.
# z = -0.69158, p-value = 0.4892
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.05415192 
hist(data_JUNIOR_SCHOOL1_RANK3$LAB)
qqnorm(data_JUNIOR_SCHOOL1_RANK3$LAB)
qqline(data_JUNIOR_SCHOOL1_RANK3$LAB)
ks.test(data_JUNIOR_SCHOOL1_RANK3$LAB,dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$LAB),sd(data_JUNIOR_SCHOOL1_RANK3$LAB)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK3$LAB)
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK3) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK3_M4CID0$LAB)#73
median(data_JUNIOR_SCHOOL1_RANK3_M4CID1$LAB)#80
wilcox.test(data_JUNIOR_SCHOOL1_RANK3_M4CID0$LAB,data_JUNIOR_SCHOOL1_RANK3_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK3_M4CID0$LAB and data_JUNIOR_SCHOOL1_RANK3_M4CID1$LAB
# W = 3358, p-value = 4.243e-06
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID0$BEHAV,80))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID1$BEHAV,80))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.BEHAV..80.,sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.BEHAV..80.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.BEHAV..80. and sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.BEHAV..80.
# z = 0.86535, p-value = 0.3868
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.06884236
#Independency
hist(data_JUNIOR_SCHOOL1_RANK3$BEHAV)
qqnorm(data_JUNIOR_SCHOOL1_RANK3$BEHAV)
qqline(data_JUNIOR_SCHOOL1_RANK3$BEHAV)
ks.test(data_JUNIOR_SCHOOL1_RANK3$BEHAV,dnorm(mean(data_SCHOOL1_RANK3$BEHAV),sd(data_SCHOOL1_RANK3$BEHAV)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK3$BEHAV)
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK3) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK3_M4CID0$BEHAV)#94
median(data_JUNIOR_SCHOOL1_RANK3_M4CID1$BEHAV)#90
wilcox.test(data_JUNIOR_SCHOOL1_RANK3_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_RANK3_M4CID1$BEHAV,alternative = "greater")
# # Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK3_M4CID0$BEHAV and data_JUNIOR_SCHOOL1_RANK3_M4CID1$BEHAV
# W = 6894, p-value = 6.883e-05
# alternative hypothesis: true location shift is greater than 0

#CLASS
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID0$CLASS,80))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID1$CLASS,80))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.CLASS..80.,sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.CLASS..80.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.CLASS..80. and sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.CLASS..80.
# z = 0.41559, p-value = 0.6777
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.0317865 
#independent
hist(data_JUNIOR_SCHOOL1_RANK3$CLASS)
qqnorm(data_JUNIOR_SCHOOL1_RANK3$CLASS)
qqline(data_JUNIOR_SCHOOL1_RANK3$CLASS)
ks.test(data_JUNIOR_SCHOOL1_RANK3$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$CLASS),sd(data_JUNIOR_SCHOOL1_RANK3$CLASS)))
shapiro.test(data_JUNIOR_SCHOOL1_RANK3$CLASS)
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK3) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK3_M4CID0$CLASS)#77
median(data_JUNIOR_SCHOOL1_RANK3_M4CID1$CLASS)#81
wilcox.test(data_JUNIOR_SCHOOL1_RANK3_M4CID0$CLASS,data_JUNIOR_SCHOOL1_RANK3_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK3_M4CID0$CLASS and data_JUNIOR_SCHOOL1_RANK3_M4CID1$CLASS
# W = 4495, p-value = 0.03643
# alternative hypothesis: true location shift is less than 0

# Table 17: Inference for junior school 1, RANK 1, 2, 3

#Conclusion
# RANK 1: negative effect for TEST, BEHAV, CLASS and positive on LAB
# RANK 2: no effect for TEST and CLASS, positive for LAB, negative on BEHAV
# RANK 3: no effect for TEST, negative for BEHAV and positive for LAB and CLASS
# Only RANK 3, the more adapted, benefit more with the 4CID methodology
# RANK 1 and 2, the less adapted benefit only on the LAB variable;

########## GENDER
#This analysis uses school 1 and junior grade sample
data_junior_SCHOOL1<-filter(data,GRADE <="2" & SCHOOL=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_FEMALE<-filter(data,GRADE<="2" & SCHOOL=="1",GENDER=="F") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_MALE<-filter(data,GRADE<="2" & SCHOOL=="1",GENDER=="M") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_M4CID0_MALE<-filter(data,GRADE >="0" & GRADE <="2" & SCHOOL=="1" & M4CID =="0"  & GENDER == "M") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_M4CID0_FEMALE<-filter(data,GRADE >="0" & GRADE <="2" & SCHOOL=="1" & M4CID =="0" & GENDER == "F") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_M4CID1_MALE<-filter(data,GRADE >="0" & GRADE <="2" & SCHOOL=="1" & M4CID =="1"  & GENDER == "M") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_M4CID1_FEMALE<-filter(data,GRADE >="0" & GRADE <="2" & SCHOOL=="1" & M4CID =="1" & GENDER == "F") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

summary(data_junior_SCHOOL1)
length(data_junior_SCHOOL1$ID)
length(which(data_junior_SCHOOL1$GENDER=="F"))
length(which(data_junior_SCHOOL1$GENDER=="M"))
length(which(data_junior_SCHOOL1$GENDER=="F"))/length(data_junior_SCHOOL1$ID)
length(which(data_junior_SCHOOL1$GENDER=="M"))/length(data_junior_SCHOOL1$ID)
length(data_junior_SCHOOL1_M4CID0_FEMALE$ID)#191
length(data_junior_SCHOOL1_M4CID1_FEMALE$ID)#169
length(data_junior_SCHOOL1_M4CID0_MALE$ID)#203
length(data_junior_SCHOOL1_M4CID1_MALE$ID)#196

#sample entries: 759; Female = 360 (47%); Male=399 53%();
#data sample not normal but independet

#TEST
# independency
sample1<-data.frame(sample(data_junior_SCHOOL1_M4CID0_FEMALE$TEST,150))
sample2<-data.frame(sample(data_junior_SCHOOL1_M4CID1_FEMALE$TEST,150))
#Kendall correlation test
cor.test(sample1$sample.data_junior_SCHOOL1_M4CID0_FEMALE.TEST..150.,sample2$sample.data_junior_SCHOOL1_M4CID1_FEMALE.TEST..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_junior_SCHOOL1_M4CID0_FEMALE.TEST..150. and sample2$sample.data_junior_SCHOOL1_M4CID1_FEMALE.TEST..150.
# z = 1.405, p-value = 0.16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.07853475 
sample3<-data.frame(sample(data_junior_SCHOOL1_M4CID0_MALE$TEST,150))
sample4<-data.frame(sample(data_junior_SCHOOL1_M4CID1_MALE$TEST,150))
#Kendall correlation test
cor.test(sample3$sample.data_junior_SCHOOL1_M4CID0_MALE.TEST..150.,sample4$sample.data_junior_SCHOOL1_M4CID1_MALE.TEST..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_junior_SCHOOL1_M4CID0_MALE.TEST..150. and sample4$sample.data_junior_SCHOOL1_M4CID1_MALE.TEST..150.
# z = 0.55939, p-value = 0.5759
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.03126279
# samples are independent
p60<-ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=TEST, linetype = factor(GENDER)))
median(data_junior_SCHOOL1_M4CID0_FEMALE$TEST)#54
median(data_junior_SCHOOL1_M4CID1_FEMALE$TEST)#61
median(data_junior_SCHOOL1_M4CID0_MALE$TEST)#54
median(data_junior_SCHOOL1_M4CID1_MALE$TEST)#56
#It seems that the female benefit, less impact on male
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$TEST,data_junior_SCHOOL1_M4CID1_MALE$TEST, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$TEST and data_junior_SCHOOL1_M4CID1_MALE$TEST
# W = 19379, p-value = 0.3275
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$TEST,data_junior_SCHOOL1_M4CID1_FEMALE$TEST, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$TEST and data_junior_SCHOOL1_M4CID1_FEMALE$TEST
# W = 13462, p-value = 0.003293
# alternative hypothesis: true location shift is less than 0

#LAB
sample1<-data.frame(sample(data_junior_SCHOOL1_M4CID0_FEMALE$LAB,150))
sample2<-data.frame(sample(data_junior_SCHOOL1_M4CID1_FEMALE$LAB,150))
#Kendall correlation test
cor.test(sample1$sample.data_junior_SCHOOL1_M4CID0_FEMALE.LAB..150.,sample2$sample.data_junior_SCHOOL1_M4CID1_FEMALE.LAB..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_junior_SCHOOL1_M4CID0_FEMALE.LAB..150. and sample2$sample.data_junior_SCHOOL1_M4CID1_FEMALE.LAB..150.
# z = -1.2413, p-value = 0.2145
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.06971223 
sample3<-data.frame(sample(data_junior_SCHOOL1_M4CID0_MALE$LAB,150))
sample4<-data.frame(sample(data_junior_SCHOOL1_M4CID1_MALE$LAB,150))
#Kendall correlation test
cor.test(sample3$sample.data_junior_SCHOOL1_M4CID0_MALE.LAB..150.,sample4$sample.data_junior_SCHOOL1_M4CID1_MALE.LAB..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_junior_SCHOOL1_M4CID0_MALE.LAB..150. and sample4$sample.data_junior_SCHOOL1_M4CID1_MALE.LAB..150.
# z = 1.2979, p-value = 0.1943
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.07271416 
# samples are independent
median(data_junior_SCHOOL1_M4CID0_FEMALE$LAB)#52
median(data_junior_SCHOOL1_M4CID1_FEMALE$LAB)#70
median(data_junior_SCHOOL1_M4CID0_MALE$LAB)#53
median(data_junior_SCHOOL1_M4CID1_MALE$LAB)#62
#It seems that the female benefit, less impact on male
p61<-ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=LAB, linetype = factor(GENDER)))
#It seems that both male/female benefit
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$LAB,data_junior_SCHOOL1_M4CID1_MALE$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$LAB and data_junior_SCHOOL1_M4CID1_MALE$LAB
# W = 15124, p-value = 1.716e-05
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$LAB,data_junior_SCHOOL1_M4CID1_FEMALE$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$LAB and data_junior_SCHOOL1_M4CID1_FEMALE$LAB
# W = 7745, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

#BEHAV
# Independency
sample1<-data.frame(sample(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV,150))
sample2<-data.frame(sample(data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV,150))
#Kendall correlation test
cor.test(sample1$sample.data_junior_SCHOOL1_M4CID0_FEMALE.BEHAV..150.,sample2$sample.data_junior_SCHOOL1_M4CID1_FEMALE.BEHAV..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_junior_SCHOOL1_M4CID0_FEMALE.BEHAV..150. and sample2$sample.data_junior_SCHOOL1_M4CID1_FEMALE.BEHAV..150.
# z = 0.087917, p-value = 0.9299
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.004975126 
sample3<-data.frame(sample(data_junior_SCHOOL1_M4CID0_MALE$BEHAV,150))
sample4<-data.frame(sample(data_junior_SCHOOL1_M4CID1_MALE$BEHAV,150))
#Kendall correlation test
cor.test(sample3$sample.data_junior_SCHOOL1_M4CID0_MALE.BEHAV..150.,sample4$sample.data_junior_SCHOOL1_M4CID1_MALE.BEHAV..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_junior_SCHOOL1_M4CID0_MALE.BEHAV..150. and sample4$sample.data_junior_SCHOOL1_M4CID1_MALE.BEHAV..150.
# z = 0.34496, p-value = 0.7301
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01941931 
# sampleas are independent
median(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV)#82
median(data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV)#81
median(data_junior_SCHOOL1_M4CID0_MALE$BEHAV)#81
median(data_junior_SCHOOL1_M4CID1_MALE$BEHAV)#66
p62<-ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=BEHAV, linetype = factor(GENDER)))
#It seems that the male/female don't benefit, male is worse
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$BEHAV,data_junior_SCHOOL1_M4CID1_MALE$BEHAV, alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$BEHAV and data_junior_SCHOOL1_M4CID1_MALE$BEHAV
# W = 27558, p-value = 1.402e-11
# alternative hypothesis: true location shift is greater than 0
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV,data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV, alternative = "greater")
# #Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV and data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV
# W = 17146, p-value = 0.1534
# alternative hypothesis: true location shift is greater than 0

#CLASS
#Independency
sample1<-data.frame(sample(data_junior_SCHOOL1_M4CID0_FEMALE$CLASS,150))
sample2<-data.frame(sample(data_junior_SCHOOL1_M4CID1_FEMALE$CLASS,150))
#Kendall correlation test
cor.test(sample1$sample.data_junior_SCHOOL1_M4CID0_FEMALE.CLASS..150.,sample2$sample.data_junior_SCHOOL1_M4CID1_FEMALE.CLASS..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_junior_SCHOOL1_M4CID0_FEMALE.CLASS..150. and sample2$sample.data_junior_SCHOOL1_M4CID1_FEMALE.CLASS..150.
# z = 0.32176, p-value = 0.7476
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.0177475 
sample3<-data.frame(sample(data_junior_SCHOOL1_M4CID0_MALE$CLASS,150))
sample4<-data.frame(sample(data_junior_SCHOOL1_M4CID1_MALE$CLASS,150))
#Kendall correlation test
cor.test(sample3$sample.data_junior_SCHOOL1_M4CID0_MALE.CLASS..150.,sample4$sample.data_junior_SCHOOL1_M4CID1_MALE.CLASS..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_junior_SCHOOL1_M4CID0_MALE.CLASS..150. and sample4$sample.data_junior_SCHOOL1_M4CID1_MALE.CLASS..150.
# z = 0.45176, p-value = 0.6514
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.02490928 
#samples are independent
median(data_junior_SCHOOL1_M4CID0_FEMALE$CLASS)#59
median(data_junior_SCHOOL1_M4CID1_FEMALE$CLASS)#67
median(data_junior_SCHOOL1_M4CID0_MALE$CLASS)#59
median(data_junior_SCHOOL1_M4CID1_MALE$CLASS)#59
p63<-ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=CLASS, linetype = factor(GENDER)))
#It seems that the female have more benefit then the male
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$CLASS,data_junior_SCHOOL1_M4CID1_MALE$CLASS, alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$CLASS and data_junior_SCHOOL1_M4CID1_MALE$CLASS
# W = 19177, p-value = 0.5338
# alternative hypothesis: true location shift is not equal to 0
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$CLASS,data_junior_SCHOOL1_M4CID1_FEMALE$CLASS, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$CLASS and data_junior_SCHOOL1_M4CID1_FEMALE$CLASS
# W = 11344, p-value = 5.695e-07
# alternative hypothesis: true location shift is less than 0

# Figure 15: junior sample boxplots for GENDER analysis 
grid.arrange(p60, p61, p62, p63,  nrow = 2,top="Box plot 4C/ID treatment effect", bottom="Figure 15: junior sample boxplots for GENDER analysis")

# Table 18: Inference sample junior school 1 for GENDER

#Conclusion
# Male for Test no impact; female positive impact
# Both benefit in LAB variable
# In the BEHAV variable negative impact for MALE and no impact for FEMALE
# positive impact on female Class variable but no impact for the male
# Female, averall, benefit more

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

########## METHODOLOGY

## Hypothesis test discussion

## Normality and Independent samples


########## Summary results and notes

# The main goal is to understand the impact of the use of Inductive Methodology 4C/ID by answering the questions: 
#   1 - changing the learning methodologies from one mainly deductive (Direct Instruction[^1]) to another one mostly inductive (4C/ID [^2]), means what for your students? Who benefits? What are the pros and cons about this methodological decision?
#   2 - Should you, as a teacher of science and tech, move to implement the inductive strategy as your methodology for learning? 
#   3 - And if you do, what are the outcomes? What are the risks? What were the consequences for the student's academic results? 
#   4 - Is there a different impact between female and male? 
#   5 - And within the different grades?
#   6 - Student more adapted to school have benefit more with the use of 4CID methodology?

# sample are independent, and genneraly not normal

# conclusion: 
# It seems that the 4CID effect is not positive for all the variables neither for the overall perspective 
# it seems that school 0 as better TEST grades them school 2, while there a jump when the 4CID is implemented;
# That is not so obvious for the LAB variable. Clearly that the BEHAV variable suuffers a loss with the implementation of 4CID. 
# for the CLASS variable, after a positive effect it goes down, perhaps following the BEHAV variable
# Only the LAB variable show an increase. The red dot and the skewness showed in figure 7, 
# show that could be no positive overall 4C/ID effect and the sample are not normal, which corroborate 
# upper analysis

#Conclusion: overall, over time, it seems that there is a negative 4C/ID effect. 
#However, R^2 values are very weak. It can be seen that a school effect could be undeway

# conclusion: these R2 low doesn't mean that the variables are dependent nor the 
# There is some degree of correlation that doesn't permit to conclude of the variable dependency
# The clear correlation between TEST and CLASS variable show that they are dependent and
# Kendal correlation test corroborates that variables are dependent but with a levell degree 
#arround 20 to 40%: not high; meaning that the way the variables are assessed are different. They correlate,
# but at a low levell, showing a bad levell, which means that they were assessed with different techniques.
# the class test correlation is just a refence that we know, in advance, that there is a math relationship

#conclusion: data seems to be not normal, except the CLASS variable

#conclusion: sample data don't behav as a normal distribution under both tests, nor if we
#do the same analysis for the school 0 and shcool 1. Meaning that we should use a non-parametric 
#hyphotesis testing stats

# Conclusions diferent SAMPLES for the 4C/ID treatment effect

#conclusion: sample data don't behav as a normal distribution under both tests, nor if we
#do the same analysis for the school 0 and shcool 1. Meaning that we should use a non-parametric 
#hyphotesis testing stats

#Inference conclusion for the 4C/ID treatment effect for all sample, 
#no matter which school, for each data variables: TEST, LAB and BEHAV
# Samples are independent, Wilcoxon inference non-parametric test
# for the global TEST, BEHAV and CLASS variables, negative 4CID effect;
# LAB variable, positive effect

#Hypothesis: does the patterns showned in the last graphs where there was a negative bump during the transition to the second 
#between the schools showed earlier in the graphs should be considered?
#Is there a school effect on the results?
#should the data between schools in the same 4CID conditions 0 be assessed? yes,
#but only the junior school should be compared
#we should use the CLASS to study the differences between schools and not the other
#variables; if we use the variable because it is a math relation qith the others
# just with class variable


# Conclusion
#Variable TEST, LAB, CLASS greater in school 0 then in school 1
#Variable BEHAV is less for the School 0 then in the school 1
# So, this pivotal moment, take this research to consider that 
#there is a school effect, that biased all variables, in the negative way except
#for the BEHAV variable, in the positive way for school 1. So this is going to be restricted to school 1:
# it is considered an analysis for the school 1 as a global data sample,
# and an analysis only at the junior level in school 1. Secondary school wil 
#not be analized because there is no scondary sample with no 4CID.  

## SCHOOL efect
## Inference sample analysis, 4C/ID, taking in account the school effect
# prepararing the sample to take into account the differences; in the case of TEST, LAB and CLASS, it will
#be added to the sample of school 1 the differences computed in this variables. In the case of the variable BEHAV, 
# it will be added to school 0, in order to not create negative numbers, consequently, to keep the variables between 0 and 100;
#compare data_junior_computed
#compare samples after the computation explained just now:
#data_JUNIOR_SCHOOL0 and data_junior_computed_SCHOOL1_M4CID0: are they from the same distribution?
#Conlusion: yes, they are from the same sample distribution
# table 11 : computed school differences for the junior sample school 0 and 1 without 4C/ID methodology

# Inference conclusion for computed (school effect) junior differences 4C/ID tretament effec, 
# Samples: school 0 and school 1 with only 4C/ID
# TEST and CLASS no effect
# negative on BEHAV
# Positive effect for LAB

# Conclusion computed (school effect) diferences for junior and junior high school 0 and 1 
#(exclude school 1 without 4C/ID)
# TEST, BEHAV and CLASS negative
# No effect for LAB 

# Conclusions 4C/ID treatment effect with computed (school effect) variables sample diferences for the entire sample
# TEST, BEHAV and CLASS negative
# LAB positive

# 4C/ID tretament effect just for SCHOOL 1 sample
# sample junior and junior high school 1
# sample independent, no rejection of h0
# 4CID as no impact on TEST Variable;
# 4CID as positive impact in LAB and CLASS Variables;
# 4CID as a negative impact in BEHAV variable

# sample junior School 1
# No rejection under H0, sample independent
# 4CID as a positive impact on TEST, LAB and CLASS Variable;
# 4CID as a negative impact in BEHAV variable

#Conclusions GRADE
#samples not normal but independent
# GRADE 7
# 4CID has no impact on TEST and CLASS Variables;
# 4CID has positive impact in LAB Variable;
# 4CID has a negative impact in BEHAV variable

#GRADE 8
# Note: M4CID=="1",#71 entrances, less then 100 entrances
# 4CID has a positive impact on TEST, LAB, 
# 4CID negative on BEHAV Variable;
# 4CID has no impact on CLASS

#Grade 9
# 4CID has no impact on TEST Variable;
# 4CID has positive impact in LAB, BEHAV and CLASS Variable;

# Conclusion RANK

# RANK 1: negative effect for TEST, BEHAV, CLASS and positive on LAB
# RANK 2: no effect for TEST and CLASS, positive for LAB, negative on BEHAV
# RANK 3: no effect for TEST, negative for BEHAV and positive for LAB and CLASS
# Only RANK 3, the more adapted, benefit more with the 4CID methodology
# RANK 1 and 2, the less adapted benefit only on the LAB variable;

# Conclusion GENDER
# Male for Test no impact; female positive impact
# Both benefit in LAB variable
# In the BEHAV variable negative impact for MALE and no impact for FEMALE
# positive impact on female Class variable but no impact for the male
# Female, averall, benefit more

# Figure 16: overview inference graph analysis results 

# Resume results table to present them in a more readable table
results <- data.frame(
  samples=c("GLOBAL","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"),
  TEST1=c(54,0,54,0,-34,0,0,54,0),
  TEST2=c(58,0,58,0,-29,0,0,56,0),
  LAB1=c(53,56,55,44,26,51,73,52,53),
  LAB2=c(66,74,68,66,35,62,80,70,72),
  BEHAV1=c(-81,-77,-85,80,-65,-80,-94,0,-81),
  BEHAV2=c(-74,-71,-60,85,-50,-67,-90,0,-66),
  CLASS1=c(59,0,0,55,-39, 0,77,59,0),
  CLASS2=c(62,0,0,64,-37, 0,81,67,0),
  SAMPLE=c(1,1,1,1,1,1,1,1,1)
)
results
?geom_errorbar
ggplot(results) +
  geom_errorbar( aes(x=factor(samples, levels = c("GLOBAL","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE")), ymin=TEST1, ymax=TEST2), colour="orange",width=0.2,alpha=0.9, size=1) +
  geom_errorbar( aes(x=samples, ymin=LAB1, ymax=LAB2), width=0.4, colour="red", alpha=0.9, size=1) +
  geom_errorbar( aes(x=samples, ymin=BEHAV1, ymax=BEHAV2), width=0.4, colour="green", alpha=0.9, size=1) +
  geom_errorbar( aes(x=samples, ymin=CLASS1, ymax=CLASS2), width=0.4, colour="blue", alpha=0.9, size=1) +
  coord_flip()


## NOTES and OBSERVATIONS

## Policy implications

## Future research

## Tie loose hands together

########## doubts and discussions
# 802!?!?!? - scale change
#Kendall correlation test, sometimes because of the random sample it gives results of depency...
#what to ? a for cycle and calcula probability of getting independency?
#rank 1, kendall ties!?

########## REFERENCES

#INTERNET
# STATS: https://courses.edx.org/dashboard/programs/71c95834-f6df-4f78-8cd7-c6461dd9b1d4/
# ggplot http://rafalab.dfci.harvard.edu/dsbook/ggplot2.html
# ggplot https://www.youtube.com/watch?v=1GmQ5BdAhG4 
# normal distrubution https://www.kdnuggets.com/2018/07/explaining-68-95-99-7-rule-normal-distribution.html
# normal distrb. plot https://steemit.com/programming/@dkmathstats/creating-normal-distribution-plots-with-r-programming
# CLT http://genomicsclass.github.io/book/pages/clt_and_t-distribution.html
# t-test http://genomicsclass.github.io/book/pages/t-tests_in_practice.html
# graphics ggplot https://r4ds.had.co.nz/graphics-for-communication.html
# graphics ggplot https://ggplot2.tidyverse.org/
# non-parametric regression http://users.stat.umn.edu/~helwig/notes/smooth-notes.html#need-for-np-regression
# Histogram with kernel density estimation https://r-charts.com/distribution/histogram-density-ggplot2/?utm_content=cmp-true
# Exploratory Data Analysis http://genomicsclass.github.io/book/pages/exploratory_data_analysis.html 
# Rstudio and github https://resources.github.com/github-and-rstudio/ 
# ggplot2 cheat sheet: https://github.com/rstudio/cheatsheets/blob/main/data-visualization.pdf
# ggplot2: https://ggplot2.tidyverse.org/ 
# grid graphics: https://bookdown.org/rdpeng/RProgDA/the-grid-package.html
# Independent samples and hypothesis testing: https://datatab.net/tutorial/dependent-and-independent-samples
# non-parametric assumptions: https://bookdown.org/mandyyao98/bookdown-demo-master/lecture-9-non-parametric-methods.html
# correlation test: https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/#correlation-test
# Wilcoxon Rank Sum and Signed Rank Tests https://stat.ethz.ch/R-manual/R-devel/library/stats/html/wilcox.test.html
# Mann–Whitney U test https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test#Assumptions_and_formal_statement_of_hypotheses
# Non parametric tests: https://bookdown.org/siju_swamy/Stat_Lab/correlation-and-regression-analysis-in-r.html#correlation-analysis 
# Regression tools: https://ds4ps.org/pe4ps-textbook/docs/index.html
# Regression: https://stats.stackexchange.com/questions/381158/getting-equation-for-lm-ggplot-geom-smooth-with-multiple-levels
# Regression: https://www.datacamp.com/tutorial/linear-regression-R?utm_source=google&utm_medium=paid_search&utm_campaignid=20068300784&utm_adgroupid=152527239790&utm_device=c&utm_keyword=&utm_matchtype=&utm_network=g&utm_adpostion=&utm_creative=657040117338&utm_targetid=dsa-1947282172981&utm_loc_interest_ms=&utm_loc_physical_ms=1011751&utm_content=dsa~page~community-tuto&utm_campaign=230119_1-sea~dsa~tutorials_2-b2c_3-s-eu_4-prc_5-na_6-na_7-le_8-pdsh-go_9-na_10-na_11-na-mayftyc23&gclid=EAIaIQobChMIkI64lYGB_wIVA5xoCR0rngkIEAAYASAAEgInF_D_BwE
# RDD: https://rpubs.com/phle/r_tutorial_regression_discontinuity_design
# RDD: https://bookdown.org/carillitony/bailey/chp11.html
# RDD: https://ds4ps.org/pe4ps-textbook/docs/p-060-reg-discontinuity.html

# ARTICLES

Van Merrienboer, Jeroen J. G. & Clark, Richard & Croock, Marcel. (2002). Blueprints for complex learning: The 4C/ID-model. Educational Technology Research and Development. 50. 39-61. 10.1007/BF02504993. This article provides an overview description of the four-component instructional design system (4C/ID-model) developed originally
by van Merriënboer and others in the early 1990s (van Merriënboer, Jelsma, & Paas, 1992) for the design of training programs
for complex skills. It discusses the structure of training blueprints for complex learning and associated instructional methods.
The basic claim is that four interrelated components are essential in blueprints for complex learning: (a) learning tasks,
(b) supportive information, (c) just-in-time (JIT) information, and (d) part-task practice. Instructional methods for each
component are coupled to the basic learning processes involved in complex learning and a fully worked-out example of a training
blueprint for “searching for literature” is provided. Readers who benefit from a structured advance organizer should consider
reading the appendix at the end of this article before reading the entire article.

??? Van Merriënboer, J. J. G., & Kester, L. (2008). Whole-task models in education. 
In J. M. Spector, M. D. Merrill, J. J. G. van Merriënboer, & M. P. Driscoll (Eds.), 
Handbook of research on educational communications and technology (3rd ed, pp. 441–456). 
Mahwah, NJ: Erlbaum/Routledge.


# BOOKS

blueprint 4cdi
stats...


