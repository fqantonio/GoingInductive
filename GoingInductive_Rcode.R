#INDEX
# GOALS - 45
# HYPOTHESIS - 54
# DATA - Exploratory data analysis (EDA) - 57
  # DATA summary - 93
  # Resume - 315
  # DATA variables description - 352
  # DATA structure - 367
  # NORMALITY - 551
# RESULTS - 729
  ## Global Inference - 732
  ## Differences between junior school 0 and 1, without 4CID - 838
  ## school effect - 929
  ## School 1, global sample (include junior high for school 1) - 1023
  ## Computed sample junior with and without 4C/ID, between school 0 and school 1 (with 4C/ID) - 1097
  ## Inference for computed samples
      ### junior differences with and without 4C/ID, between school 0 and school 1 (only with 4C/ID) - 1177
      ### global sample differences with and without 4CID - 1274
  ## Inference junior and junior high, school 1 - 1367
  ## Junior sample for school 1 only - 1492
  ## Inference SCHOOL 1, Grade 7  - 1612
  ## Inference SCHOOL 1, Grade 8 - 1730
  ## Inference SCHOOL 1, Grade 9 -1847
  ## RANK - 1982
  ## GENDER - 2358
# REGRESSION ANALYSIS - 2546
# Regression discontinuity design (RDD) - 2668
# SUMMARY results and observations/notes - 2458
# REFERENCES - 2854

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
# The **hypothesis** is that this change will have a positive impact in all learning areas researched for samples included in this work, because, in overall, it bases on the 4C/ID methodology: that in itself is anchored on Brain Based Learning (BBL), Cognitive Load Theory (CLT), Multi-Intelligence Theory (MIT) and Multimedia Learning (ML).

######### DATA - exploratory data analysis - EDA

#AT this point I will do an exploratory data analysis (EDA)
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
# sample differences between school 0 and school 1
summary(data)
# Median and average very close, seems to have some normality structure, except for the BEHAV variable
length(data$ID)
length(which(data$SCHOOL=="0"))
length(which(data$SCHOOL=="0"))/length(data$ID)
length(which(data$SCHOOL=="1"))
length(which(data$SCHOOL=="1"))/length(data$ID)
# school 0, 476, 34% and school 1, 938, 66%

sd(data$TEST)
sd(data$LAB)
sd(data$BEHAV)
sd(data$CLASS)

data_M4CID0<- filter(data,M4CID=="0") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_M4CID0$ID)
summary(data_M4CID0)
# ID             DATE          SCHOOL          GENDER         
# Min.   :  1.0   Min.   :2003   Min.   :0.0000   Length:828        
# 1st Qu.:207.8   1st Qu.:2004   1st Qu.:0.0000   Class :character  
# Median :414.5   Median :2006   Median :0.0000   Mode  :character  
# Mean   :414.5   Mean   :2007   Mean   :0.4251                     
# 3rd Qu.:621.2   3rd Qu.:2010   3rd Qu.:1.0000                     
# Max.   :828.0   Max.   :2011   Max.   :1.0000                     
# TEST             LAB             BEHAV            GRADE      
# Min.   : 16.00   Min.   :  0.00   Min.   : 28.00   Min.   :0.000  
# 1st Qu.: 50.00   1st Qu.: 44.00   1st Qu.: 68.00   1st Qu.:0.000  
# Median : 62.00   Median : 60.00   Median : 79.00   Median :1.000  
# Mean   : 62.29   Mean   : 57.06   Mean   : 77.44   Mean   :1.214  
# 3rd Qu.: 75.00   3rd Qu.: 71.25   3rd Qu.: 88.00   3rd Qu.:2.000  
# Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :6.000  
# M4CID       CLASS      
# Min.   :0   Min.   :14.20  
# 1st Qu.:0   1st Qu.:53.98  
# Median :0   Median :63.70  
# Mean   :0   Mean   :63.75  
# 3rd Qu.:0   3rd Qu.:73.40  
# Max.   :0   Max.   :97.10 

sd(data_M4CID0$TEST)
sd(data_M4CID0$LAB)
sd(data_M4CID0$BEHAV)
sd(data_M4CID0$CLASS)

data_M4CID1<- filter(data,M4CID=="1") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_M4CID1$ID)
summary(data_M4CID1)
# ID              DATE          SCHOOL     GENDER         
# Min.   : 829.0   Min.   :2012   Min.   :1   Length:586        
# 1st Qu.: 975.2   1st Qu.:2013   1st Qu.:1   Class :character  
# Median :1121.5   Median :2016   Median :1   Mode  :character  
# Mean   :1121.5   Mean   :2015   Mean   :1                     
# 3rd Qu.:1267.8   3rd Qu.:2017   3rd Qu.:1                     
# Max.   :1414.0   Max.   :2017   Max.   :1                     
# TEST             LAB             BEHAV            GRADE      
# Min.   :  0.00   Min.   : 15.00   Min.   : 27.00   Min.   :0.000  
# 1st Qu.: 46.00   1st Qu.: 52.00   1st Qu.: 64.00   1st Qu.:1.000  
# Median : 57.00   Median : 65.00   Median : 76.00   Median :2.000  
# Mean   : 58.49   Mean   : 63.96   Mean   : 74.21   Mean   :2.549  
# 3rd Qu.: 70.00   3rd Qu.: 76.00   3rd Qu.: 86.00   3rd Qu.:5.000  
# Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :6.000  
# M4CID       CLASS       
# Min.   :1   Min.   : 17.60  
# 1st Qu.:1   1st Qu.: 53.62  
# Median :1   Median : 61.90  
# Mean   :1   Mean   : 63.28  
# 3rd Qu.:1   3rd Qu.: 72.35  
# Max.   :1   Max.   :100.00 

sd(data_M4CID1$TEST)
sd(data_M4CID1$LAB)
sd(data_M4CID1$BEHAV)
sd(data_M4CID1$CLASS)

data_SCHOOL0<- filter(data,SCHOOL=="0") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_SCHOOL0$ID)
summary(data_SCHOOL0)
# ID             DATE          SCHOOL     GENDER               TEST            LAB        
# Min.   :  1.0   Min.   :2003   Min.   :0   Length:476         Min.   :28.00   Min.   : 10.00  
# 1st Qu.:119.8   1st Qu.:2003   1st Qu.:0   Class :character   1st Qu.:56.00   1st Qu.: 52.00  
# Median :238.5   Median :2005   Median :0   Mode  :character   Median :68.00   Median : 61.50  
# Mean   :238.5   Mean   :2005   Mean   :0                      Mean   :67.33   Mean   : 62.36  
# 3rd Qu.:357.2   3rd Qu.:2006   3rd Qu.:0                      3rd Qu.:78.00   3rd Qu.: 76.00  
# Max.   :476.0   Max.   :2008   Max.   :0                      Max.   :97.00   Max.   :100.00  
# BEHAV            GRADE           M4CID       CLASS      
# Min.   : 37.00   Min.   :0.000   Min.   :0   Min.   :30.70  
# 1st Qu.: 66.00   1st Qu.:0.000   1st Qu.:0   1st Qu.:58.50  
# Median : 77.00   Median :1.000   Median :0   Median :68.50  
# Mean   : 76.06   Mean   :1.092   Mean   :0   Mean   :67.58  
# 3rd Qu.: 85.00   3rd Qu.:2.000   3rd Qu.:0   3rd Qu.:76.78  
# Max.   :100.00   Max.   :2.000   Max.   :0   Max.   :96.40 

sd(data_SCHOOL0$TEST)
sd(data_SCHOOL0$LAB)
sd(data_SCHOOL0$BEHAV)
sd(data_SCHOOL0$CLASS)

data_SCHOOL1<- filter(data,SCHOOL=="1") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_SCHOOL1$ID)
summary(data_SCHOOL1)
# ID            DATE          SCHOOL     GENDER               TEST             LAB        
# Min.   : 477   Min.   :2009   Min.   :1   Length:1149        Min.   :  0.00   Min.   :  0.00  
# 1st Qu.: 764   1st Qu.:2011   1st Qu.:1   Class :character   1st Qu.: 45.00   1st Qu.: 45.00  
# Median :1051   Median :2014   Median :1   Mode  :character   Median : 56.00   Median : 60.00  
# Mean   :1051   Mean   :2014   Mean   :1                      Mean   : 57.06   Mean   : 57.75  
# 3rd Qu.:1338   3rd Qu.:2017   3rd Qu.:1                      3rd Qu.: 70.00   3rd Qu.: 71.00  
# Max.   :1625   Max.   :2019   Max.   :1                      Max.   :100.00   Max.   :100.00  
# BEHAV            GRADE           M4CID            CLASS       
# Min.   :  0.00   Min.   :0.000   Min.   :0.0000   Min.   :  0.00  
# 1st Qu.: 65.00   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.: 50.80  
# Median : 76.00   Median :2.000   Median :1.0000   Median : 60.10  
# Mean   : 74.49   Mean   :2.427   Mean   :0.6936   Mean   : 60.75  
# 3rd Qu.: 87.00   3rd Qu.:5.000   3rd Qu.:1.0000   3rd Qu.: 70.70  
# Max.   :100.00   Max.   :6.000   Max.   :1.0000   Max.   :100.00  

sd(data_SCHOOL1$TEST)
sd(data_SCHOOL1$LAB)
sd(data_SCHOOL1$BEHAV)
sd(data_SCHOOL1$CLASS)

# Global sample Gender
length(which(data$GENDER=="F"))
length(which(data$GENDER=="F"))/length(data$ID)
length(which(data$GENDER=="M"))
length(which(data$GENDER=="M"))/length(data$ID)
# female/male sample: 602/812, 43%/57%: no balanced GENDER

#with and without 4CID
length(data$ID)
length(which(data$M4CID=="0"))
length(which(data$M4CID=="0"))/length(data$ID)
length(which(data$M4CID=="1"))
length(which(data$M4CID=="1"))/length(data$ID)
# sample without 4CID: 828, 59%; with 4CID: 586, 41%: more data with 4CID methodology, not balanced

# grade distribution
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
# Grade 0 (7th) - 353 (25%)
# Grade 1 (8th) - 411 (29%)
# Grade 2 (9th) - 396 (28%)
# Grade 3 (10th) - 27 (2%)
# Grade 4 (11th) - 18 (1%)
# Grade 5 (10technical) - 117 (8%)
# Grade 6 (11technical) - 93 (7%)
# very low sample entries levels 10/11 grade general and techinal edu path

# junior school, 7 to 9th grade
data_JUNIOR <- filter(data,GRADE <="2") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_JUNIOR$ID)
summary(data_JUNIOR)
length(which(data_JUNIOR$SCHOOL=="0"))
length(which(data_JUNIOR$SCHOOL=="0"))/length(data_JUNIOR$ID)
length(which(data_JUNIOR$SCHOOL=="1"))
length(which(data_JUNIOR$SCHOOL=="1"))/length(data_JUNIOR$ID)
data_JUNIOR_SCHOOL1 <- filter(data_JUNIOR,SCHOOL =="1") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(which(data_JUNIOR_SCHOOL1$M4CID=="1"))
length(which(data_JUNIOR_SCHOOL1$M4CID=="1"))/length(data_JUNIOR$ID)
length(which(data_JUNIOR_SCHOOL1$M4CID=="0"))
length(which(data_JUNIOR_SCHOOL1$M4CID=="0"))/length(data_JUNIOR$ID)
# Junior 1159; SCHOOL 0, M4CID = 1, - 476 (41%);
# SCHOOL 1 - SCHOOL 1: 683 (59%); M4CID = 0: 318 (27%) ; M4CID = 1: 365 (31%)

#Gender Junior
length(data_JUNIOR$ID)
length(which(data_JUNIOR$GENDER=="F"))
length(which(data_JUNIOR$GENDER=="F"))/length(data_JUNIOR$ID)
length(which(data_JUNIOR$GENDER=="M"))
length(which(data_JUNIOR$GENDER=="M"))/length(data_JUNIOR$ID)
# Gender Junior: total, 1159; female 576 (49%); male 583 (50%)

#data length high junior, secondary (sec) grades ()
lsec<-length(which(data$GRADE==3))+length(which(data$GRADE==4))+length(which(data$GRADE==5))+length(which(data$GRADE==6))
lsec
lsec/length(data$ID)
# sample entries secondary 255, 18% of the global sample
#it seems good to use secondary grade data for all sample inference
data_sec <- filter(data,GRADE >="3" & GRADE <="6") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
summary(data_sec)
# ID              DATE          SCHOOL     GENDER         
# Min.   : 578.0   Min.   :2009   Min.   :1   Length:255        
# 1st Qu.: 982.5   1st Qu.:2013   1st Qu.:1   Class :character  
# Median :1216.0   Median :2016   Median :1   Mode  :character  
# Mean   :1135.5   Mean   :2015   Mean   :1                     
# 3rd Qu.:1350.5   3rd Qu.:2017   3rd Qu.:1                     
# Max.   :1414.0   Max.   :2017   Max.   :1                     
# TEST             LAB             BEHAV            GRADE      
# Min.   :  0.00   Min.   :  0.00   Min.   : 27.00   Min.   :3.000  
# 1st Qu.: 47.00   1st Qu.: 47.00   1st Qu.: 65.00   1st Qu.:5.000  
# Median : 54.00   Median : 63.00   Median : 76.00   Median :5.000  
# Mean   : 55.74   Mean   : 61.02   Mean   : 73.24   Mean   :5.082  
# 3rd Qu.: 65.50   3rd Qu.: 73.00   3rd Qu.: 83.00   3rd Qu.:6.000  
# Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :6.000  
# M4CID            CLASS       
# Min.   :0.0000   Min.   : 17.60  
# 1st Qu.:1.0000   1st Qu.: 51.65  
# Median :1.0000   Median : 60.10  
# Mean   :0.8667   Mean   : 60.82  
# 3rd Qu.:1.0000   3rd Qu.: 69.30  
# Max.   :1.0000   Max.   :100.00  
length(data_sec$ID)
length(which(data_sec$M4CID==0))
length(which(data_sec$M4CID==0))/length(data_sec$ID)
length(which(data_sec$M4CID==1))
length(which(data_sec$M4CID==1))/length(data_sec$ID)
# obs 255
# Higher Junior (secondary), no 4CID, 34, 13%;
# Higher Junior (secondary), 4CID, 221, 86%
# poorly balanced

########## Resume
# The student data came from two different school: until 2008 school 0, after that, school 1, from 2009 until 2017;
# School 0 has only junior grades, 7 to 9th grade
# School 1 has junior grades and junior high with general courses and professional/technical coourses
# Male is represented by M and female by F
# with 4CID, variable M4CID = 1, otherwise M4CID = 0;
# Grades 7,8,9, 10, 11, 10P (technical), 11P (Technical) are categorizes by 0 to 6, respectively;
# Total sample data length 1414;
# school 0, 476, 34% and school 1, 938, 66%
# female/male sample: 602/812, 43%/57%: no balanced GENDER
# sample without 4CID: 828, 59%; with 4CID: 586, 41%: more data with 4CID methodology, not balanced
# Grade 0 (7th) - 353 (25%)
# Grade 1 (8th) - 411 (29%)
# Grade 2 (9th) - 396 (28%)
# Grade 3 (10th) - 27 (2%)
# Grade 4 (11th) - 18 (1%)
# Grade 5 (10technical) - 117 (8%)
# Grade 6 (11technical) - 93 (7%)
# very low sample entries levels 10/11 grade general and techinal edu path
# Junior 1159; SCHOOL 0, M4CID = 0, - 476 (41%);
# SCHOOL 1 - SCHOOL 1: 683 (59%); M4CID = 0: 318 (27%) ; M4CID = 1: 365 (31%)
# Gender Junior: total, 1159; female 576 (49%); male 583 (50%)
# sample entries secondary 255, 18% of the global sample
#it seems good to use secondary grade data for all sample inference
# obs 255
# Higher Junior (secondary), no 4CID, 34, 13%;
# Higher Junior (secondary), 4CID, 221, 86%
# poorly balanced

# Table 1: variable description
# Table 2: distribution by grade 
# Table 3: schools descriptive data sample
# Table 4: data from the treatment and control group

# Conclusion: From this analysis it seems to be possible comparing not only all the sample but also,
# just the Junior: this sample is more balanced in terms of grades and gender. The secondary is not balanced but it\
# can be used fro global sample inference. Junior sample its a better one.

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

#Table 1: Variable description

##########DATA structure

# graph analysis, all grades, global sample

# histograms and QQ plots
p1<-ggplot(data,aes(TEST,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p2<-ggplot(data,aes(LAB,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p3<-ggplot(data,aes(BEHAV,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
p4<-ggplot(data,aes(CLASS,after_stat(density))) + geom_histogram(binwidth=10,colour = 1, fill = "white") + geom_density(lwd = 1, colour = 4,fill = 2, alpha = 0.50)
# figure 1: Histograms with kernel density estimate for TEST, LAB, BEHAV and CLASS variables 
grid.arrange(p1, p2, p3, p4,  nrow = 2, top="Normality analysis on graphs",bottom="Figure 1: Histograms with kernel density estimate for variables TEST, LAB, BEHAV and CLASS")

# box plots
#global 2003 to 2017 global variable boxplots
summary(data)
p5<-ggplot(data,aes(DATE,TEST)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=TEST),se=FALSE)+ geom_vline(xintercept=2009,col="red")+geom_vline(xintercept=2012,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p6<-ggplot(data,aes(DATE,LAB)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=LAB),se=FALSE)+ geom_vline(xintercept=2009,col="red")+geom_vline(xintercept=2012,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p7<-ggplot(data,aes(DATE,BEHAV)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=BEHAV),se=FALSE)+ geom_vline(xintercept=2009,col="red")+geom_vline(xintercept=2012,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p8<-ggplot(data,aes(DATE,CLASS)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=CLASS),se=FALSE)+geom_vline(xintercept=2009,col="red")+geom_vline(xintercept=2012,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
# figure 2: Boxplot time series for TEST, LAB, BEHAV and CLASS variables  
grid.arrange(p5, p6, p7, p8,  nrow = 2, top="Boxplot time series",bottom="Figure 2: all data sample")

# Junior, school 0 and 1
data_junior<-filter(data,GRADE <="2") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
p9<-ggplot(data_junior,aes(DATE,TEST)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=TEST),se=FALSE)+ geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p10<-ggplot(data_junior,aes(DATE,LAB)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=LAB),se=FALSE)+ geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=5,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p11<-ggplot(data_junior,aes(DATE,BEHAV)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=BEHAV),se=FALSE)+ geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=25,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
p12<-ggplot(data_junior,aes(DATE,CLASS)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=CLASS),se=FALSE)+geom_vline(xintercept=2008,col="red")+geom_vline(xintercept=2013,col="blue")+geom_text(x=2003+1,y=25,label="School 0",col="black")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015+2,y=5,label="4C/ID Methodology",col="blue")
# figure 3: Boxplot time series for TEST, LAB, BEHAV and CLASS variables junior grade  
grid.arrange(p9, p10, p11, p12,  nrow = 2, top="Boxplot time series",bottom="Figure 3: data sample junior grade")

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
p17<-ggplot(data_junior_SCHOOL1,aes(DATE,TEST)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=TEST),se=FALSE)+geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=15,label="School 1",col="red")+geom_text(x=2015,y=15,label="4C/ID Methodology",col="blue")
p18<-ggplot(data_junior_SCHOOL1,aes(DATE,LAB)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=LAB),se=FALSE)+ geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=5,label="School 1",col="red")+geom_text(x=2015,y=5,label="4C/ID Methodology",col="blue")
p19<-ggplot(data_junior_SCHOOL1,aes(DATE,BEHAV)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=BEHAV),se=FALSE)+geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=30,label="School 1",col="red")+geom_text(x=2015,y=30,label="4C/ID Methodology",col="blue")
p20<-ggplot(data_junior_SCHOOL1,aes(DATE,CLASS)) + geom_boxplot(aes(group=DATE))+geom_smooth(aes(x=DATE,y=CLASS),se=FALSE)+geom_vline(xintercept=2013,col="blue")+geom_text(x=2009+1,y=20,label="School 1",col="red")+geom_text(x=2015,y=20,label="4C/ID Methodology",col="blue")
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
# figure 7: boxplots grouped by 4C/ID methodology showing the mean
grid.arrange(p28, p29, p30, p31, nrow = 2, top="All sample boxplots" , bottom = "Figure 7: boxplots grouped by 4C/ID methodology showing the mean")

# conclusion: 
# It seems that the 4CID effect is not positive when considering all sample, except for the variable LAB.
# it seems that school 0 as better TEST grades them school 2, while there a jump when the 4CID is implemented;
# That is not so obvious for the LAB variable. Clearly that the BEHAV variable suffers a loss with the implementation of 4CID. 
# for the CLASS variable, after a positive effect it goes down, perhaps following the BEHAV variable
# Only the LAB variable show an increase. The red dot and the skewness showed in figure 7, 
# heavy tails showed by the sd table nubers; mean and median are differnt, sligtly.
# show that could be no positive overall 4C/ID effect and the sample are not normal, which corroborate 
# upper analysis

# Scatterplots and Correlations
# time series regression and Local Polynomial Regression Fitting
p32<-ggplot(data,aes(DATE, TEST)) + geom_point() + geom_smooth(se=FALSE,color="blue") + geom_smooth(method=lm,se=FALSE,color="red",linetype=2)+geom_vline(xintercept=2012,col="green",linetype=2)+ geom_vline(xintercept=2009,col="green",linetype=2)+geom_text(x=2005,y=10,label="SCHOOL 0")+geom_text(x=2009+2,y=10,label="SCHOOL 1",col="red")+geom_text(x=2013+2,y=10,label="4C/ID",col="red")
summary(lm(DATE~TEST,data))$adj.r.squared
summary(lm(DATE~TEST,data))
# R^2=0.05
plot(lm(DATE~TEST,data))
# residuals: not normal
p33<-ggplot(data,aes(DATE,LAB)) + geom_point() + geom_smooth(se=FALSE,color="blue") + geom_smooth(method=lm,se=FALSE,color="red",linetype=2)+geom_vline(xintercept=2012,col="green",linetype=2)+ geom_vline(xintercept=2009,col="green",linetype=2)+geom_text(x=2005,y=10,label="SCHOOL 0")+geom_text(x=2009+2,y=10,label="SCHOOL 1",col="red")+geom_text(x=2013+2,y=10,label="4C/ID",col="red")
summary(lm(DATE~LAB,data))$adj.r.squared
summary(lm(DATE~LAB,data))
# R^2= 0.002
plot(lm(DATE~LAB,data))
# residuals: not normal
p34<-ggplot(data,aes(DATE, BEHAV)) + geom_point() + geom_smooth(se=FALSE,color="blue") + geom_smooth(method=lm,se=FALSE,color="red",linetype=2)+geom_vline(xintercept=2012,col="green",linetype=2)+ geom_vline(xintercept=2009,col="green",linetype=2)+geom_text(x=2005,y=30,label="SCHOOL 0")+geom_text(x=2009+2,y=30,label="SCHOOL 1",col="red")+geom_text(x=2013+2,y=30,label="4C/ID",col="red")
summary(lm(DATE~BEHAV,data))$adj.r.squared
summary(lm(DATE~BEHAV,data))
# R^2= 0.02
plot(lm(DATE~BEHAV,data))
# residuals: not normal
p35<-ggplot(data,aes(DATE, CLASS)) + geom_point() + geom_smooth(se=FALSE,color="blue") + geom_smooth(method=lm,se=FALSE,color="red",linetype=2)+geom_vline(xintercept=2012,col="green",linetype=2)+ geom_vline(xintercept=2009,col="green",linetype=2)+geom_text(x=2005,y=20,label="SCHOOL 0")+geom_text(x=2009+2,y=20,label="SCHOOL 1",col="red")+geom_text(x=2013+2,y=20,label="4C/ID",col="red")
summary(lm(DATE~CLASS,data))$adj.r.squared
summary(lm(DATE~CLASS,data))
# R^2 = 0.02
plot(lm(DATE~CLASS,data))
# residuals: not normal
# Figure 8: Local Polynomial Regression Fitting in blue
grid.arrange(p32, p33, p34, p35,  nrow = 2, top="Time series regression scatterplots", bottom="Figure 8: Local Polynomial Regression Fitting in blue")

#Conclusion: overall, over time, it seems that there is a negative 4C/ID effect.  
#However, R^2 values are very weak. It can be seen that a school effect could be 
#underway

#global data sample variable correlations
# first attempt to make the indeoendency analysis to the variables: just to check if there is a 
# the levell of dependency of the variables, or to see if the assessement was equal tecnhique
# If there is a correlation then variables are dependent, the inverse is not correct 
p36<-ggplot(data,aes(TEST, LAB)) + geom_point() + geom_smooth(method=lm,se=FALSE)+geom_text(x=30,y=0,label="R^2 = 0.24; tau = 35%",color = "blue")
summary(lm(TEST~LAB,data))$adj.r.squared
#R2=0.24
plot(lm(DATE~LAB,data))
#Not normal so use Kendall correlation test
cor.test(data$TEST, data$LAB,  method="kendall")
# Kendall's rank correlation tau
# data:  data$TEST and data$LAB
# z = 19.616, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.354927 
p37<-ggplot(data,aes(TEST, BEHAV)) + geom_point() + geom_smooth(method=lm)+geom_text(x=30,y=20,label="R^2 = 0.14; tau=26%",color = "blue")
summary(lm(TEST~BEHAV,data))$adj.r.squared
# R2=0.14
cor.test(data$TEST, data$BEHAV,  method="kendall")
# Kendall's rank correlation tau
# data:  data$TEST and data$BEHAV
# z = 14.324, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.2589172 
p38<-ggplot(data,aes(LAB, BEHAV)) + geom_point() + geom_smooth(method=lm)+geom_text(x=30,y=20,label="R^2 = 0.15; tau = 27%",color = "blue")
summary(lm(LAB~BEHAV,data))$adj.r.squared
#R2=0.15
cor.test(data$LAB, data$BEHAV,  method="kendall")
# Kendall's rank correlation tau
# data:  data$LAB and data$BEHAV
# z = 14.835, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.2689518 
p39<-ggplot(data,aes(TEST, CLASS)) + geom_point() + geom_smooth(method=lm)+geom_text(x=20,y=100,label="R^2 = 0.80; tau=72%",color = "blue")
summary(lm(TEST~CLASS,data))$adj.r.squared
#R2 0.80
cor.test(data$TEST, data$CLASS,  method="kendall")
# Kendall's rank correlation tau
# data:  data$TEST and data$CLASS
# z = 40.179, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.7195958 

#Figure 9: variable dependency analysis
grid.arrange(p36, p37, p38,p39,  nrow = 2,bottom="Figure 9: Variable dependency analysis", top="All sample variable correlations analysis")

# conclusion: these R2 low doesn't mean that the variables are dependent nor the 
# There is some degree of correlation that doesn't permit to conclude of the variable dependency
# The clear correlation between TEST and CLASS variable show that they are dependent and
# Kendal correlation test corroborates that variables are dependent but with a levell degree 
#arround 20 to 40%: not high; meaning that the way the variables are assessed are different. They correlate,
# but at a low levell, showing a bad levell, which means that they were assessed with different techniques.
# the class test correlation is just a refence that we know, in advance, that there is a math relationship

# Table 5: kendall's correlation

# 7,8,9
# limit the analysis to Junior grade: GRADE 0,1,2, meaning 7,8 and 9 grade both schools
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
#Figure 10: normality testing for the junior grade sample
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
# D = 0.99385,  p-value = 0.003534
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data$TEST
# W = 0.99013, p-value = 3.336e-07

hist(data$LAB)
p49<-ggplot(data, aes(sample = LAB)) + stat_qq() + stat_qq_line()+labs(y="LAB",x="Theoretical quantiles")
#qqnorm(data$LAB);qqline(data$LAB)
ks.test(data$LAB,dnorm(mean(data$LAB),sd(data$LAB)))
shapiro.test(data$LAB)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data$LAB and dnorm(mean(data$LAB), sd(data$LAB))
# D = 0.9901, p-value = 0.01908
# alternative hypothesis: two-sided
# # Shapiro-Wilk normality test
# data:  data$LAB
# W = 0.97869, p-value = 1.513e-13

hist(data$BEHAV)
p50<-ggplot(data, aes(sample = BEHAV)) + stat_qq() + stat_qq_line()+labs(y="BEHAV",x="Theoretical quantiles")
#qqnorm(data$BEHAV);qqline(data$BEHAV)
ks.test(data$BEHAV,dnorm(mean(data$BEHAV),sd(data$BEHAV)))
shapiro.test(data$BEHAV)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data$BEHAV and dnorm(mean(data$BEHAV), sd(data$BEHAV))
# D = 1, p-value = 0.0007067
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data$BEHAV
# W = 0.97422, p-value = 3.244e-15

hist(data$CLASS)
p51<-ggplot(data, aes(sample = CLASS)) + stat_qq() + stat_qq_line()+labs(y="CLASS",x="Theoretical quantiles")
#qqnorm(data$CLASS);qqline(data$CLASS)
ks.test(data$CLASS,dnorm(mean(data$CLASS),sd(data$CLASS)))
shapiro.test(data$CLASS)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data$CLASS and dnorm(mean(data$CLASS), sd(data$CLASS))
# D = 1, p-value = 0.001413
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data$CLASS
# W = 0.99587, p-value = 0.0006986

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
# D = 1, p-value = 0.002924
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$TEST
# W = 0.98652, p-value = 6.164e-06
hist(data_JUNIOR_SCHOOL1$LAB)
p57<-ggplot(data_JUNIOR_SCHOOL1, aes(sample = LAB)) + stat_qq() + stat_qq_line()+labs(y="LAB",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL1$LAB);qqline(data_JUNIOR_SCHOOL1$LAB)
ks.test(data_JUNIOR_SCHOOL1$LAB,dnorm(mean(data_JUNIOR_SCHOOL1$LAB),sd(data_JUNIOR_SCHOOL1$LAB)))
shapiro.test(data_JUNIOR_SCHOOL1$LAB)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$LAB and dnorm(mean(data_JUNIOR_SCHOOL1$LAB), sd(data_JUNIOR_SCHOOL1$LAB))
# D = 0.98097, p-value = 0.03947
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$LAB
# W = 0.97414, p-value = 1.264e-09
hist(data_JUNIOR_SCHOOL1$BEHAV)
p58<-ggplot(data_JUNIOR_SCHOOL1, aes(sample = BEHAV)) + stat_qq() + stat_qq_line()+labs(y="BEHAV",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL1$BEHAV);qqline(data_JUNIOR_SCHOOL1$BEHAV)
ks.test(data_JUNIOR_SCHOOL1$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1$BEHAV),sd(data_JUNIOR_SCHOOL0$BEHAV)))
shapiro.test(data_JUNIOR_SCHOOL1$BEHAV)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$BEHAV and dnorm(mean(data_JUNIOR_SCHOOL1$BEHAV), sd(data_JUNIOR_SCHOOL0$BEHAV))
# D = 1, p-value = 0.001462
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$BEHAV
#W = 0.9632, p-value = 4.577e-12
hist(data_JUNIOR_SCHOOL1$CLASS)
p59<-ggplot(data_JUNIOR_SCHOOL1, aes(sample = CLASS)) + stat_qq() + stat_qq_line()+labs(y="CLASS",x="Theoretical quantiles")
#qqnorm(data_JUNIOR_SCHOOL1$CLASS);qqline(data_JUNIOR_SCHOOL1$CLASS)
ks.test(data_JUNIOR_SCHOOL1$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1$CLASS),sd(data_JUNIOR_SCHOOL1$CLASS)))
shapiro.test(data_JUNIOR_SCHOOL1$CLASS)
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$CLASS and dnorm(mean(data_JUNIOR_SCHOOL1$CLASS), sd(data_JUNIOR_SCHOOL1$CLASS))
# D = 1, p-value = 0.002924
# alternative hypothesis: two-sided
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$CLASS
# W = 0.99145, p-value = 0.0005524

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

# RESULTS
#Inference (and independency analysis)
#Inference for the global differences in M4CID, no matter which school, for each data variables: TEST, LAB and BEHAV
head(data)
summary(data)
data_M4CID0<-filter(data,M4CID =="0") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
data_M4CID1<-filter(data,M4CID =="1") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
length(data_M4CID0$ID)#828
length(data_M4CID1$ID)#586
# Along the way, sample independency will be tested and non normality will be considered

#TEST
## Independency
sample1<-data.frame(sample(data_M4CID0$TEST,500))
sample2<-data.frame(sample(data_M4CID1$TEST,500))
#Kendall correlation test needs same sample length
cor.test(sample1$sample.data_M4CID0.TEST..500.,sample2$sample.data_M4CID1.TEST..500.,method = "kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_M4CID0.TEST..500. and sample2$sample.data_M4CID1.TEST..500.
# z = -0.69155, p-value = 0.4892
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.02102116  
# samples are independent, so its possible to use Wilcoxon inference test
ggplot(data) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_M4CID0$TEST)#62
median(data_M4CID1$TEST)#57
wilcox.test(data_M4CID0$TEST,data_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0$TEST and data_M4CID1$TEST
# W = 276677, p-value = 3.314e-06
# alternative hypothesis: true location shift is greater than 0

#LAB
sample1<-data.frame(sample(data_M4CID0$LAB,500))
sample2<-data.frame(sample(data_M4CID1$LAB,500))
#Kendall correlation test
cor.test(sample1$sample.data_M4CID0.LAB..500.,sample2$sample.data_M4CID1.LAB..500.,method = "kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_M4CID0.LAB..500. and sample2$sample.data_M4CID1.LAB..500.
# z = 0.78833, p-value = 0.4305
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau  0.02418545
# samples are independent, so its possible to use Wilcoxon inference test
ggplot(data) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_M4CID0$LAB)#60
median(data_M4CID1$LAB)#65
wilcox.test(data_M4CID0$LAB,data_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0$LAB and data_M4CID1$LAB
# W = 195470, p-value = 2.265e-10
# alternative hypothesis: true location shift is less than 0

#BEHAV
sample1<-data.frame(sample(data_M4CID0$BEHAV,500))
sample2<-data.frame(sample(data_M4CID1$BEHAV,500))
#Kendall's rank correlation tau
cor.test(sample1$sample.data_M4CID0.BEHAV..500.,sample2$sample.data_M4CID1.BEHAV..500.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_M4CID0.BEHAV..500. and sample2$sample.data_M4CID1.BEHAV..500.
# z = -0.71102, p-value = 0.4771
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.02170898
# samples are independent, so its possible to use Wilcoxon inference test
ggplot(data) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_M4CID0$BEHAV)#79
median(data_M4CID1$BEHAV)#76
wilcox.test(data_M4CID0$BEHAV,data_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0$BEHAV and data_M4CID1$BEHAV
# W = 268902, p-value = 0.000253
# alternative hypothesis: true location shift is greater than 0

#CLASS
sample1<-data.frame(sample(data_M4CID0$CLASS,500))
sample2<-data.frame(sample(data_M4CID1$CLASS,500))
#Kendall correlation test
cor.test(sample1$sample.data_M4CID0.CLASS..500.,sample2$sample.data_M4CID1.CLASS..500., method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_M4CID0.CLASS..500. and sample2$sample.data_M4CID1.CLASS..500.
# z = 0.8145, p-value = 0.4154
# alternative hypothesis: true tau is not equal to 0
# sample estimates:tau 0.02440767  
# samples are independent, so its possible to use Wilcoxon inference test
ggplot(data) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_M4CID0$CLASS)#64
median(data_M4CID1$CLASS)#62
wilcox.test(data_M4CID0$CLASS,data_M4CID1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0$CLASS and data_M4CID1$CLASS
# W = 252126, p-value = 0.104
# alternative hypothesis: true location shift is greater than 0

#Inference conclusion for the 4C/ID treatment effect for all sample, 
# no matter which school, for each data variables: TEST, LAB, BEHAV and CLASS
# Samples are independent and not. normal, Wilcoxon inference non-parametric test
# for the global TEST and BEHAV, negative 4CID effect
# no effect for CLASS variable;
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

#Inference for the differences in schools (control group), without 4CID, for each variable: TEST, LAB, BEHAV and CLASS
## DATA
summary(data)
data_M4CID0<-filter(data,data$M4CID=="0")  %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
summary(data_M4CID0)
length(which(data_M4CID0$SCHOOL=="0"))#476 entrances
length(which(data_M4CID0$SCHOOL=="1"))#352 entrances

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
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_M4CID0$TEST and dnorm(mean(data_M4CID0$TEST), sd(data_M4CID0$TEST))
# D = 1, p-value = 0.002413
# alternative hypothesis: two-sided
shapiro.test(data_M4CID0$TEST)
# Shapiro-Wilk normality test
# data:  data_M4CID0$TEST
# W = 0.98967, p-value = 1.442e-05
# p-value << 0,05 so, conclusion: not normal
# The independency is not measured because we are talking about two different schools in different towns
ggplot(data_M4CID0) + geom_boxplot(aes(y=TEST, x=factor(SCHOOL)))
median(data_M4CID0_SCHOOL0$TEST)#68
median(data_M4CID0_SCHOOL1$TEST)#54
wilcox.test(data_M4CID0_SCHOOL0$TEST,data_M4CID0_SCHOOL1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$TEST and data_M4CID0_SCHOOL1$TEST
# W = 117771, p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0

# LAB
hist(data_M4CID0$LAB)
qqnorm(data_M4CID0$LAB)
qqline(data_M4CID0$LAB)
ks.test(data_M4CID0$LAB,dnorm(mean(data_M4CID0$LAB),sd(data_M4CID0$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_M4CID0$LAB and dnorm(mean(data_M4CID0$LAB), sd(data_M4CID0$LAB))
# D = 0.98309, p-value = 0.03619
# alternative hypothesis: two-sided
shapiro.test(data_M4CID0$LAB)
# Shapiro-Wilk normality test
# data:  data_M4CID0$LAB
# W = 0.97381, p-value = 4.944e-11
#p-value << 0,05 so, conclusion: not normal
ggplot(data_M4CID0) + geom_boxplot(aes(y=LAB, x=factor(SCHOOL)))
median(data_M4CID0_SCHOOL0$LAB)#62
median(data_M4CID0_SCHOOL1$LAB)#51
wilcox.test(data_M4CID0_SCHOOL0$LAB,data_M4CID0_SCHOOL1$LAB,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$LAB and data_M4CID0_SCHOOL1$LAB
# W = 112552, p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0

# BEHAV
hist(data_M4CID0$BEHAV)
qqnorm(data_M4CID0$BEHAV)
qqline(data_M4CID0$BEHAV)
ks.test(data_M4CID0$BEHAV,dnorm(mean(data_M4CID0$BEHAV),sd(data_M4CID0$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_M4CID0$BEHAV and dnorm(mean(data_M4CID0$BEHAV), sd(data_M4CID0$BEHAV))
# D = 1, p-value = 0.001206
# alternative hypothesis: two-sided
shapiro.test(data_M4CID0$BEHAV)
# Shapiro-Wilk normality test
# data:  data_M4CID0$BEHAV
# W = 0.97675, p-value = 3.278e-10
# p-vaue << 0,05, so conclusion: not normal
ggplot(data_M4CID0) + geom_boxplot(aes(y=BEHAV, x=factor(SCHOOL)))
median(data_M4CID0_SCHOOL0$BEHAV)#77
median(data_M4CID0_SCHOOL1$BEHAV)#80
wilcox.test(data_M4CID0_SCHOOL0$BEHAV,data_M4CID0_SCHOOL1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$BEHAV and data_M4CID0_SCHOOL1$BEHAV
# W = 70972, p-value = 8.34e-05
# alternative hypothesis: true location shift is less than 0

# CLASS
hist(data_M4CID0$CLASS)
qqnorm(data_M4CID0$CLASS)
qqline(data_M4CID0$CLASS)
ks.test(data_M4CID0$CLASS,dnorm(mean(data_M4CID0$CLASS),sd(data_M4CID0$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_M4CID0$CLASS and dnorm(mean(data_M4CID0$CLASS), sd(data_M4CID0$CLASS))
# D = 1, p-value = 0.002413
# alternative hypothesis: two-sided

shapiro.test(data_M4CID0$CLASS)
# Shapiro-Wilk normality test
# data:  data_M4CID0$CLASS
# W = 0.99571, p-value = 0.02136
#p-value < 0,05, so conclusion: not normal
ggplot(data_M4CID0) + geom_boxplot(aes(y=CLASS, x=factor(SCHOOL)))
median(data_M4CID0_SCHOOL0$CLASS)#69
median(data_M4CID0_SCHOOL1$CLASS)#58
wilcox.test(data_M4CID0_SCHOOL0$CLASS,data_M4CID0_SCHOOL1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$CLASS and data_M4CID0_SCHOOL1$CLASS
# W = 115938, p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0

# table 10 : Inference for the differences in school

# Conclusion
# Variable TEST, LAB, CLASS greater in school 0 then in school 1
# Variable BEHAV is less for the School 0 then in the school 1
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
# In this sample, variables will be added with the differences computed before in order
# to acess if the two sample are from the same distribution: confrmation after school effect: are the same?
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

length(data_SCHOOL0$ID)#476
length(data_junior_computed_SCHOOL1_M4CID0$ID)#318

#TEST
median(data_SCHOOL0$TEST)#68
median(data_junior_computed_SCHOOL1_M4CID0$TEST)#66
wilcox.test(data_SCHOOL0$TEST,data_junior_computed_SCHOOL1_M4CID0$TEST,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL0$TEST and data_junior_computed_SCHOOL1_M4CID0$TEST
# W = 78009, p-value = 0.4629
# alternative hypothesis: true location shift is not equal to 0

#LAB
median(data_SCHOOL0$LAB)#62
median(data_junior_computed_SCHOOL1_M4CID0$LAB)#64
wilcox.test(data_SCHOOL0$LAB,data_junior_computed_SCHOOL1_M4CID0$LAB,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL0$LAB and data_junior_computed_SCHOOL1_M4CID0$LAB
# W = 75615, p-value = 0.9827
# alternative hypothesis: true location shift is not equal to 0

#BEHAV
median(data_SCHOOL0$BEHAV)#77
median(data_junior_computed_SCHOOL1_M4CID0$BEHAV)#78
hist(data_SCHOOL0$BEHAV)
hist(data_junior_computed_SCHOOL1_M4CID0$BEHAV)
wilcox.test(data_SCHOOL0$BEHAV,data_junior_computed_SCHOOL1_M4CID0$BEHAV,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL0$BEHAV and data_junior_computed_SCHOOL1_M4CID0$BEHAV
# W = 74502, p-value = 0.709
# alternative hypothesis: true location shift is not equal to 0

#CLASS
median(data_SCHOOL0$CLASS)#69
median(data_junior_computed_SCHOOL1_M4CID0$CLASS)#67
wilcox.test(data_SCHOOL0$CLASS,data_junior_computed_SCHOOL1_M4CID0$CLASS,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL0$CLASS and data_junior_computed_SCHOOL1_M4CID0$CLASS
# W = 77022, p-value = 0.6728
# alternative hypothesis: true location shift is not equal to 0

#Conclusion: yes, they are from the same sample distribution

# Inference for all sample with school effect
data_computed <- data
data_computed$TEST<-ifelse(data_computed$SCHOOL=="1",data_computed$TEST+dif_mean_TEST,data_computed$TEST)
data_computed$LAB<-ifelse(data_computed$SCHOOL=="1",data_computed$LAB+dif_mean_LAB,data_computed$LAB)
data_computed$CLASS<-ifelse(data_computed$SCHOOL=="1",data_computed$CLASS+dif_mean_CLASS,data_junior_computed$CLASS)
data_computed$BEHAV<-ifelse(data_computed$SCHOOL=="1",data_computed$BEHAV+dif_mean_BEHAV,data_junior_computed$BEHAV)

data_computed_M4CID0<-filter(data_computed,M4CID =="0") %>% select(ID,GENDER,BEHAV,M4CID,SCHOOL,GRADE,TEST,LAB, CLASS)
data_computed_M4CID1<-filter(data_computed,M4CID =="1") %>% select(ID,GENDER,BEHAV,M4CID,SCHOOL,GRADE,TEST,LAB, CLASS)
length(data_computed_M4CID0$ID)#828
length(data_computed_M4CID1$ID)#586

#TEST
summary(data_computed)
hist(data_computed$TEST)
qqnorm(data_computed$TEST)
qqline(data_computed$TEST)
ks.test(data_computed$TEST,dnorm(mean(data_computed$TEST),sd(data_computed$TEST)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$TEST and dnorm(mean(data_computed$TEST), sd(data_computed$TEST))
# D = 1, p-value = 0.0007067
# alternative hypothesis: two-sided
shapiro.test(data_computed$TEST)
# Shapiro-Wilk normality test
# data:  data_computed$TEST
# W = 0.99565, p-value = 0.0004377
#conclusion: not normal
sample1<-data.frame(sample(data_computed_M4CID0$TEST,500))
sample2<-data.frame(sample(data_computed_M4CID1$TEST,500))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_computed_M4CID0.TEST..500.,sample2$sample.data_computed_M4CID1.TEST..500.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_M4CID0.TEST..500. and sample2$sample.data_computed_M4CID1.TEST..500.
# z = -0.34307, p-value = 0.7315
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01039902 
# sample independent
ggplot(data_computed) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_M4CID0$TEST)#67
median(data_computed_M4CID1$TEST)#69
wilcox.test(data_computed_M4CID0$TEST,data_computed_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_M4CID0$TEST and data_computed_M4CID1$TEST
# W = 223254, p-value = 0.005261
# alternative hypothesis: true location shift is less than 0



# continuar com as restantes varíaveis !??!?!?!?!


# Inference for computed junior differences with and without 4C/ID, between school 0 and school 1
data_junior_computed_M4CID0<-filter(data_junior_computed,GRADE <= "2" & M4CID =="0") %>% select(ID,GENDER,BEHAV,M4CID,SCHOOL,GRADE,TEST,LAB, CLASS)
data_junior_computed_M4CID1<-filter(data_junior_computed,GRADE <= "2" & M4CID =="1") %>% select(ID,GENDER,BEHAV,M4CID,SCHOOL,GRADE,TEST,LAB, CLASS)
length(data_junior_computed_M4CID0$ID)#794
length(data_junior_computed_M4CID1$ID)#365

#TEST
summary(data_junior_computed)
hist(data_junior_computed$TEST)
qqnorm(data_junior_computed$TEST)
qqline(data_junior_computed$TEST)
ks.test(data_junior_computed$TEST,dnorm(mean(data_junior_computed$TEST),sd(data_junior_computed$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_junior_computed$TEST and dnorm(mean(data_junior_computed$TEST), sd(data_junior_computed$TEST))
# D = 1, p-value = 0.001724
# alternative hypothesis: two-sided
shapiro.test(data_junior_computed$TEST)
# Shapiro-Wilk normality test
# data:  data_junior_computed$TEST
# W = 0.99472, p-value = 0.0004287
#conclusion: not normal and the samples are indedependent considering that they are from differents schools
ggplot(data_junior_computed) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_M4CID0$TEST)#67
median(data_junior_computed_M4CID1$TEST)#70
wilcox.test(data_junior_computed_M4CID0$TEST,data_junior_computed_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_M4CID0$TEST and data_junior_computed_M4CID1$TEST
# W = 127454, p-value = 0.0004884
# alternative hypothesis: true location shift is less than 0

#LAB
summary(data_junior_computed)
hist(data_junior_computed$LAB)
qqnorm(data_junior_computed$LAB)
qqline(data_junior_computed$LAB)
ks.test(data_junior_computed$LAB,dnorm(mean(data_junior_computed$LAB),sd(data_junior_computed$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_junior_computed$LAB and dnorm(mean(data_junior_computed$LAB), sd(data_junior_computed$LAB))
# D = 1, p-value = 0.001724
# alternative hypothesis: two-sided
shapiro.test(data_junior_computed$LAB)
# Shapiro-Wilk normality test
# data:  data_junior_computed$LAB
# W = 0.98666, p-value = 8.433e-09

#conclusion: not normal and the samples are indedependent considering that they are from differents schools
ggplot(data_junior_computed) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_M4CID0$LAB)#63
median(data_junior_computed_M4CID1$LAB)#79
wilcox.test(data_junior_computed_M4CID0$LAB,data_junior_computed_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_M4CID0$LAB and data_junior_computed_M4CID1$LAB
# W = 77643, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

#BEHAV
summary(data_junior_computed)
hist(data_junior_computed$BEHAV)
qqnorm(data_junior_computed$BEHAV)
qqline(data_junior_computed$BEHAV)
ks.test(data_junior_computed$BEHAV,dnorm(mean(data_junior_computed$BEHAV),sd(data_junior_computed$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_junior_computed$BEHAV and dnorm(mean(data_junior_computed$BEHAV), sd(data_junior_computed$BEHAV))
# D = 1, p-value = 0.0008621
# alternative hypothesis: two-sided
shapiro.test(data_junior_computed$LAB)
# Shapiro-Wilk normality test
# data:  data_junior_computed$LAB
# W = 0.98666, p-value = 8.433e-09
#conclusion: not normal and the samples are indedependent considering that they are from differents schools
ggplot(data_junior_computed) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_M4CID0$BEHAV)#77
median(data_junior_computed_M4CID1$BEHAV)#73
wilcox.test(data_junior_computed_M4CID0$BEHAV,data_junior_computed_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_M4CID0$BEHAV and data_junior_computed_M4CID1$BEHAV
# W = 165710, p-value = 4.228e-05
# alternative hypothesis: true location shift is greater than 0

#CLASS
summary(data_junior_computed)
hist(data_junior_computed$CLASS)
qqnorm(data_junior_computed$CLASS)
qqline(data_junior_computed$CLASS)
ks.test(data_junior_computed$CLASS,dnorm(mean(data_junior_computed$CLASS),sd(data_junior_computed$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_junior_computed$CLASS and dnorm(mean(data_junior_computed$CLASS), sd(data_junior_computed$CLASS))
# D = 1, p-value = 0.001724
# alternative hypothesis: two-sided
shapiro.test(data_junior_computed$LAB)
# Shapiro-Wilk normality test
# data:  data_junior_computed$LAB
# W = 0.98666, p-value = 8.433e-09
#conclusion: not normal and the samples are indedependent considering that they are from differents schools
ggplot(data_junior_computed) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_M4CID0$CLASS)#68
median(data_junior_computed_M4CID1$CLASS)#72
wilcox.test(data_junior_computed_M4CID0$CLASS,data_junior_computed_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_M4CID0$CLASS and data_junior_computed_M4CID1$CLASS
# W = 112527, p-value = 4.763e-10
# alternative hypothesis: true location shift is less than 0

# Inference for computed junior differences with and without 4C/ID, between school 0 and school 1 (this school only with 4C/ID)
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
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_junior_computed$TEST and dnorm(mean(data_junior_computed$TEST), sd(data_junior_computed$TEST))
# D = 1, p-value = 0.001724
# alternative hypothesis: two-sided
shapiro.test(data_junior_computed$TEST)
# Shapiro-Wilk normality test
# data:  data_junior_computed$TEST
# W = 0.99472, p-value = 0.0004287
#conclusion: not normal and the samples are independent considering that they are from differents schools
ggplot(data_junior_computed) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_SCHOOL0_M4CID0$TEST)#68
median(data_junior_computed_SCHOOL1_M4CID1$TEST)#70
wilcox.test(data_junior_computed_SCHOOL0_M4CID0$TEST,data_junior_computed_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_SCHOOL0_M4CID0$TEST and data_junior_computed_SCHOOL1_M4CID1$TEST
# W = 77129, p-value = 0.002636
# alternative hypothesis: true location shift is less than 0

#LAB
summary(data_junior_computed)
hist(data_junior_computed$LAB)
qqnorm(data_junior_computed$LAB)
qqline(data_junior_computed$LAB)
ks.test(data_junior_computed$LAB,dnorm(mean(data_junior_computed$LAB),sd(data_junior_computed$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_junior_computed$LAB and dnorm(mean(data_junior_computed$LAB), sd(data_junior_computed$LAB))
# D = 1, p-value = 0.001724
# alternative hypothesis: two-sided
shapiro.test(data_junior_computed$LAB)
# Shapiro-Wilk normality test
# data:  data_junior_computed$LAB
# W = 0.98666, p-value = 8.433e-09
#conclusion: not normal and independent because different schools
ggplot(data_junior_computed) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_SCHOOL0_M4CID0$LAB)#62
median(data_junior_computed_SCHOOL1_M4CID1$LAB)#79
wilcox.test(data_junior_computed_SCHOOL0_M4CID0$LAB,data_junior_computed_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_M4CID0_SCHOOL0$LAB and data_junior_computed_M4CID1_SCHOOL1$LAB
# W = 44432, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
summary(data_junior_computed)
hist(data_junior_computed$BEHAV)
qqnorm(data_junior_computed$BEHAV)
qqline(data_junior_computed$BEHAV)
ks.test(data_junior_computed$BEHAV,dnorm(mean(data_junior_computed$BEHAV),sd(data_junior_computed$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_junior_computed$BEHAV and dnorm(mean(data_junior_computed$BEHAV), sd(data_junior_computed$BEHAV))
# D = 1, p-value = 0.0008621
# alternative hypothesis: two-sided
shapiro.test(data_junior_computed$BEHAV)
# Shapiro-Wilk normality test
# data:  data_junior_computed$BEHAV
# W = 0.97605, p-value = 6.305e-13
#conclusion: not normal and the samples are indedependent considering that they are from differents schools
ggplot(data_junior_computed) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_SCHOOL0_M4CID0$BEHAV)#77
median(data_junior_computed_SCHOOL1_M4CID1$BEHAV)#73
wilcox.test(data_junior_computed_SCHOOL0_M4CID0$BEHAV,data_junior_computed_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# # Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_SCHOOL0_M4CID0$BEHAV and data_junior_computed_SCHOOL1_M4CID1$BEHAV
# W = 98978, p-value = 0.0002622
# alternative hypothesis: true location shift is greater than 0

# CLASS
summary(data_junior_computed)
hist(data_junior_computed$CLASS)
qqnorm(data_junior_computed$CLASS)
qqline(data_junior_computed$CLASS)
ks.test(data_junior_computed$CLASS,dnorm(mean(data_junior_computed$CLASS),sd(data_junior_computed$CLASS)))
# data:  data_junior_computed$CLASS and dnorm(mean(data_junior_computed$CLASS), sd(data_junior_computed$CLASS))
# D = 1, p-value = 0.001724
# alternative hypothesis: two-sided
shapiro.test(data_junior_computed$CLASS)
# Shapiro-Wilk normality test
# data:  data_junior_computed$CLASS
# W = 0.9966, p-value = 0.01261
#conclusion: not normal
ggplot(data_junior_computed) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))#NOTE: this is exactly the same sample infered next because it contains data from SCHOOL1 and M4CID0
median(data_junior_computed_SCHOOL0_M4CID0$CLASS)#69
median(data_junior_computed_SCHOOL1_M4CID1$CLASS)#72
wilcox.test(data_junior_computed_SCHOOL0_M4CID0$CLASS,data_junior_computed_SCHOOL1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_computed_SCHOOL0_M4CID0$CLASS and data_junior_computed_SCHOOL1_M4CID1$CLASS
# W = 67685, p-value = 1.958e-08
# alternative hypothesis: true location shift is less than 0

# Inference conclusion for computed junior differences with and without 4C/ID, 
# between school 0 and school 1 (this school with only 4C/ID)
# negative on BEHAV
# Positive effect for LAB, TEST and CLASS

# Inference for computed junior differences between school 0 and global school 1 junior and junior high with 4CID
# Excluding School 1 with no 4C/ID
# use the calculus about the computed differences already made 

# see Table 11: Differences computed for junior school 0 and 1

# sample with only school 0 and school 1 junior and junior high with 4C/ID (exclude school 1 without 4C/ID)
data_computed<-filter(data,SCHOOL == "0" | (SCHOOL == "1" & M4CID == "1")) %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

# add to school 1
data_computed$TEST<-ifelse(data_computed$M4CID==1,data_computed$TEST+dif_mean_TEST,data_computed$TEST)
data_computed$LAB<-ifelse(data_computed$M4CID==1,data_computed$LAB+dif_mean_LAB,data_computed$LAB)
data_computed$CLASS<-ifelse(data_computed$M4CID==1,data_computed$CLASS+dif_mean_CLASS,data_computed$CLASS)
data_computed$BEHAV<-ifelse(data_computed$M4CID==1,data_computed$BEHAV+dif_mean_BEHAV,data_computed$BEHAV)

data_computed_SCHOOL0_M4CID0<-filter(data_computed,SCHOOL == "0" & M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_SCHOOL1_M4CID1<-filter(data_computed,SCHOOL == "1" & M4CID == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

length(data_computed_SCHOOL0_M4CID0$ID)#476
length(data_computed_SCHOOL1_M4CID1$ID)#586

#TEST
summary(data_computed)
hist(data_computed$TEST)
qqnorm(data_computed$TEST)
qqline(data_computed$TEST)
ks.test(data_computed$TEST,dnorm(mean(data_computed$TEST),sd(data_computed$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$TEST and dnorm(mean(data_computed$TEST), sd(data_computed$TEST))
# D = 1, p-value = 0.001881
# alternative hypothesis: two-sided
shapiro.test(data_computed$TEST)
# Shapiro-Wilk normality test
# data:  data_computed$TEST
# W = 0.99582, p-value = 0.005493
#conclusion: not normal
ggplot(data_computed) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_SCHOOL0_M4CID0$TEST)#68
median(data_computed_SCHOOL1_M4CID1$TEST)#69
wilcox.test(data_computed_SCHOOL0_M4CID0$TEST,data_computed_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_SCHOOL0_M4CID0$TEST and data_computed_SCHOOL1_M4CID1$TEST
# W = 130091, p-value = 0.02962
# alternative hypothesis: true location shift is less than 0

#LAB
summary(data_computed)
hist(data_computed$LAB)
qqnorm(data_computed$LAB)
qqline(data_computed$LAB)
ks.test(data_computed$LAB,dnorm(mean(data_computed$LAB),sd(data_computed$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$LAB and dnorm(mean(data_computed$LAB), sd(data_computed$LAB))
# D = 1, p-value = 0.001881
# alternative hypothesis: two-sided
shapiro.test(data_computed$LAB)
# Shapiro-Wilk normality test
# data:  data_computed$LAB
# W = 0.9906, p-value = 2.643e-06
# conclusion: not normal
ggplot(data_computed) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_SCHOOL0_M4CID0$LAB)#62
median(data_computed_SCHOOL1_M4CID1$LAB)#78
wilcox.test(data_computed_SCHOOL0_M4CID0$LAB,data_computed_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_SCHOOL0_M4CID0$LAB and data_computed_SCHOOL1_M4CID1$LAB
# W = 77111, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
summary(data_computed)
hist(data_computed$BEHAV)
qqnorm(data_computed$BEHAV)
qqline(data_computed$BEHAV)
ks.test(data_computed$BEHAV,dnorm(mean(data_computed$BEHAV),sd(data_computed$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$BEHAV and dnorm(mean(data_computed$BEHAV), sd(data_computed$BEHAV))
# D = 1, p-value = 0.0009407
# alternative hypothesis: two-sided
shapiro.test(data_computed$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed$BEHAV
# W = 0.9795, p-value = 4.29e-11
#conclusion: not normal
ggplot(data_computed) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_SCHOOL0_M4CID0$BEHAV)#77
median(data_computed_SCHOOL1_M4CID1$BEHAV)#73
wilcox.test(data_computed_SCHOOL0_M4CID0$BEHAV,data_computed_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_SCHOOL0_M4CID0$BEHAV and data_computed_SCHOOL1_M4CID1$BEHAV
# W = 164808, p-value = 1.716e-07
# alternative hypothesis: true location shift is greater than 0

# CLASS
summary(data_computed)
hist(data_computed$CLASS)
qqnorm(data_computed$CLASS)
qqline(data_computed$CLASS)
ks.test(data_computed$CLASS,dnorm(mean(data_computed$CLASS),sd(data_computed$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$CLASS and dnorm(mean(data_computed$CLASS), sd(data_computed$CLASS))
# D = 1, p-value = 0.001881
# alternative hypothesis: two-sided
shapiro.test(data_computed$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed$CLASS
# W = 0.99616, p-value = 0.00977
#conclusion: not normal
ggplot(data_computed) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))#not accurate, it includes school 1 and M4CID0
median(data_computed_SCHOOL0_M4CID0$CLASS)#69
median(data_computed_SCHOOL1_M4CID1$CLASS)#71
wilcox.test(data_computed_SCHOOL0_M4CID0$CLASS,data_computed_SCHOOL1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_SCHOOL0_M4CID0$CLASS and data_computed_SCHOOL1_M4CID1$CLASS
# W = 115251, p-value = 5.531e-07
# alternative hypothesis: true location shift is less than 0

# Conclusion computed diferences for junior and junior high school 0 and 1 
#(exclude school 1 without 4C/ID)
# negative BEHAV 
# TEST, LAB, CLASS positive

# Table 12: Inference for computed diferences for junior and junior high school 0 and 1 
# (excluding school 1 without 4C/ID)

# Inference for computed global sample differences with and without 4CID
# use the calculus about the computed differences already made 

# see Table 11: Differences computed for junior school 0 and 1

# all sample
data_computed_all<-data # all for all sample

# values added to variables only of school 1
data_computed_all$TEST<-ifelse(data_computed_all$SCHOOL==1,data_computed_all$TEST+dif_mean_TEST,data_computed_all$TEST)
data_computed_all$LAB<-ifelse(data_computed_all$SCHOOL==1,data_computed_all$LAB+dif_mean_LAB,data_computed_all$LAB)
data_computed_all$CLASS<-ifelse(data_computed_all$SCHOOL==1,data_computed_all$CLASS+dif_mean_CLASS,data_computed_all$CLASS)
data_computed_all$BEHAV<-ifelse(data_computed_all$SCHOOL==1,data_computed_all$BEHAV+dif_mean_BEHAV,data_computed_all$BEHAV)

#all sample with and without 4C/ID, no matter which school
data_computed_all_M4CID0<-filter(data_computed_all,M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_all_M4CID1<-filter(data_computed_all,M4CID == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_computed_all)

length(data_computed_all_M4CID0$ID)#828
length(data_computed_all_M4CID1$ID)#586

#TEST
hist(data_computed_all$TEST)
qqnorm(data_computed_all$TEST)
qqline(data_computed_all$TEST)
ks.test(data_computed_all$TEST,dnorm(mean(data_computed_all$TEST),sd(data_computed_all$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_all$TEST and dnorm(mean(data_computed_all$TEST), sd(data_computed_all$TEST))
# D = 1, p-value = 0.0007067
# alternative hypothesis: two-sided
shapiro.test(data_computed_all$TEST)
# Shapiro-Wilk normality test
# data:  data_computed_all$TEST
# W = 0.99565, p-value = 0.0004377
#conclusion: not normal
ggplot(data_computed_all) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_all_M4CID0$TEST)#67
median(data_computed_all_M4CID1$TEST)#69
wilcox.test(data_computed_all_M4CID0$TEST,data_computed_all_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_all_M4CID0$TEST and data_computed_all_M4CID1$TEST
# W = 223254, p-value = 0.005261
# alternative hypothesis: true location shift is less than 0

#LAB
hist(data_computed_all$LAB)
qqnorm(data_computed_all$LAB)
qqline(data_computed_all$LAB)
ks.test(data_computed_all$LAB,dnorm(mean(data_computed_all$LAB),sd(data_computed_all$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_all$LAB and dnorm(mean(data_computed_all$LAB), sd(data_computed_all$LAB))
# D = 1, p-value = 0.0007067
# alternative hypothesis: two-sided
shapiro.test(data_computed_all$LAB)
# Shapiro-Wilk normality test
# data:  data_computed_all$LAB
# W = 0.98723, p-value = 8.131e-10
#conclusion: not normal
ggplot(data_computed_all) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_all_M4CID0$LAB)#63
median(data_computed_all_M4CID1$LAB)#78
wilcox.test(data_computed_all_M4CID0$LAB,data_computed_all_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_all_M4CID0$LAB and data_computed_all_M4CID1$LAB
# W = 140102, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_computed_all$BEHAV)
qqnorm(data_computed_all$BEHAV)
qqline(data_computed_all$BEHAV)
ks.test(data_computed_all$BEHAV,dnorm(mean(data_computed_all$BEHAV),sd(data_computed_all$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_all$BEHAV and dnorm(mean(data_computed_all$BEHAV), sd(data_computed_all$BEHAV))
# D = 1, p-value = 0.0007067
# alternative hypothesis: two-sided
shapiro.test(data_computed_all$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed_all$BEHAV
# W = 0.97601, p-value = 1.333e-14
#conclusion: not normal
ggplot(data_computed_all) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_all_M4CID0$BEHAV)#77
median(data_computed_all_M4CID1$BEHAV)#73
wilcox.test(data_computed_all_M4CID0$BEHAV,data_computed_all_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_all_M4CID0$BEHAV and data_computed_all_M4CID1$BEHAV
# W = 287377, p-value = 1.613e-09
# alternative hypothesis: true location shift is greater than 0

# CLASS
hist(data_computed_all$CLASS)
qqnorm(data_computed_all$CLASS)
qqline(data_computed_all$CLASS)
ks.test(data_computed_all$CLASS,dnorm(mean(data_computed_all$CLASS),sd(data_computed_all$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_all$CLASS and dnorm(mean(data_computed_all$CLASS), sd(data_computed_all$CLASS))
# D = 1, p-value = 0.001413
# alternative hypothesis: two-sided
shapiro.test(data_computed_all$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed_all$CLASS
# W = 0.99646, p-value = 0.002478
#conclusion: normal
ggplot(data_computed_all) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_all_M4CID0$CLASS)#68
median(data_computed_all_M4CID1$CLASS)#71
wilcox.test(data_computed_all_M4CID0$CLASS,data_computed_all_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_all_M4CID0$CLASS and data_computed_all_M4CID1$CLASS
# W = 199597, p-value = 6.514e-09
# alternative hypothesis: true location shift is less than 0

# Conclusions 4C/ID treatment effect with computed samples for the entire sample
# BEHAV negative
# TEST, LAB, CLASS positive

# Table 13: Inference for computed differences for the entire sample

# Inference only for School 1, global sample (include junior high for school 1): sample not computed
summary(data)
data_SCHOOL1<-filter(data,data$SCHOOL=="1")  %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
length(which(data_SCHOOL1$M4CID=="0"))#352 observations
length(which(data_SCHOOL1$M4CID=="1"))#586 observations
data_SCHOOL1_M4CID0 <- filter(data_SCHOOL1,M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_SCHOOL1_M4CID1 <- filter(data_SCHOOL1,M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_SCHOOL1)
hist(data_SCHOOL1$TEST)
qqnorm(data_SCHOOL1$TEST)
qqline(data_SCHOOL1$TEST)
ks.test(data_SCHOOL1$TEST,dnorm(mean(data_SCHOOL1$TEST),sd(data_SCHOOL1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1$TEST and dnorm(mean(data_SCHOOL1$TEST), sd(data_SCHOOL1$TEST))
# D = 0.99787, p-value = 0.005325
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1$TEST
# W = 0.98943, p-value = 2.8e-06
#conclusion: Samples not normal, so use Kendall non-parametric correlation test

# TEST
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$TEST,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$TEST,300))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_SCHOOL1_M4CID0.TEST..300.,sample2$sample.data_SCHOOL1_M4CID1.TEST..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.TEST..300. and sample2$sample.data_SCHOOL1_M4CID1.TEST..300.
# z = 0.21033, p-value = 0.8334
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.008279556 
# sample independency, so it is possible to use Wilcoxon non-parametric inference test
ggplot(data_SCHOOL1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_M4CID0$TEST)#54
median(data_SCHOOL1_M4CID1$TEST)#57
wilcox.test(data_SCHOOL1_M4CID0$TEST,data_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_M4CID0$TEST and data_SCHOOL1_M4CID1$TEST
# W = 93164, p-value = 0.006522
# alternative hypothesis: true location shift is less than 0

# LAB
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$LAB,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$LAB,300))
#Not normal, Kendall non-parametric correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.LAB..300.,sample2$sample.data_SCHOOL1_M4CID1.LAB..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.LAB..300. and sample2$sample.data_SCHOOL1_M4CID1.LAB..300.
# z = -1.4187, p-value = 0.156
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.05601023 
hist(data_SCHOOL1$LAB)
qqnorm(data_SCHOOL1$LAB)
qqline(data_SCHOOL1$LAB)
ks.test(data_SCHOOL1$LAB,dnorm(mean(data_SCHOOL1$LAB),sd(data_SCHOOL1$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1$LAB and dnorm(mean(data_SCHOOL1$LAB), sd(data_SCHOOL1$LAB))
# D = 0.98507, p-value = 0.02875
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1$LAB
# W = 0.97789, p-value = 9.766e-11
#conclusion: not normal
ggplot(data_SCHOOL1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_M4CID0$LAB)#51
median(data_SCHOOL1_M4CID1$LAB)#65
wilcox.test(data_SCHOOL1_M4CID0$LAB,data_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_M4CID0$LAB and data_SCHOOL1_M4CID1$LAB
# W = 62990, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$BEHAV,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$BEHAV,300))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.BEHAV..300.,sample2$sample.data_SCHOOL1_M4CID1.BEHAV..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.BEHAV..300. and sample2$sample.data_SCHOOL1_M4CID1.BEHAV..300.
# z = 0.26, p-value = 0.7949
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01029303 
hist(data_SCHOOL1$BEHAV)
qqnorm(data_SCHOOL1$BEHAV)
qqline(data_SCHOOL1$BEHAV)
ks.test(data_SCHOOL1$BEHAV,dnorm(mean(data_SCHOOL1$BEHAV),sd(data_SCHOOL1$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1$BEHAV and dnorm(mean(data_SCHOOL1$BEHAV), sd(data_SCHOOL1$BEHAV))
# D = 1, p-value = 0.001065
# alternative hypothesis: two-sided
# shapiro.test(data_SCHOOL1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1$BEHAV
# W = 0.96689, p-value = 9.072e-14
#conclusion: not normal and independent
ggplot(data_SCHOOL1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_M4CID0$BEHAV)#80
median(data_SCHOOL1_M4CID1$BEHAV)#76
wilcox.test(data_SCHOOL1_M4CID0$BEHAV,data_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_M4CID0$BEHAV and data_SCHOOL1_M4CID1$BEHAV
# W = 122569, p-value = 6.553e-07
# alternative hypothesis: true location shift is greater than 0

# CLASS
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$CLASS,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$CLASS,300))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.CLASS..300.,sample2$sample.data_SCHOOL1_M4CID1.CLASS..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.CLASS..300. and sample2$sample.data_SCHOOL1_M4CID1.CLASS..300.
# z = 1.0436, p-value = 0.2967
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau  0.04046901 
hist(data_SCHOOL1$CLASS)
qqnorm(data_SCHOOL1$CLASS)
qqline(data_SCHOOL1$CLASS)
ks.test(data_SCHOOL1$CLASS,dnorm(mean(data_SCHOOL1$CLASS),sd(data_SCHOOL1$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1$CLASS and dnorm(mean(data_SCHOOL1$CLASS), sd(data_SCHOOL1$CLASS))
# D = 1, p-value = 0.00213
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1$CLASS
# W = 0.992, p-value = 5.797e-05
#conclusion: not normal and independent samples
ggplot(data_SCHOOL1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_M4CID0$CLASS)#58
median(data_SCHOOL1_M4CID1$CLASS)#62
wilcox.test(data_SCHOOL1_M4CID0$CLASS,data_SCHOOL1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_M4CID0$CLASS and data_SCHOOL1_M4CID1$CLASS
# W = 84348, p-value = 1.459e-06
# alternative hypothesis: true location shift is less than 0

# Table 14: Inference only for junior and junior high school 1

#Conclusions for sample junior and junior high school 1
# not normal, sample independent, no rejection of h0
# 4CID as positive impact on TEST, LAB and CLASS Variables;
# 4CID as a negative impact in BEHAV variable

# inference junior, school 1
data_JUNIOR_SCHOOL1<-filter(data,GRADE <="2" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_JUNIOR_SCHOOL1)
summary(data_JUNIOR_SCHOOL1)

length(which(data_JUNIOR_SCHOOL1$M4CID=="0"))#318 entrances
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
# z = 1.545, p-value = 0.1223
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.06083573
hist(data_JUNIOR_SCHOOL1$TEST)
qqnorm(data_JUNIOR_SCHOOL1$TEST)
qqline(data_JUNIOR_SCHOOL1$TEST)
ks.test(data_JUNIOR_SCHOOL1$TEST,dnorm(mean(data_JUNIOR_SCHOOL1$TEST),sd(data_JUNIOR_SCHOOL1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$TEST and dnorm(mean(data_JUNIOR_SCHOOL1$TEST), sd(data_JUNIOR_SCHOOL1$TEST))
# D = 1, p-value = 0.002924
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1$TEST)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$TEST
# W = 0.98652, p-value = 6.164e-06
#conclusion: not normal but independent sample
ggplot(data_JUNIOR_SCHOOL1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_M4CID0$TEST)#54
median(data_JUNIOR_SCHOOL1_M4CID1$TEST)#58
wilcox.test(data_JUNIOR_SCHOOL1_M4CID0$TEST,data_JUNIOR_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_M4CID0$TEST and data_JUNIOR_SCHOOL1_M4CID1$TEST
# W = 50325, p-value = 0.00136
# alternative hypothesis: true location shift is less than 0

# LAB
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$LAB,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$LAB,300))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.LAB..300.,sample2$sample.data_SCHOOL1_M4CID1.LAB..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.LAB..300. and sample2$sample.data_SCHOOL1_M4CID1.LAB..300.
# z = -0.8257, p-value = 0.409
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.03263017 
hist(data_JUNIOR_SCHOOL1$LAB)
qqnorm(data_JUNIOR_SCHOOL1$LAB)
qqline(data_JUNIOR_SCHOOL1$LAB)
ks.test(data_JUNIOR_SCHOOL1$LAB,dnorm(mean(data_JUNIOR_SCHOOL1$LAB),sd(data_JUNIOR_SCHOOL1$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$LAB and dnorm(mean(data_JUNIOR_SCHOOL1$LAB), sd(data_JUNIOR_SCHOOL1$LAB))
# D = 0.98097, p-value = 0.03947
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1$LAB)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$LAB
# W = 0.97414, p-value = 1.264e-09
#conclusion: not normal and independent
ggplot(data_JUNIOR_SCHOOL1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_M4CID0$LAB)#51
median(data_JUNIOR_SCHOOL1_M4CID1$LAB)#66
wilcox.test(data_JUNIOR_SCHOOL1_M4CID0$LAB,data_JUNIOR_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_M4CID0$LAB and data_JUNIOR_SCHOOL1_M4CID1$LAB
# W = 33211, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$BEHAV,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$BEHAV,300))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.BEHAV..300.,sample2$sample.data_SCHOOL1_M4CID1.BEHAV..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.BEHAV..300. and sample2$sample.data_SCHOOL1_M4CID1.BEHAV..300.
# z = -0.34014, p-value = 0.7338
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01346713 
hist(data_JUNIOR_SCHOOL1$BEHAV)
qqnorm(data_JUNIOR_SCHOOL1$BEHAV)
qqline(data_JUNIOR_SCHOOL1$BEHAV)
ks.test(data_JUNIOR_SCHOOL1$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1$BEHAV),sd(data_JUNIOR_SCHOOL1$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$BEHAV and dnorm(mean(data_JUNIOR_SCHOOL1$BEHAV), sd(data_JUNIOR_SCHOOL1$BEHAV))
# D = 1, p-value = 0.001462
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$BEHAV
# W = 0.9632, p-value = 4.577e-12
#conclusion: not normal but independent
ggplot(data_JUNIOR_SCHOOL1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_M4CID0$BEHAV)#81
median(data_JUNIOR_SCHOOL1_M4CID1$BEHAV)#76
wilcox.test(data_JUNIOR_SCHOOL1_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_M4CID0$BEHAV and data_JUNIOR_SCHOOL1_M4CID1$BEHAV
# W = 66732, p-value = 0.0003594
# alternative hypothesis: true location shift is greater than 0

# CLASS
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$CLASS,300))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$CLASS,300))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.CLASS..300.,sample2$sample.data_SCHOOL1_M4CID1.CLASS..300.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.CLASS..300. and sample2$sample.data_SCHOOL1_M4CID1.CLASS..300.
# z = 0.21828, p-value = 0.8272
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.008464829
hist(data_JUNIOR_SCHOOL1$CLASS)
qqnorm(data_JUNIOR_SCHOOL1$CLASS)
qqline(data_JUNIOR_SCHOOL1$CLASS)
ks.test(data_JUNIOR_SCHOOL1$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1$CLASS),sd(data_JUNIOR_SCHOOL1$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1$CLASS and dnorm(mean(data_JUNIOR_SCHOOL1$CLASS), sd(data_JUNIOR_SCHOOL1$CLASS))
# D = 1, p-value = 0.002924
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1$CLASS)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1$CLASS
# W = 0.99145, p-value = 0.0005524
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
# not normal sample, No rejection under H0, sample independent
# 4CID as a positive impact on TEST, LAB and CLASS Variable;
# 4CID as a negative impact in BEHAV variable

# Table 15: Inference for junior school 1

# GRADE
# Inference SCHOOL 1, Grade 7
summary (data)
data_7_SCHOOL1<-filter(data,GRADE =="0" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_7_SCHOOL1)
summary(data_7_SCHOOL1)

length(which(data_7_SCHOOL1$M4CID=="0"))#123 obs
length(which(data_7_SCHOOL1$M4CID=="1"))#99 obs

data_7_SCHOOL1_M4CID0 <- filter(data_7_SCHOOL1,M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_7_SCHOOL1_M4CID1 <- filter(data_7_SCHOOL1,M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_7_SCHOOL1_M4CID0)
summary(data_7_SCHOOL1_M4CID1)

# TEST
hist(data_7_SCHOOL1$TEST)
qqnorm(data_7_SCHOOL1$TEST)
qqline(data_7_SCHOOL1$TEST)
ks.test(data_7_SCHOOL1$TEST,dnorm(mean(data_7_SCHOOL1$TEST),sd(data_7_SCHOOL1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_7_SCHOOL1$TEST and dnorm(mean(data_7_SCHOOL1$TEST), sd(data_7_SCHOOL1$TEST))
# D = 1, p-value = 0.008969
# alternative hypothesis: two-sided
shapiro.test(data_7_SCHOOL1$TEST)
# Shapiro-Wilk normality test
# data:  data_7_SCHOOL1$TEST
# W = 0.98586, p-value = 0.02637
#conclusion: not normal
sample1<-data.frame(sample(data_7_SCHOOL1_M4CID0$TEST,90))
sample2<-data.frame(sample(data_7_SCHOOL1_M4CID1$TEST,90))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_M4CID0.TEST..90.,sample2$sample.data_7_SCHOOL1_M4CID1.TEST..90.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_M4CID0.TEST..90. and sample2$sample.data_7_SCHOOL1_M4CID1.TEST..90.
# z = 1.1478, p-value = 0.2511
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.08346056 
# Independet sample
ggplot(data_7_SCHOOL1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_7_SCHOOL1_M4CID0$TEST)#54
median(data_7_SCHOOL1_M4CID1$TEST)#58
wilcox.test(data_7_SCHOOL1_M4CID0$TEST,data_7_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_M4CID0$TEST and data_7_SCHOOL1_M4CID1$TEST
# W = 5604.5, p-value = 0.1547

# LAB
hist(data_7_SCHOOL1$LAB)
qqnorm(data_7_SCHOOL1$LAB)
qqline(data_7_SCHOOL1$LAB)
ks.test(data_7_SCHOOL1$LAB,dnorm(mean(data_7_SCHOOL1$LAB),sd(data_7_SCHOOL1$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_7_SCHOOL1$LAB and dnorm(mean(data_7_SCHOOL1$LAB), sd(data_7_SCHOOL1$LAB))
# D = 1, p-value = 0.008969
# alternative hypothesis: two-sided
shapiro.test(data_7_SCHOOL1$LAB)
# Shapiro-Wilk normality test
# data:  data_7_SCHOOL1$LAB
# W = 0.97527, p-value = 0.0006179
#conclusion: not normal
sample1<-data.frame(sample(data_7_SCHOOL1_M4CID0$LAB,90))
sample2<-data.frame(sample(data_7_SCHOOL1_M4CID1$LAB,90))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_M4CID0.LAB..90.,sample2$sample.data_7_SCHOOL1_M4CID1.LAB..90.,method="kendall")
# KKendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_M4CID0.LAB..90. and sample2$sample.data_7_SCHOOL1_M4CID1.LAB..90.
# z = 0.15359, p-value = 0.8779
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01121734 
# independent samples
ggplot(data_7_SCHOOL1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_7_SCHOOL1_M4CID0$LAB)#56
median(data_7_SCHOOL1_M4CID1$LAB)#69
wilcox.test(data_7_SCHOOL1_M4CID0$LAB,data_7_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_M4CID0$LAB and data_7_SCHOOL1_M4CID1$LAB
# W = 4079, p-value = 1.198e-05
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_7_SCHOOL1$BEHAV)
qqnorm(data_7_SCHOOL1$BEHAV)
qqline(data_7_SCHOOL1$BEHAV)
ks.test(data_7_SCHOOL1$BEHAV,dnorm(mean(data_7_SCHOOL1$BEHAV),sd(data_7_SCHOOL1$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_7_SCHOOL1$BEHAV and dnorm(mean(data_7_SCHOOL1$BEHAV), sd(data_7_SCHOOL1$BEHAV))
# D = 1, p-value = 0.008969
# alternative hypothesis: two-sided
shapiro.test(data_7_SCHOOL1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_7_SCHOOL1$BEHAV
# W = 0.9766, p-value = 0.0009589
#conclusion: not normal
sample1<-data.frame(sample(data_7_SCHOOL1_M4CID0$BEHAV,90))
sample2<-data.frame(sample(data_7_SCHOOL1_M4CID1$BEHAV,90))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_M4CID0.BEHAV..90.,sample2$sample.data_7_SCHOOL1_M4CID1.BEHAV..90.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_M4CID0.BEHAV..90. and sample2$sample.data_7_SCHOOL1_M4CID1.BEHAV..90.
# z = 0.55146, p-value = 0.5813
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04028563 
# sample independent
ggplot(data_7_SCHOOL1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_7_SCHOOL1_M4CID0$BEHAV)#77
median(data_7_SCHOOL1_M4CID1$BEHAV)#73
wilcox.test(data_7_SCHOOL1_M4CID0$BEHAV,data_7_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_M4CID0$BEHAV and data_7_SCHOOL1_M4CID1$BEHAV
# W = 7084, p-value = 0.0182
# alternative hypothesis: true location shift is greater than 0

# CLASS
hist(data_7_SCHOOL1$CLASS)
qqnorm(data_7_SCHOOL1$CLASS)
qqline(data_7_SCHOOL1$CLASS)
ks.test(data_7_SCHOOL1$CLASS,dnorm(mean(data_7_SCHOOL1$CLASS),sd(data_7_SCHOOL1$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_7_SCHOOL1$CLASS and dnorm(mean(data_7_SCHOOL1$CLASS), sd(data_7_SCHOOL1$CLASS))
# D = 1, p-value = 0.008969
# alternative hypothesis: two-sided
shapiro.test(data_7_SCHOOL1$CLASS)
# Shapiro-Wilk normality test
# data:  data_7_SCHOOL1$CLASS
# W = 0.9928, p-value = 0.3536
#conclusion: shapir report normal, while KS no
sample1<-data.frame(sample(data_7_SCHOOL1_M4CID0$CLASS,90))
sample2<-data.frame(sample(data_7_SCHOOL1_M4CID1$CLASS,90))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_M4CID0.CLASS..90.,sample2$sample.data_7_SCHOOL1_M4CID1.CLASS..90.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_M4CID0.CLASS..90. and sample2$sample.data_7_SCHOOL1_M4CID1.CLASS..90.
# z = 0.0034856, p-value = 0.9972
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.0002501876 
# Independent samples
ggplot(data_7_SCHOOL1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_7_SCHOOL1_M4CID0$CLASS)#60
median(data_7_SCHOOL1_M4CID1$CLASS)#64
wilcox.test(data_7_SCHOOL1_M4CID0$CLASS,data_7_SCHOOL1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_M4CID0$CLASS and data_7_SCHOOL1_M4CID1$CLASS
# W = 5144.5, p-value = 0.02366
# alternative hypothesis: true location shift is less than 0

# Inference SCHOOL 1, Grade 8
summary (data)
data_8_SCHOOL1<-filter(data,GRADE =="1" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_8_SCHOOL1)
summary(data_8_SCHOOL1)

length(which(data_8_SCHOOL1$M4CID=="0"))#92 obs
length(which(data_8_SCHOOL1$M4CID=="1"))#147 obs

data_8_SCHOOL1_M4CID0 <- filter(data_8_SCHOOL1,M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_8_SCHOOL1_M4CID1 <- filter(data_8_SCHOOL1,M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_8_SCHOOL1_M4CID0)
summary(data_8_SCHOOL1_M4CID1)

# TEST
hist(data_8_SCHOOL1$TEST)
qqnorm(data_8_SCHOOL1$TEST)
qqline(data_8_SCHOOL1$TEST)
ks.test(data_8_SCHOOL1$TEST,dnorm(mean(data_8_SCHOOL1$TEST),sd(data_8_SCHOOL1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_8_SCHOOL1$TEST and dnorm(mean(data_8_SCHOOL1$TEST), sd(data_8_SCHOOL1$TEST))
# D = 1, p-value = 0.004167
# alternative hypothesis: two-sided
shapiro.test(data_8_SCHOOL1$TEST)
# Shapiro-Wilk normality test
# data:  data_8_SCHOOL1$TEST
# W = 0.98173, p-value = 0.003576
#conclusion: not normal
sample1<-data.frame(sample(data_8_SCHOOL1_M4CID0$TEST,90))
sample2<-data.frame(sample(data_8_SCHOOL1_M4CID1$TEST,90))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_M4CID0.TEST..90.,sample2$sample.data_8_SCHOOL1_M4CID1.TEST..90.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_M4CID0.TEST..90. and sample2$sample.data_8_SCHOOL1_M4CID1.TEST..90.
# z = 0.67348, p-value = 0.5006
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04906577 
#samples are independent
ggplot(data_8_SCHOOL1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_8_SCHOOL1_M4CID0$TEST)#54
median(data_8_SCHOOL1_M4CID1$TEST)#58
wilcox.test(data_8_SCHOOL1_M4CID0$TEST,data_8_SCHOOL1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_M4CID0$TEST and data_8_SCHOOL1_M4CID1$TEST
# W = 4986.5, p-value = 0.00032
# alternative hypothesis: true location shift is less than 0

# LAB
hist(data_8_SCHOOL1$LAB)
qqnorm(data_8_SCHOOL1$LAB)
qqline(data_8_SCHOOL1$LAB)
ks.test(data_8_SCHOOL1$LAB,dnorm(mean(data_8_SCHOOL1$LAB),sd(data_8_SCHOOL1$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_8_SCHOOL1$LAB and dnorm(mean(data_8_SCHOOL1$LAB), sd(data_8_SCHOOL1$LAB))
# D = 0.96234, p-value = 0.07917
# alternative hypothesis: two-sided
shapiro.test(data_8_SCHOOL1$LAB)
# Shapiro-Wilk normality test
# data:  data_8_SCHOOL1$LAB
# W = 0.96304, p-value = 7.635e-06
#conclusion: not normal by shapiro analysis
sample1<-data.frame(sample(data_8_SCHOOL1_M4CID0$LAB,90))
sample2<-data.frame(sample(data_8_SCHOOL1_M4CID1$LAB,90))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_M4CID0.LAB..90.,sample2$sample.data_8_SCHOOL1_M4CID1.LAB..90.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_M4CID0.LAB..90. and sample2$sample.data_8_SCHOOL1_M4CID1.LAB..90.
# z = 0.28602, p-value = 0.7749
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.02151758 
# sample are independent
ggplot(data_8_SCHOOL1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_8_SCHOOL1_M4CID0$LAB)#51
median(data_8_SCHOOL1_M4CID1$LAB)#62
wilcox.test(data_8_SCHOOL1_M4CID0$LAB,data_8_SCHOOL1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_M4CID0$LAB and data_8_SCHOOL1_M4CID1$LAB
# W = 4156.5, p-value = 2.672e-07
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_8_SCHOOL1$BEHAV)
qqnorm(data_8_SCHOOL1$BEHAV)
qqline(data_8_SCHOOL1$BEHAV)
ks.test(data_8_SCHOOL1$BEHAV,dnorm(mean(data_8_SCHOOL1$BEHAV),sd(data_8_SCHOOL1$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_8_SCHOOL1$BEHAV and dnorm(mean(data_8_SCHOOL1$BEHAV), sd(data_8_SCHOOL1$BEHAV))
# D = 1, p-value = 0.004167
# alternative hypothesis: two-sided
shapiro.test(data_8_SCHOOL1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_8_SCHOOL1$BEHAV
# W = 0.94447, p-value = 6.821e-08
#conclusion: not normal
sample1<-data.frame(sample(data_8_SCHOOL1_M4CID0$BEHAV,90))
sample2<-data.frame(sample(data_8_SCHOOL1_M4CID1$BEHAV,90))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_M4CID0.BEHAV..90.,sample2$sample.data_8_SCHOOL1_M4CID1.BEHAV..90.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_M4CID0.BEHAV..90. and sample2$sample.data_8_SCHOOL1_M4CID1.BEHAV..90.
# z = 1.4214, p-value = 0.1552
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.105448 
ggplot(data_8_SCHOOL1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_8_SCHOOL1_M4CID0$BEHAV)#88
median(data_8_SCHOOL1_M4CID1$BEHAV)#72
wilcox.test(data_8_SCHOOL1_M4CID0$BEHAV,data_8_SCHOOL1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_M4CID0$BEHAV and data_8_SCHOOL1_M4CID1$BEHAV
# W = 9869, p-value = 1.142e-09
# alternative hypothesis: true location shift is greater than 0

# CLASS
hist(data_8_SCHOOL1$CLASS)
qqnorm(data_8_SCHOOL1$CLASS)
qqline(data_8_SCHOOL1$CLASS)
ks.test(data_8_SCHOOL1$CLASS,dnorm(mean(data_8_SCHOOL1$CLASS),sd(data_8_SCHOOL1$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_8_SCHOOL1$CLASS and dnorm(mean(data_8_SCHOOL1$CLASS), sd(data_8_SCHOOL1$CLASS))
# D = 1, p-value = 0.008333
# alternative hypothesis: two-sided
shapiro.test(data_8_SCHOOL1$CLASS)
# Shapiro-Wilk normality test
# data:  data_8_SCHOOL1$CLASS
# W = 0.98399, p-value = 0.008599
#conclusion: not normal
sample1<-data.frame(sample(data_8_SCHOOL1_M4CID0$CLASS,90))
sample2<-data.frame(sample(data_8_SCHOOL1_M4CID1$CLASS,90))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_M4CID0.CLASS..90.,sample2$sample.data_8_SCHOOL1_M4CID1.CLASS..90.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_M4CID0.CLASS..90. and sample2$sample.data_8_SCHOOL1_M4CID1.CLASS..90.
# z = 0.055772, p-value = 0.9555
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.004006516 
# Sample independent
ggplot(data_8_SCHOOL1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_8_SCHOOL1_M4CID0$CLASS)#58
median(data_8_SCHOOL1_M4CID1$CLASS)#61
wilcox.test(data_8_SCHOOL1_M4CID0$CLASS,data_8_SCHOOL1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_M4CID0$CLASS and data_8_SCHOOL1_M4CID1$CLASS
# W = 5330, p-value = 0.002957
# alternative hypothesis: true location shift is less than 0

# Inference for SCHOOL 1, Grade 9
summary (data)
data_9_SCHOOL1<-filter(data,GRADE =="2" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_9_SCHOOL1)
summary(data_9_SCHOOL1)

length(which(data_9_SCHOOL1$M4CID=="0"))#103 obs
length(which(data_9_SCHOOL1$M4CID=="1"))#119 obs

data_9_SCHOOL1_M4CID0 <- filter(data_9_SCHOOL1,M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_9_SCHOOL1_M4CID1 <- filter(data_9_SCHOOL1,M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_9_SCHOOL1_M4CID0)
summary(data_9_SCHOOL1_M4CID1)

# TEST
hist(data_9_SCHOOL1$TEST)
qqnorm(data_9_SCHOOL1$TEST)
qqline(data_9_SCHOOL1$TEST)
ks.test(data_9_SCHOOL1$TEST,dnorm(mean(data_9_SCHOOL1$TEST),sd(data_9_SCHOOL1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_9_SCHOOL1$TEST and dnorm(mean(data_9_SCHOOL1$TEST), sd(data_9_SCHOOL1$TEST))
# D = 1, p-value = 0.008969
# alternative hypothesis: two-sided
shapiro.test(data_9_SCHOOL1$TEST)
# Shapiro-Wilk normality test
# data:  data_9_SCHOOL1$TEST
# W = 0.97502, p-value = 0.0005698
#conclusion: not normal
sample1<-data.frame(sample(data_9_SCHOOL1_M4CID0$TEST,100))
sample2<-data.frame(sample(data_9_SCHOOL1_M4CID1$TEST,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_M4CID0.TEST..100.,sample2$sample.data_9_SCHOOL1_M4CID1.TEST..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_M4CID0.TEST..100. and sample2$sample.data_9_SCHOOL1_M4CID1.TEST..100.
# z = -0.66468, p-value = 0.5063
# alternative hypothesis: true tau is not equal to 0
# sample estimates:tau -0.04571077 
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
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_9_SCHOOL1$LAB and dnorm(mean(data_9_SCHOOL1$LAB), sd(data_9_SCHOOL1$LAB))
# D = 0.98198, p-value = 0.04036
# alternative hypothesis: two-sided
shapiro.test(data_9_SCHOOL1$LAB)
# Shapiro-Wilk normality test
# data:  data_9_SCHOOL1$LAB
# W = 0.97603, p-value = 0.0007947
#conclusion: not normal
sample1<-data.frame(sample(data_9_SCHOOL1_M4CID0$LAB,100))
sample2<-data.frame(sample(data_9_SCHOOL1_M4CID1$LAB,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_M4CID0.LAB..100.,sample2$sample.data_9_SCHOOL1_M4CID1.LAB..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_M4CID0.LAB..100. and sample2$sample.data_9_SCHOOL1_M4CID1.LAB..100.
# z = 1.5863, p-value = 0.1127
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.1096457 
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
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_9_SCHOOL1$BEHAV and dnorm(mean(data_9_SCHOOL1$BEHAV), sd(data_9_SCHOOL1$BEHAV))
# D = 1, p-value = 0.004484
# alternative hypothesis: two-sided
shapiro.test(data_9_SCHOOL1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_9_SCHOOL1$BEHAV
# W = 0.94726, p-value = 3.149e-07
#conclusion: not normal
sample1<-data.frame(sample(data_9_SCHOOL1_M4CID0$BEHAV,100))
sample2<-data.frame(sample(data_9_SCHOOL1_M4CID1$BEHAV,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_M4CID0.BEHAV..100.,sample2$sample.data_9_SCHOOL1_M4CID1.BEHAV..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_M4CID0.BEHAV..100. and sample2$sample.data_9_SCHOOL1_M4CID1.BEHAV..100.
# z = -0.32248, p-value = 0.7471
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.02254305 
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
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_9_SCHOOL1$CLASS and dnorm(mean(data_9_SCHOOL1$CLASS), sd(data_9_SCHOOL1$CLASS))
# D = 1, p-value = 0.008969
# alternative hypothesis: two-sided
shapiro.test(data_9_SCHOOL1$CLASS)
# Shapiro-Wilk normality test
# data:  data_9_SCHOOL1$CLASS
# W = 0.98916, p-value = 0.09299
#conclusion: normal by Shapiro
sample1<-data.frame(sample(data_9_SCHOOL1_M4CID0$CLASS,100))
sample2<-data.frame(sample(data_9_SCHOOL1_M4CID1$CLASS,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_M4CID0.CLASS..100.,sample2$sample.data_9_SCHOOL1_M4CID1.CLASS..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_M4CID0.CLASS..100. and sample2$sample.data_9_SCHOOL1_M4CID1.CLASS..100.
# z = 0.19657, p-value = 0.8442
# alternative hypothesis: true tau is not equal to 0
# sample estimates:  tau 0.01335222 
# independent
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
# 4CID has positive impact on TEST, LAB and CLASS Variables;
# 4CID has a negative impact in BEHAV variable

#GRADE 8
# Note: M4CID=="1",less then 100 entrances
# 4CID has a positive impact on TEST, LAB and CLASS
# 4CID negative on BEHAV Variable;

#Grade 9
# 4CID has no impact on TEST Variable;
# 4CID has positive impact in LAB, BEHAV and CLASS Variable;

########## RANK

#This analysis uses school 1 junior and junior high grade
summary(data)
data_SCHOOL1<-filter(data,SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_SCHOOL1)
data_SCHOOL1$RANK <-
  ifelse(data_SCHOOL1$CLASS <=45, 1,
         ifelse((data_SCHOOL1$CLASS >45 & data_SCHOOL1$CLASS<70), 2,
                ifelse(data_SCHOOL1$CLASS >=70, 3,NA)))
summary(data_SCHOOL1)
length(data_SCHOOL1$ID)#938 entrances
length(which(data_SCHOOL1$RANK=="1"))#93
length(which(data_SCHOOL1$RANK=="1"))/length(data_SCHOOL1$ID)#9%
length(which(data_SCHOOL1$RANK=="2"))#600
length(which(data_SCHOOL1$RANK=="2"))/length(data_SCHOOL1$ID)#64%
length(which(data_SCHOOL1$RANK=="3"))#245
length(which(data_SCHOOL1$RANK=="3"))/length(data_SCHOOL1$ID)#26%

#RANK 1, 93,9%
#RANK 2, 600, 64%
#RANK 3, 245, 26% 

#RANK 1
data_SCHOOL1_RANK1 <- filter(data_SCHOOL1,RANK=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_RANK1_M4CID0 <- filter(data_SCHOOL1_RANK1, M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_RANK1_M4CID1 <- filter(data_SCHOOL1_RANK1, M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_SCHOOL1_RANK1_M4CID0)
length(data_SCHOOL1_RANK1_M4CID0$ID)#56
length(data_SCHOOL1_RANK1_M4CID1$ID)#37
#note: under 100 sample entrances

# TEST
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK1_M4CID0$TEST,30))
sample2<-data.frame(sample(data_SCHOOL1_RANK1_M4CID1$TEST,30))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK1_M4CID0.TEST..30.,sample2$sample.data_SCHOOL1_RANK1_M4CID1.TEST..30.,method="kendall",exact = FALSE)
?cor.test
sample1
library("Kendall")
Kendall(sample1$sample.data_SCHOOL1_RANK1_M4CID0.TEST..30.,sample2$sample.data_SCHOOL1_RANK1_M4CID1.TEST..30.)
length(sample1$TEST)
#Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK1_M4CID0.TEST..30. and sample2$sample.data_SCHOOL1_RANK1_M4CID1.TEST..30.
# z = 0.7897, p-value = 0.4297
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.104641 
# sample independent
hist(data_SCHOOL1_RANK1$TEST)
qqnorm(data_SCHOOL1_RANK1$TEST)
qqline(data_SCHOOL1_RANK1$TEST)
ks.test(data_SCHOOL1_RANK1$TEST,dnorm(mean(data_SCHOOL1_RANK1$TEST),sd(data_SCHOOL1_RANK1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK1$TEST and dnorm(mean(data_SCHOOL1_RANK1$TEST), sd(data_SCHOOL1_RANK1$TEST))
# D = 0.97849, p-value = 0.06383
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK1$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK1$TEST
# W = 0.96926, p-value = 0.02717
#conclusion: normal
ggplot(data_SCHOOL1_RANK1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_RANK1_M4CID0$TEST)#34
median(data_SCHOOL1_RANK1_M4CID1$TEST)#33
wilcox.test(data_SCHOOL1_RANK1_M4CID0$TEST,data_SCHOOL1_RANK1_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK1_M4CID0$TEST and data_SCHOOL1_RANK1_M4CID1$TEST
# W = 1242.5, p-value = 0.05279
# alternative hypothesis: true location shift is greater than 0

# LAB
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK1_M4CID0$LAB,30))
sample2<-data.frame(sample(data_SCHOOL1_RANK1_M4CID1$LAB,30))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK1_M4CID0.LAB..30.,sample2$sample.data_SCHOOL1_RANK1_M4CID1.LAB..30.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK1_M4CID0.LAB..30. and sample2$sample.data_SCHOOL1_RANK1_M4CID1.LAB..30.
# z = -0.92261, p-value = 0.3562
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1239852 
#sample independent
hist(data_SCHOOL1_RANK1$LAB)
qqnorm(data_SCHOOL1_RANK1$LAB)
qqline(data_SCHOOL1_RANK1$LAB)
ks.test(data_SCHOOL1_RANK1$LAB,dnorm(mean(data_SCHOOL1_RANK1$LAB),sd(data_SCHOOL1_RANK1$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK1$LAB and dnorm(mean(data_SCHOOL1_RANK1$LAB), sd(data_SCHOOL1_RANK1$LAB))
# D = 0.87097, p-value = 0.266
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK1$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK1$LAB
# W = 0.94629, p-value = 0.0007958
#conclusion: not normal
ggplot(data_SCHOOL1_RANK1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_RANK1_M4CID0$LAB)#26
median(data_SCHOOL1_RANK1_M4CID1$LAB)#35
wilcox.test(data_SCHOOL1_RANK1_M4CID0$LAB,data_SCHOOL1_RANK1_M4CID1$LAB,alternative = "less")
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK1$LAB
# W = 0.94629, p-value = 0.0007958

# BEHAV
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK1_M4CID0$BEHAV,30))
sample2<-data.frame(sample(data_SCHOOL1_RANK1_M4CID1$BEHAV,30))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK1_M4CID0.BEHAV..30.,sample2$sample.data_SCHOOL1_RANK1_M4CID1.BEHAV..30.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK1_M4CID0.BEHAV..30. and sample2$sample.data_SCHOOL1_RANK1_M4CID1.BEHAV..30.
# z = -1.3245, p-value = 0.1853
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1739132 
#sample independent
hist(data_SCHOOL1_RANK1$BEHAV)
qqnorm(data_SCHOOL1_RANK1$BEHAV)
qqline(data_SCHOOL1_RANK1$BEHAV)
ks.test(data_SCHOOL1_RANK1$BEHAV,dnorm(mean(data_SCHOOL1_RANK1$BEHAV),sd(data_SCHOOL1_RANK1$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK1$BEHAV and dnorm(mean(data_SCHOOL1_RANK1$BEHAV), sd(data_SCHOOL1_RANK1$BEHAV))
# D = 1, p-value = 0.02128
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK1$BEHAV
# W = 0.9841, p-value = 0.3199
#conclusion: normal
ggplot(data_SCHOOL1_RANK1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_RANK1_M4CID0$BEHAV)#65
median(data_SCHOOL1_RANK1_M4CID1$BEHAV)#53
wilcox.test(data_SCHOOL1_RANK1_M4CID0$BEHAV,data_SCHOOL1_RANK1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK1_M4CID0$BEHAV and data_SCHOOL1_RANK1_M4CID1$BEHAV
# W = 1435, p-value = 0.0008741
# alternative hypothesis: true location shift is greater than 0

# CLASS
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK1_M4CID0$CLASS,30))
sample2<-data.frame(sample(data_SCHOOL1_RANK1_M4CID1$CLASS,30))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK1_M4CID0.CLASS..30.,sample2$sample.data_SCHOOL1_RANK1_M4CID1.CLASS..30.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK1_M4CID0.CLASS..30. and sample2$sample.data_SCHOOL1_RANK1_M4CID1.CLASS..30.
# z = 0.62483, p-value = 0.5321
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.08083141 
hist(data_SCHOOL1_RANK1$CLASS)
qqnorm(data_SCHOOL1_RANK1$CLASS)
qqline(data_SCHOOL1_RANK1$CLASS)
ks.test(data_SCHOOL1_RANK1$CLASS,dnorm(mean(data_SCHOOL1_RANK1$CLASS),sd(data_SCHOOL1_RANK1$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK1$CLASS and dnorm(mean(data_SCHOOL1_RANK1$CLASS), sd(data_SCHOOL1_RANK1$CLASS))
# D = 1, p-value = 0.01064
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK1$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK1$CLASS
# W = 0.88291, p-value = 5.321e-07
#conclusion: not normal
ggplot(data_SCHOOL1_RANK1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_RANK1_M4CID0$CLASS)#39
median(data_SCHOOL1_RANK1_M4CID1$CLASS)#38
wilcox.test(data_SCHOOL1_RANK1_M4CID0$CLASS,data_SCHOOL1_RANK1_M4CID1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK1_M4CID0$CLASS and data_SCHOOL1_RANK1_M4CID1$CLASS
# W = 1173, p-value = 0.142
# alternative hypothesis: true location shift is greater than 0

#RANK 2
data_SCHOOL1_RANK2 <- filter(data_SCHOOL1,RANK=="2") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_RANK2_M4CID0 <- filter(data_SCHOOL1_RANK2, M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_RANK2_M4CID1 <- filter(data_SCHOOL1_RANK2, M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_SCHOOL1_RANK2_M4CID0)
length(data_SCHOOL1_RANK2_M4CID0$ID)#224
length(data_SCHOOL1_RANK2_M4CID1$ID)#376

# TEST
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK2_M4CID0$TEST,200))
sample2<-data.frame(sample(data_SCHOOL1_RANK2_M4CID1$TEST,200))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK2_M4CID0.TEST..200.,sample2$sample.data_SCHOOL1_RANK2_M4CID1.TEST..200.,method="kendall")
#KKendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK2_M4CID0.TEST..200. and sample2$sample.data_SCHOOL1_RANK2_M4CID1.TEST..200.
# z = -0.19579, p-value = 0.8448
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.009561547 
# sample independent
hist(data_SCHOOL1_RANK2$TEST)
qqnorm(data_SCHOOL1_RANK2$TEST)
qqline(data_SCHOOL1_RANK2$TEST)
ks.test(data_SCHOOL1_RANK2$TEST,dnorm(mean(data_SCHOOL1_RANK2$TEST),sd(data_SCHOOL1_RANK2$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK2$TEST and dnorm(mean(data_SCHOOL1_RANK2$TEST), sd(data_SCHOOL1_RANK2$TEST))
# D = 1, p-value = 0.003328
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK2$TEST)
# Exact two-sample Kolmogorov-Smirnov test
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK2$TEST
# W = 0.99632, p-value = 0.1806
#conclusion: normal
ggplot(data_SCHOOL1_RANK2) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_RANK2_M4CID0$TEST)#53
median(data_SCHOOL1_RANK2_M4CID1$TEST)#51
wilcox.test(data_SCHOOL1_RANK2_M4CID0$TEST,data_SCHOOL1_RANK2_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK2_M4CID0$TEST and data_SCHOOL1_RANK2_M4CID1$TEST
# W = 44638, p-value = 0.1093
# alternative hypothesis: true location shift is greater than 0

# LAB
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK2_M4CID0$LAB,200))
sample2<-data.frame(sample(data_SCHOOL1_RANK2_M4CID1$LAB,200))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK2_M4CID0.LAB..200.,sample2$sample.data_SCHOOL1_RANK2_M4CID1.LAB..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK2_M4CID0.LAB..200. and sample2$sample.data_SCHOOL1_RANK2_M4CID1.LAB..200.
# z = 0.96981, p-value = 0.3321
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04735476 
#sample independent
hist(data_SCHOOL1_RANK2$LAB)
qqnorm(data_SCHOOL1_RANK2$LAB)
qqline(data_SCHOOL1_RANK2$LAB)
ks.test(data_SCHOOL1_RANK2$LAB,dnorm(mean(data_SCHOOL1_RANK2$LAB),sd(data_SCHOOL1_RANK2$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK2$LAB and dnorm(mean(data_SCHOOL1_RANK2$LAB), sd(data_SCHOOL1_RANK2$LAB))
# D = 0.99667, p-value = 0.006656
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK2$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK2$LAB
# W = 0.97051, p-value = 1.262e-09
#conclusion: not normal
ggplot(data_SCHOOL1_RANK2) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_RANK2_M4CID0$LAB)#51
median(data_SCHOOL1_RANK2_M4CID1$LAB)#60
wilcox.test(data_SCHOOL1_RANK2_M4CID0$LAB,data_SCHOOL1_RANK2_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK2_M4CID0$LAB and data_SCHOOL1_RANK2_M4CID1$LAB
# W = 27436, p-value = 4.39e-13
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK2_M4CID0$BEHAV,200))
sample2<-data.frame(sample(data_SCHOOL1_RANK2_M4CID1$BEHAV,200))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK2_M4CID0.BEHAV..200.,sample2$sample.data_SCHOOL1_RANK2_M4CID1.BEHAV..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK2_M4CID0.BEHAV..200. and sample2$sample.data_SCHOOL1_RANK2_M4CID1.BEHAV..200.
# z = 0.075125, p-value = 0.9401
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.003660025 
#sample independent
hist(data_SCHOOL1_RANK2$BEHAV)
qqnorm(data_SCHOOL1_RANK2$BEHAV)
qqline(data_SCHOOL1_RANK2$BEHAV)
ks.test(data_SCHOOL1_RANK2$BEHAV,dnorm(mean(data_SCHOOL1_RANK2$BEHAV),sd(data_SCHOOL1_RANK2$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK2$BEHAV and dnorm(mean(data_SCHOOL1_RANK2$BEHAV), sd(data_SCHOOL1_RANK2$BEHAV))
# D = 1, p-value = 0.001664
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK2$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK2$BEHAV
# W = 0.97972, p-value = 2.177e-07
#conclusion: not normal
ggplot(data_SCHOOL1_RANK2) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_RANK2_M4CID0$BEHAV)#80
median(data_SCHOOL1_RANK2_M4CID1$BEHAV)#72
wilcox.test(data_SCHOOL1_RANK2_M4CID0$BEHAV,data_SCHOOL1_RANK2_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK2_M4CID0$BEHAV and data_SCHOOL1_RANK2_M4CID1$BEHAV
# W = 55922, p-value = 8.72e-12
# alternative hypothesis: true location shift is greater than 0

# CLASS
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK2_M4CID0$CLASS,200))
sample2<-data.frame(sample(data_SCHOOL1_RANK2_M4CID1$CLASS,200))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK2_M4CID0.CLASS..200.,sample2$sample.data_SCHOOL1_RANK2_M4CID1.CLASS..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK2_M4CID0.CLASS..200. and sample2$sample.data_SCHOOL1_RANK2_M4CID1.CLASS..200.
# z = 0.51255, p-value = 0.6083
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.02445235 
hist(data_SCHOOL1_RANK2$CLASS)
qqnorm(data_SCHOOL1_RANK2$CLASS)
qqline(data_SCHOOL1_RANK2$CLASS)
ks.test(data_SCHOOL1_RANK2$CLASS,dnorm(mean(data_SCHOOL1_RANK2$CLASS),sd(data_SCHOOL1_RANK2$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK2$CLASS and dnorm(mean(data_SCHOOL1_RANK2$CLASS), sd(data_SCHOOL1_RANK2$CLASS))
# D = 1, p-value = 0.003328
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK2$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK2$CLASS
# W = 0.96624, p-value = 1.614e-10
#conclusion: not normal
ggplot(data_SCHOOL1_RANK2) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_RANK2_M4CID0$CLASS)#57
median(data_SCHOOL1_RANK2_M4CID1$CLASS)#58
wilcox.test(data_SCHOOL1_RANK2_M4CID0$CLASS,data_SCHOOL1_RANK2_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK2_M4CID0$CLASS and data_SCHOOL1_RANK2_M4CID1$CLASS
# W = 40020, p-value = 0.1543
# alternative hypothesis: true location shift is less than 0

#RANK 3
data_SCHOOL1_RANK3 <- filter(data_SCHOOL1,RANK=="3") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_RANK3_M4CID0 <- filter(data_SCHOOL1_RANK3, M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_RANK3_M4CID1 <- filter(data_SCHOOL1_RANK3, M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_SCHOOL1_RANK3_M4CID0)
length(data_SCHOOL1_RANK3_M4CID0$ID)#72
length(data_SCHOOL1_RANK3_M4CID1$ID)#173

# TEST
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK3_M4CID0$TEST,70))
sample2<-data.frame(sample(data_SCHOOL1_RANK3_M4CID1$TEST,70))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK3_M4CID0.TEST..70.,sample2$sample.data_SCHOOL1_RANK3_M4CID1.TEST..70.,method="kendall")
#Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK3_M4CID0.TEST..70. and sample2$sample.data_SCHOOL1_RANK3_M4CID1.TEST..70.
# z = -0.8738, p-value = 0.3822
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.07306728  sample independent
hist(data_SCHOOL1_RANK3$TEST)
qqnorm(data_SCHOOL1_RANK3$TEST)
qqline(data_SCHOOL1_RANK3$TEST)
ks.test(data_SCHOOL1_RANK3$TEST,dnorm(mean(data_SCHOOL1_RANK3$TEST),sd(data_SCHOOL1_RANK3$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK3$TEST and dnorm(mean(data_SCHOOL1_RANK3$TEST), sd(data_SCHOOL1_RANK3$TEST))
# D = 1, p-value = 0.004065
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK3$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK3$TEST
# W = 0.98815, p-value = 0.04122
#conclusion: not normal
ggplot(data_SCHOOL1_RANK3) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_RANK3_M4CID0$TEST)#78
median(data_SCHOOL1_RANK3_M4CID1$TEST)#79
wilcox.test(data_SCHOOL1_RANK3_M4CID0$TEST,data_SCHOOL1_RANK3_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK3_M4CID0$TEST and data_SCHOOL1_RANK3_M4CID1$TEST
# W = 6093, p-value = 0.395
# alternative hypothesis: true location shift is less than 0

# LAB
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK3_M4CID0$LAB,70))
sample2<-data.frame(sample(data_SCHOOL1_RANK3_M4CID1$LAB,70))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK3_M4CID0.LAB..70.,sample2$sample.data_SCHOOL1_RANK3_M4CID1.LAB..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK3_M4CID0.LAB..70. and sample2$sample.data_SCHOOL1_RANK3_M4CID1.LAB..70.
# z = 0.45751, p-value = 0.6473
# alternative hypothesis: true tau is not equal to 0
# sample estimates:tau 0.03836334 
#sample independent
hist(data_SCHOOL1_RANK3$LAB)
qqnorm(data_SCHOOL1_RANK3$LAB)
qqline(data_SCHOOL1_RANK3$LAB)
ks.test(data_SCHOOL1_RANK3$LAB,dnorm(mean(data_SCHOOL1_RANK3$LAB),sd(data_SCHOOL1_RANK3$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK3$LAB and dnorm(mean(data_SCHOOL1_RANK3$LAB), sd(data_SCHOOL1_RANK3$LAB))
# D = 1, p-value = 0.004065
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK3$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK3$LAB
# W = 0.95511, p-value = 6.733e-07
#conclusion: not normal
ggplot(data_SCHOOL1_RANK3) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_RANK3_M4CID0$LAB)#74
median(data_SCHOOL1_RANK3_M4CID1$LAB)#81
wilcox.test(data_SCHOOL1_RANK3_M4CID0$LAB,data_SCHOOL1_RANK3_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK3_M4CID0$LAB and data_SCHOOL1_RANK3_M4CID1$LAB
# W = 3938, p-value = 2.886e-06
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK3_M4CID0$BEHAV,70))
sample2<-data.frame(sample(data_SCHOOL1_RANK3_M4CID1$BEHAV,70))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK3_M4CID0.BEHAV..70.,sample2$sample.data_SCHOOL1_RANK3_M4CID1.BEHAV..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK3_M4CID0.BEHAV..70. and sample2$sample.data_SCHOOL1_RANK3_M4CID1.BEHAV..70.
# z = -1.7015, p-value = 0.08885
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1456001 
#sample independent
hist(data_SCHOOL1_RANK3$BEHAV)
qqnorm(data_SCHOOL1_RANK3$BEHAV)
qqline(data_SCHOOL1_RANK3$BEHAV)
ks.test(data_SCHOOL1_RANK3$BEHAV,dnorm(mean(data_SCHOOL1_RANK3$BEHAV),sd(data_SCHOOL1_RANK3$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK3$BEHAV and dnorm(mean(data_SCHOOL1_RANK3$BEHAV), sd(data_SCHOOL1_RANK3$BEHAV))
# D = 1, p-value = 0.004065
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK3$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK3$BEHAV
# W = 0.9271, p-value = 1.263e-09
#conclusion: not normal
ggplot(data_SCHOOL1_RANK3) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_RANK3_M4CID0$BEHAV)#94
median(data_SCHOOL1_RANK3_M4CID1$BEHAV)#88
wilcox.test(data_SCHOOL1_RANK3_M4CID0$BEHAV,data_SCHOOL1_RANK3_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK3_M4CID0$BEHAV and data_SCHOOL1_RANK3_M4CID1$BEHAV
# W = 8170.5, p-value = 5.982e-05
# alternative hypothesis: true location shift is greater than 0

# CLASS
# independency
sample1<-data.frame(sample(data_SCHOOL1_RANK3_M4CID0$CLASS,70))
sample2<-data.frame(sample(data_SCHOOL1_RANK3_M4CID1$CLASS,70))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_RANK3_M4CID0.CLASS..70.,sample2$sample.data_SCHOOL1_RANK3_M4CID1.CLASS..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_RANK3_M4CID0.CLASS..70. and sample2$sample.data_SCHOOL1_RANK3_M4CID1.CLASS..70.
# z = 0.49191, p-value = 0.6228
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04034947 
hist(data_SCHOOL1_RANK3$CLASS)
qqnorm(data_SCHOOL1_RANK3$CLASS)
qqline(data_SCHOOL1_RANK3$CLASS)
ks.test(data_SCHOOL1_RANK3$CLASS,dnorm(mean(data_SCHOOL1_RANK3$CLASS),sd(data_SCHOOL1_RANK3$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_RANK3$CLASS and dnorm(mean(data_SCHOOL1_RANK3$CLASS), sd(data_SCHOOL1_RANK3$CLASS))
# D = 1, p-value = 0.00813
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_RANK3$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_RANK3$CLASS
# W = 0.93556, p-value = 7.021e-09
#conclusion: not normal
ggplot(data_SCHOOL1_RANK3) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_RANK3_M4CID0$CLASS)#76
median(data_SCHOOL1_RANK3_M4CID1$CLASS)#79
wilcox.test(data_SCHOOL1_RANK3_M4CID0$CLASS,data_SCHOOL1_RANK3_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_RANK3_M4CID0$CLASS and data_SCHOOL1_RANK3_M4CID1$CLASS
# W = 5130.5, p-value = 0.01497
# alternative hypothesis: true location shift is less than 0

#This RANK analysis uses school 1 and junior grade sample only
summary(data)
data_JUNIOR_SCHOOL1<-filter(data,GRADE <="2" & SCHOOL == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_JUNIOR_SCHOOL1)
summary(data_JUNIOR_SCHOOL1)
data_JUNIOR_SCHOOL1$RANK <-
  ifelse(data_JUNIOR_SCHOOL1$CLASS <=45, 1,
         ifelse((data_JUNIOR_SCHOOL1$CLASS >45 & data_JUNIOR_SCHOOL1$CLASS<70), 2,
                ifelse(data_JUNIOR_SCHOOL1$CLASS >=70, 3,NA)))
summary(data_JUNIOR_SCHOOL1)
length(data_JUNIOR_SCHOOL1$ID)#683 entrances
length(which(data_JUNIOR_SCHOOL1$RANK=="1"))#77
length(which(data_JUNIOR_SCHOOL1$RANK=="1"))/length(data_JUNIOR_SCHOOL1$ID)#11%
length(which(data_JUNIOR_SCHOOL1$RANK=="2"))#421
length(which(data_JUNIOR_SCHOOL1$RANK=="2"))/length(data_JUNIOR_SCHOOL1$ID)#62%
length(which(data_JUNIOR_SCHOOL1$RANK=="3"))#185
length(which(data_JUNIOR_SCHOOL1$RANK=="3"))/length(data_JUNIOR_SCHOOL1$ID)#27%

#RANK 1, 77, 11%
#RANK 2, 421, 62%
#RANK 3, 185, 27% 

#RANK 1
data_JUNIOR_SCHOOL1_RANK1 <- filter(data_JUNIOR_SCHOOL1,RANK=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK1_M4CID0 <- filter(data_JUNIOR_SCHOOL1_RANK1, M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK1_M4CID1 <- filter(data_JUNIOR_SCHOOL1_RANK1, M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_JUNIOR_SCHOOL1_RANK1_M4CID0)
length(data_JUNIOR_SCHOOL1_RANK1_M4CID0$ID)#55
length(data_JUNIOR_SCHOOL1_RANK1_M4CID1$ID)#22
#note: under 100 sample entrances

# TEST
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID0$TEST,20))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID1$TEST,20))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.TEST..20.,sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.TEST..20.,method="kendall",exact = FALSE)
#Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.TEST..20. and sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.TEST..20.
# z = 1.0433, p-value = 0.2968
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.1720455 
# independent!

hist(data_JUNIOR_SCHOOL1_RANK1$TEST)
qqnorm(data_JUNIOR_SCHOOL1_RANK1$TEST)
qqline(data_JUNIOR_SCHOOL1_RANK1$TEST)
ks.test(data_JUNIOR_SCHOOL1_RANK1$TEST,dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$TEST),sd(data_JUNIOR_SCHOOL1_RANK1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK1$TEST and dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$TEST), sd(data_JUNIOR_SCHOOL1_RANK1$TEST))
# D = 1, p-value = 0.02564
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK1$TEST)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK1$TEST
# W = 0.97701, p-value = 0.1764
#conclusion: not normal
ggplot(data_JUNIOR_SCHOOL1_RANK1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK1_M4CID0$TEST)#33
median(data_JUNIOR_SCHOOL1_RANK1_M4CID1$TEST)#34
wilcox.test(data_JUNIOR_SCHOOL1_RANK1_M4CID0$TEST,data_JUNIOR_SCHOOL1_RANK1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK1_M4CID0$TEST and data_JUNIOR_SCHOOL1_RANK1_M4CID1$TEST
# W = 725, p-value = 0.9131
# alternative hypothesis: true location shift is less than 0

# LAB
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID0$LAB,20))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID1$LAB,20))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.LAB..20.,sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.LAB..20.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.LAB..20. and sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.LAB..20.
# z = 1.0203, p-value = 0.3076
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.1722461 
hist(data_JUNIOR_SCHOOL1_RANK1$LAB)
qqnorm(data_JUNIOR_SCHOOL1_RANK1$LAB)
qqline(data_JUNIOR_SCHOOL1_RANK1$LAB)
ks.test(data_JUNIOR_SCHOOL1_RANK1$LAB,dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$LAB),sd(data_JUNIOR_SCHOOL1_RANK1$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK1$LAB and dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$LAB), sd(data_JUNIOR_SCHOOL1_RANK1$LAB))
# D = 0.85714, p-value = 0.3077
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK1$LAB)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK1$LAB
# W = 0.93202, p-value = 0.0004847
#conclusion: not normal
ggplot(data_JUNIOR_SCHOOL1_RANK1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK1_M4CID0$LAB)#26
median(data_JUNIOR_SCHOOL1_RANK1_M4CID1$LAB)#35
wilcox.test(data_JUNIOR_SCHOOL1_RANK1_M4CID0$LAB,data_JUNIOR_SCHOOL1_RANK1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK1_M4CID0$LAB and data_JUNIOR_SCHOOL1_RANK1_M4CID1$LAB
# W = 346, p-value = 0.001746
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID0$BEHAV,20))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID1$BEHAV,20))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.BEHAV..20.,sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.BEHAV..20.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.BEHAV..20. and sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.BEHAV..20.
# z = 1.2731, p-value = 0.203
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.2113891 
hist(data_JUNIOR_SCHOOL1_RANK1$BEHAV)
qqnorm(data_JUNIOR_SCHOOL1_RANK1$BEHAV)
qqline(data_JUNIOR_SCHOOL1_RANK1$BEHAV)
ks.test(data_JUNIOR_SCHOOL1_RANK1$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$BEHAV),sd(data_JUNIOR_SCHOOL1_RANK1$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK1$BEHAV and dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$BEHAV), sd(data_JUNIOR_SCHOOL1_RANK1$BEHAV))
# D = 1, p-value = 0.02564
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK1$BEHAV
# W = 0.98835, p-value = 0.7112
#conclusion: normal
ggplot(data_JUNIOR_SCHOOL1_RANK1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK1_M4CID0$BEHAV)#65
median(data_JUNIOR_SCHOOL1_RANK1_M4CID1$BEHAV)#54
wilcox.test(data_JUNIOR_SCHOOL1_RANK1_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_RANK1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK1_M4CID0$BEHAV and data_JUNIOR_SCHOOL1_RANK1_M4CID1$BEHAV
# W = 866.5, p-value = 0.001615
# alternative hypothesis: true location shift is greater than 0

#CLASS
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID0$CLASS,20))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK1_M4CID1$CLASS,20))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.CLASS..20.,sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.CLASS..20.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID0.CLASS..20. and sample2$sample.data_JUNIOR_SCHOOL1_RANK1_M4CID1.CLASS..20.
# z = -0.35745, p-value = 0.7208
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.05835564 
#independent
hist(data_JUNIOR_SCHOOL1_RANK1$CLASS)
qqnorm(data_JUNIOR_SCHOOL1_RANK1$CLASS)
qqline(data_JUNIOR_SCHOOL1_RANK1$CLASS)
ks.test(data_JUNIOR_SCHOOL1_RANK1$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$CLASS),sd(data_JUNIOR_SCHOOL1_RANK1$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK1$CLASS and dnorm(mean(data_JUNIOR_SCHOOL1_RANK1$CLASS), sd(data_JUNIOR_SCHOOL1_RANK1$CLASS))
# D = 1, p-value = 0.01282
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK1$CLASS)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK1$CLASS
# W = 0.89273, p-value = 8.67e-06
#conclusion: not normal
ggplot(data_JUNIOR_SCHOOL1_RANK1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK1_M4CID0$CLASS)#39
median(data_JUNIOR_SCHOOL1_RANK1_M4CID1$CLASS)#39
wilcox.test(data_JUNIOR_SCHOOL1_RANK1_M4CID0$CLASS,data_JUNIOR_SCHOOL1_RANK1_M4CID1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK1_M4CID0$CLASS and data_JUNIOR_SCHOOL1_RANK1_M4CID1$CLASS
# W = 720, p-value = 0.09832
# alternative hypothesis: true location shift is greater than 0

#RANK 2
data_JUNIOR_SCHOOL1_RANK2 <- filter(data_JUNIOR_SCHOOL1,RANK=="2") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK2_M4CID0 <- filter(data_JUNIOR_SCHOOL1,RANK=="2"& M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK2_M4CID1 <- filter(data_JUNIOR_SCHOOL1,RANK=="2"& M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
length(data_JUNIOR_SCHOOL1_RANK2_M4CID0$ID)#196
length(data_JUNIOR_SCHOOL1_RANK2_M4CID1$ID)#225

# TEST
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID0$TEST,190))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID1$TEST,190))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.TEST..190.,sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.TEST..190.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.TEST..190. and sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.TEST..190.
# z = 0.73802, p-value = 0.4605
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.03689016 
# sample are independent

hist(data_JUNIOR_SCHOOL1_RANK2$TEST)
qqnorm(data_JUNIOR_SCHOOL1_RANK2$TEST)
qqline(data_JUNIOR_SCHOOL1_RANK2$TEST)
ks.test(data_JUNIOR_SCHOOL1_RANK2$TEST,dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$TEST),sd(data_JUNIOR_SCHOOL1_RANK2$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK2$TEST and dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$TEST), sd(data_JUNIOR_SCHOOL1_RANK2$TEST))
# D = 1, p-value = 0.004739
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK2$TEST)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK2$TEST
# W = 0.99566, p-value = 0.2961
#conclusion: normal 
ggplot(data_JUNIOR_SCHOOL1_RANK2) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK2_M4CID0$TEST)#53
median(data_JUNIOR_SCHOOL1_RANK2_M4CID1$TEST)#52
wilcox.test(data_JUNIOR_SCHOOL1_RANK2_M4CID0$TEST,data_JUNIOR_SCHOOL1_RANK2_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK2_M4CID0$TEST and data_JUNIOR_SCHOOL1_RANK2_M4CID1$TEST
# W = 22974, p-value = 0.2291
# alternative hypothesis: true location shift is greater than 0

# LAB
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID0$LAB,190))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID1$LAB,190))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.LAB..190.,sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.LAB..190.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.LAB..190. and sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.LAB..190.
# z = 0.7787, p-value = 0.4362
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.03905078 
# sample are independent
hist(data_JUNIOR_SCHOOL1_RANK2$LAB)
qqnorm(data_JUNIOR_SCHOOL1_RANK2$LAB)
qqline(data_JUNIOR_SCHOOL1_RANK2$LAB)
ks.test(data_JUNIOR_SCHOOL1_RANK2$LAB,dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$LAB),sd(data_JUNIOR_SCHOOL1_RANK2$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK2$LAB and dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$LAB), sd(data_JUNIOR_SCHOOL1_RANK2$LAB))
# D = 0.99525, p-value = 0.01422
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK2$LAB)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK2$LAB
# W = 0.96133, p-value = 4.451e-09
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK2) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK2_M4CID0$LAB)#51
median(data_JUNIOR_SCHOOL1_RANK2_M4CID1$LAB)#61
wilcox.test(data_JUNIOR_SCHOOL1_RANK2_M4CID0$LAB,data_JUNIOR_SCHOOL1_RANK2_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK2_M4CID0$LAB and data_JUNIOR_SCHOOL1_RANK2_M4CID1$LAB
# W = 13807, p-value = 1.774e-11
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID0$BEHAV,190))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID1$BEHAV,190))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.BEHAV..190.,sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.BEHAV..190.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.BEHAV..190. and sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.BEHAV..190.
# z = 0.13252, p-value = 0.8946
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.006618641 
# sample are independent
hist(data_JUNIOR_SCHOOL1_RANK2$BEHAV)
qqnorm(data_JUNIOR_SCHOOL1_RANK2$BEHAV)
qqline(data_JUNIOR_SCHOOL1_RANK2$BEHAV)
ks.test(data_JUNIOR_SCHOOL1_RANK2$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$BEHAV),sd(data_JUNIOR_SCHOOL1_RANK2$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK2$BEHAV and dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$BEHAV), sd(data_JUNIOR_SCHOOL1_RANK2$BEHAV))
# D = 1, p-value = 0.00237
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK2$BEHAV)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK2$BEHAV
# W = 0.98023, p-value = 1.663e-05
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK2) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK2_M4CID0$BEHAV)#80
median(data_JUNIOR_SCHOOL1_RANK2_M4CID1$BEHAV)#72
wilcox.test(data_JUNIOR_SCHOOL1_RANK2_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_RANK2_M4CID1$BEHAV,alternative = "greater")
# # Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK2_M4CID0$BEHAV and data_JUNIOR_SCHOOL1_RANK2_M4CID1$BEHAV
# W = 29278, p-value = 3.198e-09
# alternative hypothesis: true location shift is greater than 0

#CLASS
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID0$CLASS,190))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK2_M4CID1$CLASS,190))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.CLASS..190.,sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.CLASS..190.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID0.CLASS..190. and sample2$sample.data_JUNIOR_SCHOOL1_RANK2_M4CID1.CLASS..190.
# z = 0.19511, p-value = 0.8453
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.00955281 
hist(data_JUNIOR_SCHOOL1_RANK2$CLASS)
qqnorm(data_JUNIOR_SCHOOL1_RANK2$CLASS)
qqline(data_JUNIOR_SCHOOL1_RANK2$CLASS)
ks.test(data_JUNIOR_SCHOOL1_RANK2$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$CLASS),sd(data_JUNIOR_SCHOOL1_RANK2$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK2$CLASS and dnorm(mean(data_JUNIOR_SCHOOL1_RANK2$CLASS), sd(data_JUNIOR_SCHOOL1_RANK2$CLASS))
# D = 1, p-value = 0.004739
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK2$CLASS)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK2$CLASS
# W = 0.96837, p-value = 6.776e-08
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK2) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK2_M4CID0$CLASS)#57
median(data_JUNIOR_SCHOOL1_RANK2_M4CID1$CLASS)#58
wilcox.test(data_JUNIOR_SCHOOL1_RANK2_M4CID0$CLASS,data_JUNIOR_SCHOOL1_RANK2_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK2_M4CID0$CLASS and data_JUNIOR_SCHOOL1_RANK2_M4CID1$CLASS
# W = 20174, p-value = 0.06598
# alternative hypothesis: true location shift is less than 0

#RANK 3
data_JUNIOR_SCHOOL1_RANK3 <- filter(data_JUNIOR_SCHOOL1,RANK=="3") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK3_M4CID0 <- filter(data_JUNIOR_SCHOOL1,RANK=="3"& M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_JUNIOR_SCHOOL1_RANK3_M4CID1 <- filter(data_JUNIOR_SCHOOL1,RANK=="3"& M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
length(data_JUNIOR_SCHOOL1_RANK3_M4CID0$ID)#67
length(data_JUNIOR_SCHOOL1_RANK3_M4CID1$ID)#118

# TEST
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID0$TEST,60))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID1$TEST,60))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.TEST..60.,sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.TEST..60.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.TEST..60. and sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.TEST..60.
# z = 1.0292, p-value = 0.3034
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.09327927 
# independent
hist(data_JUNIOR_SCHOOL1_RANK3$TEST)
qqnorm(data_JUNIOR_SCHOOL1_RANK3$TEST)
qqline(data_JUNIOR_SCHOOL1_RANK3$TEST)
ks.test(data_JUNIOR_SCHOOL1_RANK3$TEST,dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$TEST),sd(data_JUNIOR_SCHOOL1_RANK3$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK3$TEST and dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$TEST), sd(data_JUNIOR_SCHOOL1_RANK3$TEST))
# D = 1, p-value = 0.01075
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK3$TEST)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK3$TEST
# W = 0.98645, p-value = 0.07242
#conclusion: normal 
ggplot(data_JUNIOR_SCHOOL1_RANK3) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK3_M4CID0$TEST)#79
median(data_JUNIOR_SCHOOL1_RANK3_M4CID1$TEST)#81
wilcox.test(data_JUNIOR_SCHOOL1_RANK3_M4CID0$TEST,data_JUNIOR_SCHOOL1_RANK3_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK3_M4CID0$TEST and data_JUNIOR_SCHOOL1_RANK3_M4CID1$TEST
# W = 3832.5, p-value = 0.3658
# alternative hypothesis: true location shift is less than 0

# LAB
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID0$LAB,60))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID1$LAB,60))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.LAB..60.,sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.LAB..60.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.LAB..60. and sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.LAB..60.
# z = -0.96524, p-value = 0.3344
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.08740956 
hist(data_JUNIOR_SCHOOL1_RANK3$LAB)
qqnorm(data_JUNIOR_SCHOOL1_RANK3$LAB)
qqline(data_JUNIOR_SCHOOL1_RANK3$LAB)
ks.test(data_JUNIOR_SCHOOL1_RANK3$LAB,dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$LAB),sd(data_JUNIOR_SCHOOL1_RANK3$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK3$LAB and dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$LAB), sd(data_JUNIOR_SCHOOL1_RANK3$LAB))
# D = 1, p-value = 0.01075
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK3$LAB)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK3$LAB
# W = 0.94974, p-value = 4.06e-06
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK3) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK3_M4CID0$LAB)#73
median(data_JUNIOR_SCHOOL1_RANK3_M4CID1$LAB)#81
wilcox.test(data_JUNIOR_SCHOOL1_RANK3_M4CID0$LAB,data_JUNIOR_SCHOOL1_RANK3_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK3_M4CID0$LAB and data_JUNIOR_SCHOOL1_RANK3_M4CID1$LAB
# W = 2343.5, p-value = 2.107e-06
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID0$BEHAV,60))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID1$BEHAV,60))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.BEHAV..60.,sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.BEHAV..60.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.BEHAV..60. and sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.BEHAV..60.
# z = 0.56529, p-value = 0.5719
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.05224903 
# sample independent
hist(data_JUNIOR_SCHOOL1_RANK3$BEHAV)
qqnorm(data_JUNIOR_SCHOOL1_RANK3$BEHAV)
qqline(data_JUNIOR_SCHOOL1_RANK3$BEHAV)
ks.test(data_JUNIOR_SCHOOL1_RANK3$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$BEHAV),sd(data_JUNIOR_SCHOOL1_RANK3$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK3$BEHAV and dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$BEHAV), sd(data_JUNIOR_SCHOOL1_RANK3$BEHAV))
# D = 1, p-value = 0.005376
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_RANK3$BEHAV)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK3$BEHAV
# W = 0.91126, p-value = 4.092e-09
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK3) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK3_M4CID0$BEHAV)#94
median(data_JUNIOR_SCHOOL1_RANK3_M4CID1$BEHAV)#91
wilcox.test(data_JUNIOR_SCHOOL1_RANK3_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_RANK3_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK3_M4CID0$BEHAV and data_JUNIOR_SCHOOL1_RANK3_M4CID1$BEHAV
# W = 4871.5, p-value = 0.004318
# alternative hypothesis: true location shift is greater than 0

#CLASS
# independency
sample1<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID0$CLASS,60))
sample2<-data.frame(sample(data_JUNIOR_SCHOOL1_RANK3_M4CID1$CLASS,60))
#Kendall correlation test
cor.test(sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.CLASS..60.,sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.CLASS..60.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID0.CLASS..60. and sample2$sample.data_JUNIOR_SCHOOL1_RANK3_M4CID1.CLASS..60.
# z = -0.43386, p-value = 0.6644
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.03859266 
# sample independent
hist(data_JUNIOR_SCHOOL1_RANK3$CLASS)
qqnorm(data_JUNIOR_SCHOOL1_RANK3$CLASS)
qqline(data_JUNIOR_SCHOOL1_RANK3$CLASS)
ks.test(data_JUNIOR_SCHOOL1_RANK3$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$CLASS),sd(data_JUNIOR_SCHOOL1_RANK3$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_RANK3$CLASS and dnorm(mean(data_JUNIOR_SCHOOL1_RANK3$CLASS), sd(data_JUNIOR_SCHOOL1_RANK3$CLASS))
# D = 1, p-value = 0.01075
# alternative hypothesis: two-sided
# shapiro.test(data_JUNIOR_SCHOOL1_RANK3$CLASS)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_RANK3$CLASS
# W = 0.93836, p-value = 4.123e-07
#conclusion: not normal 
ggplot(data_JUNIOR_SCHOOL1_RANK3) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_RANK3_M4CID0$CLASS)#77
median(data_JUNIOR_SCHOOL1_RANK3_M4CID1$CLASS)#81
wilcox.test(data_JUNIOR_SCHOOL1_RANK3_M4CID0$CLASS,data_JUNIOR_SCHOOL1_RANK3_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_RANK3_M4CID0$CLASS and data_JUNIOR_SCHOOL1_RANK3_M4CID1$CLASS
# W = 3088.5, p-value = 0.006789
# alternative hypothesis: true location shift is less than 0

# Table 17: Inference for junior school 1, RANK 1, 2, 3

#Conclusion
# RANK 1, 2 and 3, same results: no effect for TEST and CLASS, negative for BEHAV, and positive on LAB

########## GENDER
#This analysis uses school 1 and junior grade sample
data_junior_SCHOOL1<-filter(data,GRADE <="2" & SCHOOL=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_FEMALE<-filter(data,GRADE<="2" & SCHOOL=="1",GENDER=="F") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_MALE<-filter(data,GRADE<="2" & SCHOOL=="1",GENDER=="M") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_M4CID0_MALE<-filter(data,GRADE <="2" & SCHOOL=="1" & M4CID =="0"  & GENDER == "M") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_M4CID0_FEMALE<-filter(data,GRADE<="2" & SCHOOL=="1" & M4CID =="0" & GENDER == "F") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_M4CID1_MALE<-filter(data, GRADE <="2" & SCHOOL=="1" & M4CID =="1"  & GENDER == "M") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_M4CID1_FEMALE<-filter(data,GRADE <="2" & SCHOOL=="1" & M4CID =="1" & GENDER == "F") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

summary(data_junior_SCHOOL1)
length(data_junior_SCHOOL1$ID)#683
length(which(data_junior_SCHOOL1$GENDER=="F"))#326
length(which(data_junior_SCHOOL1$GENDER=="M"))#357
length(which(data_junior_SCHOOL1$GENDER=="F"))/length(data_junior_SCHOOL1$ID)#48%
length(which(data_junior_SCHOOL1$GENDER=="M"))/length(data_junior_SCHOOL1$ID)#52%
length(data_junior_SCHOOL1_M4CID0_FEMALE$ID)#160
length(data_junior_SCHOOL1_M4CID1_FEMALE$ID)#166
length(data_junior_SCHOOL1_M4CID0_MALE$ID)#158
length(data_junior_SCHOOL1_M4CID1_MALE$ID)#199

#sample entries: 783; Female = 326 (48%); Male=357 52%();
# female: M4CID0 = 160; M4CID1=166
# male: M4CID0 = 158; M4CID1=199
#data sample not normal but independent

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
ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=TEST, linetype = factor(GENDER)))
median(data_junior_SCHOOL1_M4CID0_FEMALE$TEST)#54
median(data_junior_SCHOOL1_M4CID1_FEMALE$TEST)#61
median(data_junior_SCHOOL1_M4CID0_MALE$TEST)#54
median(data_junior_SCHOOL1_M4CID1_MALE$TEST)#57
#It seems that the female benefit, less impact on male
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$TEST,data_junior_SCHOOL1_M4CID1_MALE$TEST, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$TEST and data_junior_SCHOOL1_M4CID1_MALE$TEST
# W = 14188, p-value = 0.05675
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$TEST,data_junior_SCHOOL1_M4CID1_FEMALE$TEST, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$TEST and data_junior_SCHOOL1_M4CID1_FEMALE$TEST
# W = 10924, p-value = 0.002804
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
median(data_junior_SCHOOL1_M4CID0_FEMALE$LAB)#50
median(data_junior_SCHOOL1_M4CID1_FEMALE$LAB)#70
median(data_junior_SCHOOL1_M4CID0_MALE$LAB)#53
median(data_junior_SCHOOL1_M4CID1_MALE$LAB)#61
#It seems that the female benefit, less impact on male
ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=LAB, linetype = factor(GENDER)))
#It seems that both male/female benefit
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$LAB,data_junior_SCHOOL1_M4CID1_MALE$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$LAB and data_junior_SCHOOL1_M4CID1_MALE$LAB
# W = 11162, p-value = 1.249e-06
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$LAB,data_junior_SCHOOL1_M4CID1_FEMALE$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$LAB and data_junior_SCHOOL1_M4CID1_FEMALE$LAB
# W = 5499.5, p-value < 2.2e-16
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
median(data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV)#82
median(data_junior_SCHOOL1_M4CID0_MALE$BEHAV)#80
median(data_junior_SCHOOL1_M4CID1_MALE$BEHAV)#72
ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=BEHAV, linetype = factor(GENDER)))
#It seems that the male/female don't benefit, male is worse
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$BEHAV,data_junior_SCHOOL1_M4CID1_MALE$BEHAV, alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$BEHAV and data_junior_SCHOOL1_M4CID1_MALE$BEHAV
# W = 19768, p-value = 1.463e-05
# alternative hypothesis: true location shift is greater than 0
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV,data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV, alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV and data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV
# W = 13459, p-value = 0.8337
# alternative hypothesis: true location shift is not equal to 0

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
median(data_junior_SCHOOL1_M4CID0_FEMALE$CLASS)#58
median(data_junior_SCHOOL1_M4CID1_FEMALE$CLASS)#67
median(data_junior_SCHOOL1_M4CID0_MALE$CLASS)#58
median(data_junior_SCHOOL1_M4CID1_MALE$CLASS)#60
ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=CLASS, linetype = factor(GENDER)))
#It seems that the female have more benefit then the male
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$CLASS,data_junior_SCHOOL1_M4CID1_MALE$CLASS, alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$CLASS and data_junior_SCHOOL1_M4CID1_MALE$CLASS
# W = 13812, p-value = 0.04883
# alternative hypothesis: true location shift is not equal to 0
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$CLASS,data_junior_SCHOOL1_M4CID1_FEMALE$CLASS, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$CLASS and data_junior_SCHOOL1_M4CID1_FEMALE$CLASS
# W = 8602.5, p-value = 1.925e-08
# alternative hypothesis: true location shift is less than 0

# ???Figure 15: junior sample boxplots for GENDER analysis 
# ???grid.arrange(p60, p61, p62, p63,  nrow = 2,top="Box plot 4C/ID treatment effect", bottom="Figure 15: junior sample boxplots for GENDER analysis")

# Table 18: Inference sample junior school 1 for GENDER

# Results
# Test no impact for male; female positive impact
# Both benefit in LAB variable
# In the BEHAV variable negative impact for MALE and no impact for FEMALE
# positive impact on female Class variable but no impact for the male
# Female, averall, benefit more: positive in TEST, LAB and CLASS, no effect on BEHAV
# MALE: positive impact on LAB and CLASS, negative on BEHAV and no impact on TEST

########## Summary results and notes UPDATE THIS

#2003 to 2017
# for changes that have p-value that we can't reject the null I use the mean value
results <- data.frame(
  samples=c("JUNIOR school effect","all sample", "school 1","JUNIOR SCHOOL 1","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"),
  TEST1=c(67,-62, 54,54 ,56 ,54 ,57 ,33 ,53,80,54,55),
  TEST2=c(70,-57, 57,58 ,56 ,58 ,57 ,33 ,53,80,61,55),
  LAB1=c(63,60, 51,51 ,56 ,51 ,44 ,26 ,51,73,50,53),
  LAB2=c(70,65, 65,66 ,69 ,62 ,66 ,35 ,61,81,70,61),
  BEHAV1=c(-77,-79, -80,-81,-77,-85,80 ,-65 ,-80,-94,82,-80),
  BEHAV2=c(-73,-76, -76,-76,-71,-60,85 ,-54 ,-72,-91,82,-72),
  CLASS1=c(63, 58,59 ,60 ,50 ,55 ,39 ,57,77,58,58),
  CLASS2=c(63, 62,62 ,64 ,61 ,64 ,39 ,57,81,67,60)
)

#2003 to 2019
results <- data.frame(
  samples=c("Junior/junior high (both schs. sch. 1 only with 4C/ID)","Junior (school effect) sch. 1 only with 4C/ID)","Junior (school effect)","All sample (school effect)","All sample","SCHOOL 1 junior and high junior","JUNIOR SCHOOL 1","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"),
  TEST1=c(-68,0,0,0,-62,0,54,0,54,0,-34,0,0,54,0),
  TEST2=c(-57,0,0,0,-57,0,58,0,58,0,-29,0,0,56,0),
  LAB1=c(0,62,63,63,60,53,53,56,55,44,26,51,73,52,53),
  LAB2=c(0,77,77,74,63,63,66,74,68,66,35,62,80,70,72),
  BEHAV1=c(-68,-77,-70,-78,-79,-81,-81,-77,-85,80,-65,-80,-94,0,-81),
  BEHAV2=c(-57,-71,-71,-70,-73,-73,-74,-71,-60,85,-50,-67,-90,0,-66),
  CLASS1=c(-69,0,0,0,-64,59,59,0,0,55,-39, 0,77,59,0),
  CLASS2=c(-61,0,0,0,-61,61,62,0,0,64,-37, 0,81,67,0)
)

ggplot(results) +
  geom_errorbar(aes(x=factor(samples,levels = c("Junior/junior high (both schs. sch. 1 only with 4C/ID)","Junior (school effect) sch. 1 only with 4C/ID)","Junior (school effect)","All sample (school effect)","All sample","SCHOOL 1 junior and high junior","JUNIOR SCHOOL 1","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE")), ymin=TEST1, ymax=TEST2,color='TEST'),width=0.8,alpha=1, linewidth=1) +
  geom_errorbar(aes(x=samples, ymin=LAB1, ymax=LAB2,color='LAB'), width=0.4, alpha=1, linewidth=1) +
  geom_errorbar(aes(x=samples, ymin=BEHAV1, ymax=BEHAV2, color='BEHAV'), width=0.4, alpha=1, linewidth=1) +
  geom_errorbar(aes(x=samples, ymin=CLASS1, ymax=CLASS2, color='CLASS'), width=0.4, alpha=1, linewidth=1) +
  xlab("Samples")+labs(title="4C/ID treatment effect resume results",y="4C/ID negative effect, no effect, 4C/ID positive effect")+
  scale_color_manual(name='Variables',
                     breaks=c('TEST','LAB', 'BEHAV', 'CLASS'),
                     values=c('TEST'='red', 'LAB'='blue', 'BEHAV'='green', 'CLASS'='yellow'))+
  
  coord_flip(ylim = c(-100,100))

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
legend: https://www.statology.org/ggplot-manual-legend/
  
??? Van Merriënboer, J. J. G., & Kester, L. (2008). Whole-task models in education. 
In J. M. Spector, M. D. Merrill, J. J. G. van Merriënboer, & M. P. Driscoll (Eds.), 
Handbook of research on educational communications and technology (3rd ed, pp. 441–456). 
Mahwah, NJ: Erlbaum/Routledge.


# BOOKS

blueprint 4cdi
stats...


