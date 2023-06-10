#INDEX
# GOALS - 18
# HYPOTHESIS - 27
# DATA - Exploratory data analysis (EDA) - 30
  # DATA summary - 40
  # Resume - 288
  # DATA variables description - 326
  # DATA structure - 341
  # NORMALITY - 538
# REFERENCES - 717

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
# SD TEST, 17; LAB: 19; BEHAH: 15; CLASS: 14: data with heavy tails

# Junior and Junior high 
summary(data)
length(data$ID)
length(which(data$GRADE<="2"))
length(which(data$GRADE<="2"))/length(data$ID)
length(which(data$GRADE>"2"))
length(which(data$GRADE>"2"))/length(data$ID)

data_M4CID0<- filter(data,M4CID=="0") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_M4CID0$ID)#M4CID 0: 828

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
#M4CID 0: 828
# SD TEST: 17; LAB 20; BEHAV: 14; CLASS: 14;

data_M4CID1<- filter(data,M4CID=="1") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_M4CID1$ID)
#M4CID 0: 586

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
#M4CID 1: 586
# SD TEST: 17; LAB: 16; BEHAV: 16; CLASS: 14; 

data_SCHOOL0<- filter(data,SCHOOL=="0") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_SCHOOL0$ID)
# SCHOOL 0: 476

summary(data_SCHOOL0)
# ID              DATE          SCHOOL     GENDER         
# Min.   : 477.0   Min.   :2009   Min.   :1   Length:938        
# 1st Qu.: 711.2   1st Qu.:2011   1st Qu.:1   Class :character  
# Median : 945.5   Median :2013   Median :1   Mode  :character  
# Mean   : 945.5   Mean   :2013   Mean   :1                     
# 3rd Qu.:1179.8   3rd Qu.:2016   3rd Qu.:1                     
# Max.   :1414.0   Max.   :2017   Max.   :1                     
# TEST             LAB             BEHAV            GRADE     
# Min.   :  0.00   Min.   :  0.00   Min.   : 27.00   Min.   :0.00  
# 1st Qu.: 46.00   1st Qu.: 46.00   1st Qu.: 66.00   1st Qu.:1.00  
# Median : 55.00   Median : 60.00   Median : 77.50   Median :2.00  
# Mean   : 57.36   Mean   : 58.68   Mean   : 76.12   Mean   :2.11  
# 3rd Qu.: 69.00   3rd Qu.: 73.00   3rd Qu.: 88.00   3rd Qu.:3.00  
# Max.   :100.00   Max.   :100.00   Max.   :100.00   Max.   :6.00  
# M4CID            CLASS       
# Min.   :0.0000   Min.   : 14.20  
# 1st Qu.:0.0000   1st Qu.: 52.00  
# Median :1.0000   Median : 60.20  
# Mean   :0.6247   Mean   : 61.51  
# 3rd Qu.:1.0000   3rd Qu.: 70.60  
# Max.   :1.0000   Max.   :100.00  

sd(data_SCHOOL0$TEST)
sd(data_SCHOOL0$LAB)
sd(data_SCHOOL0$BEHAV)
sd(data_SCHOOL0$CLASS)
# SCHOOL 0: 476
#SD TEST 15; LAB: 17; BEHAV 13; CLASS: 12

data_SCHOOL1<- filter(data,SCHOOL=="1") %>% select(ID,DATE,SCHOOL,GENDER,TEST,LAB,BEHAV,GRADE,M4CID,CLASS)
length(data_SCHOOL1$ID)
# SCHOOL 1: 938

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
# SCHOOL 1: 938
# SD TEST: 17; LAB 20; BEHAV: 15; CLASS: 14

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
# female/male sample: 602/812, 43%/57%: no balanced GENDER

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
# Junior 1159; SCHOOL 0, M4CID = 1: 476 (41%);
# SCHOOL 1 - JUNIOR: 683 (59%); M4CID = 0: 318 (27%) ; M4CID = 1: 365 (31%)

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
# Total sample data length 1414, 1159 Junior, 82%, Junior high 255, 18%
# all sample with 4C/ID without 4C/ID
# sample without 4CID: 828, 59%; with 4CID: 586, 41%: more data with 4CID methodology, not balanced
# school 0, 476, 34% and school 1, 938, 66%
# female/male sample: 602/812, 43%/57%: no balanced GENDER
# High Junior (secondary), no 4CID, 34, 13%; with 4CID, 221, 86% poorly balanced
# SD TEST, 17; LAB: 19; BEHAH: 15; CLASS: 14: data with heavy tails
# sample entries secondary 255, 18% of the global sample
#it seems good to use secondary grade data for all sample inference
# Gender Junior: total, 1159; female 576 (49%); male 583 (50%)
# Junior 1159; SCHOOL 0, M4CID = 1, - 476 (41%);
# SCHOOL 1 - JUNIOR: 683 (59%); M4CID = 0: 318 (27%) ; M4CID = 1: 365 (31%)
# Grade 0 (7th) - 353 (25%)
# Grade 1 (8th) - 411 (29%)
# Grade 2 (9th) - 396 (28%)
# Grade 3 (10th) - 27 (2%)
# Grade 4 (11th) - 18 (1%)
# Grade 5 (10technical) - 117 (8%)
# Grade 6 (11technical) - 93 (7%)
# very low sample entries levels 10/11 grade general and techinal edu path
# female/male sample: 602/812, 43%/57%: no balanced GENDER
# SCHOOL 1: 938
# SD TEST: 17; LAB 20; BEHAV: 15; CLASS: 14
# SCHOOL 0: 476
#SD TEST 15; LAB: 17; BEHAV 13; CLASS: 12
#M4CID 0: 828
# SD TEST: 17; LAB 20; BEHAV: 14; CLASS: 14;
#M4CID 1: 586
# SD TEST: 17; LAB: 16; BEHAV: 16; CLASS: 14; 

# Conclusion: From this analysis it seems to be possible comparing not only all the sample but also,
# just the Junior: this sample is more balanced in terms of grades and gender. The secondary is not balanced but it\
# can be used fro global sample inference. Junior sample its a better one.

#variables description
#ID: Identification entry row data;
#DATE: year of the sample: it belongs, actually to 2003/2004, lective year;
#SCHOOL: there are two schools, identified by 0 (till 2008) and 1 (after 2009);
# GENDER: F and M
# TEST, LAB, BEHAV: variables of the score in the tests and observational forms at LAB (Laboratory practical skills assessment)
# and BEHAV related to accomplishement school rules behavior
# LAB work and Behavior, normally if they follow the rules stablished at school
# GRADE: categories 0 to 6, representing, respectively, 7,8,9,10,11, 10p(techical) and 11p (Technical) 
# M4CID, category 0 and 1, respectively, without 4C/ID and with 4CID;
# CLASS, 0 to 100, height average, CLASS=0.5*TEST+0.3*LAB+0.2*BEHAV: 
#this will be used in a variable to be defined below, RANK: a study for student addaptation to the school system

#DATA structure

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
# POWER: https://www.programmingr.com/examples/neat-tricks/sample-r-function/how-to-seize-pwr-statistical-power-analysis-in-r/

# About R
# https://tidyverse.github.io/ggplot2-docs/index.html
# https://r-graph-gallery.com/index.html
# https://ggplot2.tidyverse.org/index.html
# https://r-charts.com/ggplot2/axis/

# ARTICLES 

'Van Merrienboer, Jeroen J. G. & Clark, Richard & Croock, Marcel. (2002). Blueprints for complex learning: The 4C/ID-model. Educational Technology Research and Development. 50. 39-61. 10.1007/BF02504993. This article provides an overview description of the four-component instructional design system (4C/ID-model) developed originally
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


