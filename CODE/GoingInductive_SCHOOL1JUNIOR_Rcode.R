#INDEX
# RESULTS 2003:2007 - 54
  ## SCHOOL 1 - 54
  ## SCHOOL 1 JUNIOR - 201
  ## GRADE - 344
  ## RANK - 804
  ## GENDER - 1372
# RESULTS 2003:2014 - 1625 
  ## SCHOOL 1 - 1650
  ## SCHOOL 1 JUNIOR - 1805
  ## GRADE - 
  ## RANK - 
  ## GENDER - 

# libraries 
library(dplyr) #work with data frames
#library(tidyverse) #include dplyr
library(ggplot2) # graphs
library(gridExtra) #arrange graphs in rows
library("Kendall")
library("jmuOutlier")

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

# SCHOOL 1
# Inference only for School 1: 2009-2017
# off course, sample not computed
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
# not normal, sample independent
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
median(data_JUNIOR_SCHOOL1_M4CID0$CLASS)#58
median(data_JUNIOR_SCHOOL1_M4CID1$CLASS)#63
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
# 4CID has no impact on TEST, positive on LAB and CLASS Variables;
# 4CID has a negative impact in BEHAV variable

#GRADE 8
# Note: M4CID=="1",less then 100 entrances
# 4CID has a positive impact on TEST, LAB and CLASS
# 4CID negative on BEHAV Variable;

#Grade 9
# 4CID has no impact on TEST Variable;
# 4CID has positive impact in LAB, BEHAV and CLASS Variable;

########## RANK

#This analysis uses school 1 junior: 2009:2017
summary(data)
data_SCHOOL1_JUNIOR<-filter(data,SCHOOL == "1" & GRADE <=2) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_SCHOOL1_JUNIOR)
data_SCHOOL1_JUNIOR$RANK <-
  ifelse(data_SCHOOL1_JUNIOR$CLASS <=45, 1,
         ifelse((data_SCHOOL1_JUNIOR$CLASS >45 & data_SCHOOL1_JUNIOR$CLASS<70), 2,
                ifelse(data_SCHOOL1_JUNIOR$CLASS >=70, 3,NA)))
summary(data_SCHOOL1_JUNIOR)
length(data_SCHOOL1_JUNIOR$ID)#9683 entrances
length(which(data_SCHOOL1_JUNIOR$RANK=="1"))#77
length(which(data_SCHOOL1_JUNIOR$RANK=="1"))/length(data_SCHOOL1_JUNIOR$ID)#11%
length(which(data_SCHOOL1_JUNIOR$RANK=="2"))#421
length(which(data_SCHOOL1_JUNIOR$RANK=="2"))/length(data_SCHOOL1_JUNIOR$ID)#62%
length(which(data_SCHOOL1_JUNIOR$RANK=="3"))#185
length(which(data_SCHOOL1_JUNIOR$RANK=="3"))/length(data_SCHOOL1_JUNIOR$ID)#27%

#RANK 1, 77,11%
#RANK 2, 421, 62%
#RANK 3, 185, 27% 

#RANK 1
data_SCHOOL1_JUNIOR_RANK1 <- filter(data_SCHOOL1_JUNIOR,RANK=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_RANK1_M4CID0 <- filter(data_SCHOOL1_JUNIOR_RANK1, M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_RANK1_M4CID1 <- filter(data_SCHOOL1_JUNIOR_RANK1, M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_SCHOOL1_JUNIOR_RANK1_M4CID0)
length(data_SCHOOL1_JUNIOR_RANK1_M4CID0$ID)#55
length(data_SCHOOL1_JUNIOR_RANK1_M4CID1$ID)#22
#note: under 50 sample entrances: low sample: permutation test

# TEST
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK1_M4CID0$TEST,20))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK1_M4CID1$TEST,20))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID0.TEST..20.,sample2$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID1.TEST..20.,method="kendall",exact = FALSE)
Kendall(sample1$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID0.TEST..20.,sample2$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID1.TEST..20.)
length(sample1$TEST)
#Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID0.TEST..20. and sample2$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID1.TEST..20.
# z = 0.7897, p-value = 0.4297
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.104641 
# sample independent
hist(data_SCHOOL1_JUNIOR_RANK1$TEST)
qqnorm(data_SCHOOL1_JUNIOR_RANK1$TEST)
qqline(data_SCHOOL1_JUNIOR_RANK1$TEST)
ks.test(data_SCHOOL1_JUNIOR_RANK1$TEST,dnorm(mean(data_SCHOOL1_JUNIOR_RANK1$TEST),sd(data_SCHOOL1_JUNIOR_RANK1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK1$TEST and dnorm(mean(data_SCHOOL1_JUNIOR_RANK1$TEST), sd(data_SCHOOL1_JUNIOR_RANK1$TEST))
# D = 1, p-value = 0.02564
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK1$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK1$TEST
# W = 0.97701, p-value = 0.1764
#conclusion: normal
var(data_SCHOOL1_JUNIOR_RANK1_M4CID0$TEST)
var(data_SCHOOL1_JUNIOR_RANK1_M4CID1$TEST)
hist(data_SCHOOL1_JUNIOR_RANK1_M4CID0$TEST)
hist(data_SCHOOL1_JUNIOR_RANK1_M4CID1$TEST)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK1_M4CID0$TEST)#33
median(data_SCHOOL1_JUNIOR_RANK1_M4CID1$TEST)#34
wilcox.test(data_SCHOOL1_JUNIOR_RANK1_M4CID0$TEST,data_SCHOOL1_JUNIOR_RANK1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_RANK1_M4CID0$TEST and data_SCHOOL1_JUNIOR_RANK1_M4CID1$TEST
# W = 725, p-value = 0.9131
# alternative hypothesis: true location shift is not equal to 0
perm.test(data_SCHOOL1_JUNIOR_RANK1_M4CID0$TEST,data_SCHOOL1_JUNIOR_RANK1_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.0688

# LAB
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK1_M4CID0$LAB,20))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK1_M4CID1$LAB,20))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID0.LAB..20.,sample2$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID1.LAB..20.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID0.LAB..20. and sample2$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID1.LAB..20.
# z = -0.92261, p-value = 0.3562
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1239852 
#sample independent
hist(data_SCHOOL1_JUNIOR_RANK1$LAB)
qqnorm(data_SCHOOL1_JUNIOR_RANK1$LAB)
qqline(data_SCHOOL1_JUNIOR_RANK1$LAB)
ks.test(data_SCHOOL1_JUNIOR_RANK1$LAB,dnorm(mean(data_SCHOOL1_JUNIOR_RANK1$LAB),sd(data_SCHOOL1_JUNIOR_RANK1$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK1$LAB and dnorm(mean(data_SCHOOL1_JUNIOR_RANK1$LAB), sd(data_SCHOOL1_JUNIOR_RANK1$LAB))
# D = 0.85714, p-value = 0.3077
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK1$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK1$LAB
# W = 0.93202, p-value = 0.0004847
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_RANK1_M4CID0$LAB)
var(data_SCHOOL1_JUNIOR_RANK1_M4CID1$LAB)
hist(data_SCHOOL1_JUNIOR_RANK1_M4CID0$LAB)
hist(data_SCHOOL1_JUNIOR_RANK1_M4CID1$LAB)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK1_M4CID0$LAB)#26
median(data_SCHOOL1_JUNIOR_RANK1_M4CID1$LAB)#35
wilcox.test(data_SCHOOL1_JUNIOR_RANK1_M4CID0$LAB,data_SCHOOL1_JUNIOR_RANK1_M4CID1$LAB,alternative = "less")
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK1$LAB
# W = 346, p-value = 0.001746
perm.test(data_SCHOOL1_JUNIOR_RANK1_M4CID0$LAB,data_SCHOOL1_JUNIOR_RANK1_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.0015

# BEHAV
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK1_M4CID0$BEHAV,20))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK1_M4CID1$BEHAV,20))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID0.BEHAV..20.,sample2$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID1.BEHAV..20.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID0.BEHAV..20. and sample2$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID1.BEHAV..20.
# z = -1.3245, p-value = 0.1853
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1739132 
#sample independent
hist(data_SCHOOL1_JUNIOR_RANK1$BEHAV)
qqnorm(data_SCHOOL1_JUNIOR_RANK1$BEHAV)
qqline(data_SCHOOL1_JUNIOR_RANK1$BEHAV)
ks.test(data_SCHOOL1_JUNIOR_RANK1$BEHAV,dnorm(mean(data_SCHOOL1_JUNIOR_RANK1$BEHAV),sd(data_SCHOOL1_JUNIOR_RANK1$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK1$BEHAV and dnorm(mean(data_SCHOOL1_JUNIOR_RANK1$BEHAV), sd(data_SCHOOL1_JUNIOR_RANK1$BEHAV))
# D = 1, p-value = 0.02564
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK1$BEHAV
# W = 0.98835, p-value = 0.7112
#conclusion: normal
var(data_SCHOOL1_JUNIOR_RANK1_M4CID0$BEHAV)
var(data_SCHOOL1_JUNIOR_RANK1_M4CID1$BEHAV)
hist(data_SCHOOL1_JUNIOR_RANK1_M4CID0$BEHAV)
hist(data_SCHOOL1_JUNIOR_RANK1_M4CID1$BEHAV)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK1_M4CID0$BEHAV)#65
median(data_SCHOOL1_JUNIOR_RANK1_M4CID1$BEHAV)#54
wilcox.test(data_SCHOOL1_JUNIOR_RANK1_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_RANK1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_RANK1_M4CID0$BEHAV and data_SCHOOL1_JUNIOR_RANK1_M4CID1$BEHAV
# W = 866.5, p-value = 0.001615
# alternative hypothesis: true location shift is greater than 0
perm.test(data_SCHOOL1_JUNIOR_RANK1_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_RANK1_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.00685

# CLASS
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK1_M4CID0$CLASS,20))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK1_M4CID1$CLASS,20))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID0.CLASS..20.,sample2$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID1.CLASS..20.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID0.CLASS..20. and sample2$sample.data_SCHOOL1_JUNIOR_RANK1_M4CID1.CLASS..20.
# z = 0.62483, p-value = 0.5321
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.08083141 
hist(data_SCHOOL1_JUNIOR_RANK1$CLASS)
qqnorm(data_SCHOOL1_JUNIOR_RANK1$CLASS)
qqline(data_SCHOOL1_JUNIOR_RANK1$CLASS)
ks.test(data_SCHOOL1_JUNIOR_RANK1$CLASS,dnorm(mean(data_SCHOOL1_JUNIOR_RANK1$CLASS),sd(data_SCHOOL1_JUNIOR_RANK1$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK1$CLASS and dnorm(mean(data_SCHOOL1_JUNIOR_RANK1$CLASS), sd(data_SCHOOL1_JUNIOR_RANK1$CLASS))
# D = 1, p-value = 0.01282
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK1$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK1$CLASS
# W = 0.89273, p-value = 8.67e-06
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_RANK1_M4CID0$CLASS)
var(data_SCHOOL1_JUNIOR_RANK1_M4CID1$CLASS)
hist(data_SCHOOL1_JUNIOR_RANK1_M4CID0$CLASS)
hist(data_SCHOOL1_JUNIOR_RANK1_M4CID1$CLASS)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK1_M4CID0$CLASS)#39
median(data_SCHOOL1_JUNIOR_RANK1_M4CID1$CLASS)#39
wilcox.test(data_SCHOOL1_JUNIOR_RANK1_M4CID0$CLASS,data_SCHOOL1_JUNIOR_RANK1_M4CID1$CLASS,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_RANK1_M4CID0$CLASS and data_SCHOOL1_JUNIOR_RANK1_M4CID1$CLASS
# W = 720, p-value = 0.1966
# alternative hypothesis: true location shift is not equal to 0
perm.test(data_SCHOOL1_JUNIOR_RANK1_M4CID0$CLASS,data_SCHOOL1_JUNIOR_RANK1_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.696

#RANK 2
data_SCHOOL1_JUNIOR_RANK2 <- filter(data_SCHOOL1_JUNIOR,RANK=="2") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_RANK2_M4CID0 <- filter(data_SCHOOL1_JUNIOR_RANK2, M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_RANK2_M4CID1 <- filter(data_SCHOOL1_JUNIOR_RANK2, M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_SCHOOL1_JUNIOR_RANK2_M4CID0)
length(data_SCHOOL1_JUNIOR_RANK2_M4CID0$ID)#196
length(data_SCHOOL1_JUNIOR_RANK2_M4CID1$ID)#225

# TEST
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK2_M4CID0$TEST,190))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK2_M4CID1$TEST,190))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID0.TEST..190.,sample2$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID1.TEST..190.,method="kendall",exact = FALSE)
Kendall(sample1$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID0.TEST..190.,sample2$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID1.TEST..190.)
length(sample1$TEST)
#Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID0.TEST..190. and sample2$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID1.TEST..190.
# z = 0.7897, p-value = 0.4297
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.104641 
# sample independent
hist(data_SCHOOL1_JUNIOR_RANK2$TEST)
qqnorm(data_SCHOOL1_JUNIOR_RANK2$TEST)
qqline(data_SCHOOL1_JUNIOR_RANK2$TEST)
ks.test(data_SCHOOL1_JUNIOR_RANK2$TEST,dnorm(mean(data_SCHOOL1_JUNIOR_RANK2$TEST),sd(data_SCHOOL1_JUNIOR_RANK2$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK2$TEST and dnorm(mean(data_SCHOOL1_JUNIOR_RANK2$TEST), sd(data_SCHOOL1_JUNIOR_RANK2$TEST))
# D = 1, p-value = 0.004739
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK2$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK2$TEST
# W = 0.99566, p-value = 0.2961
#conclusion: normal
var(data_SCHOOL1_JUNIOR_RANK2_M4CID0$TEST)
var(data_SCHOOL1_JUNIOR_RANK2_M4CID1$TEST)
hist(data_SCHOOL1_JUNIOR_RANK2_M4CID0$TEST)
hist(data_SCHOOL1_JUNIOR_RANK2_M4CID1$TEST)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK2) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK2_M4CID0$TEST)#53
median(data_SCHOOL1_JUNIOR_RANK2_M4CID1$TEST)#52
wilcox.test(data_SCHOOL1_JUNIOR_RANK2_M4CID0$TEST,data_SCHOOL1_JUNIOR_RANK2_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_RANK2_M4CID0$TEST and data_SCHOOL1_JUNIOR_RANK2_M4CID1$TEST
# W = 22974, p-value = 0.2291
# alternative hypothesis: true location shift is not equal to 0
perm.test(data_SCHOOL1_JUNIOR_RANK2_M4CID0$TEST,data_SCHOOL1_JUNIOR_RANK2_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.45395

# LAB
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK2_M4CID0$LAB,190))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK2_M4CID1$LAB,190))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID0.LAB..190.,sample2$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID1.LAB..190.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID0.LAB..190. and sample2$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID1.LAB..190.
# z = -0.92261, p-value = 0.3562
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1239852 
#sample independent
hist(data_SCHOOL1_JUNIOR_RANK2$LAB)
qqnorm(data_SCHOOL1_JUNIOR_RANK2$LAB)
qqline(data_SCHOOL1_JUNIOR_RANK2$LAB)
ks.test(data_SCHOOL1_JUNIOR_RANK2$LAB,dnorm(mean(data_SCHOOL1_JUNIOR_RANK2$LAB),sd(data_SCHOOL1_JUNIOR_RANK2$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK2$LAB and dnorm(mean(data_SCHOOL1_JUNIOR_RANK2$LAB), sd(data_SCHOOL1_JUNIOR_RANK2$LAB))
# D = 0.99525, p-value = 0.01422
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK2$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK2$LAB
# W = 0.96133, p-value = 4.451e-09
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_RANK2_M4CID0$LAB)
var(data_SCHOOL1_JUNIOR_RANK2_M4CID1$LAB)
hist(data_SCHOOL1_JUNIOR_RANK2_M4CID0$LAB)
hist(data_SCHOOL1_JUNIOR_RANK2_M4CID1$LAB)
# almost simmetric, different variances, almost same shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK2) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK2_M4CID0$LAB)#51
median(data_SCHOOL1_JUNIOR_RANK2_M4CID1$LAB)#61
wilcox.test(data_SCHOOL1_JUNIOR_RANK2_M4CID0$LAB,data_SCHOOL1_JUNIOR_RANK2_M4CID1$LAB,alternative = "less")
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK2$LAB
# W = 13807, p-value = 1.774e-11
perm.test(data_SCHOOL1_JUNIOR_RANK2_M4CID0$LAB,data_SCHOOL1_JUNIOR_RANK2_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# BEHAV
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK2_M4CID0$BEHAV,190))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK2_M4CID1$BEHAV,190))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID0.BEHAV..190.,sample2$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID1.BEHAV..190.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID0.BEHAV..190. and sample2$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID1.BEHAV..190.
# z = -1.3245, p-value = 0.1853
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1739132 
#sample independent
hist(data_SCHOOL1_JUNIOR_RANK2$BEHAV)
qqnorm(data_SCHOOL1_JUNIOR_RANK2$BEHAV)
qqline(data_SCHOOL1_JUNIOR_RANK2$BEHAV)
ks.test(data_SCHOOL1_JUNIOR_RANK2$BEHAV,dnorm(mean(data_SCHOOL1_JUNIOR_RANK2$BEHAV),sd(data_SCHOOL1_JUNIOR_RANK2$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK2$BEHAV and dnorm(mean(data_SCHOOL1_JUNIOR_RANK2$BEHAV), sd(data_SCHOOL1_JUNIOR_RANK2$BEHAV))
# D = 1, p-value = 0.00237
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK2$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK2$BEHAV
# W = 0.98023, p-value = 1.663e-05
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_RANK2_M4CID0$BEHAV)
var(data_SCHOOL1_JUNIOR_RANK2_M4CID1$BEHAV)
hist(data_SCHOOL1_JUNIOR_RANK2_M4CID0$BEHAV)
hist(data_SCHOOL1_JUNIOR_RANK2_M4CID1$BEHAV)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK2) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK2_M4CID0$BEHAV)#80
median(data_SCHOOL1_JUNIOR_RANK2_M4CID1$BEHAV)#72
wilcox.test(data_SCHOOL1_JUNIOR_RANK2_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_RANK2_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_RANK2_M4CID0$BEHAV and data_SCHOOL1_JUNIOR_RANK2_M4CID1$BEHAV
# W = 29278, p-value = 3.198e-09
# alternative hypothesis: true location shift is greater than 0
perm.test(data_SCHOOL1_JUNIOR_RANK2_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_RANK2_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 190000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# CLASS
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK2_M4CID0$CLASS,190))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK2_M4CID1$CLASS,190))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID0.CLASS..190.,sample2$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID1.CLASS..190.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID0.CLASS..190. and sample2$sample.data_SCHOOL1_JUNIOR_RANK2_M4CID1.CLASS..190.
# z = 0.62483, p-value = 0.5321
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.08083141 
hist(data_SCHOOL1_JUNIOR_RANK2$CLASS)
qqnorm(data_SCHOOL1_JUNIOR_RANK2$CLASS)
qqline(data_SCHOOL1_JUNIOR_RANK2$CLASS)
ks.test(data_SCHOOL1_JUNIOR_RANK2$CLASS,dnorm(mean(data_SCHOOL1_JUNIOR_RANK2$CLASS),sd(data_SCHOOL1_JUNIOR_RANK2$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK2$CLASS and dnorm(mean(data_SCHOOL1_JUNIOR_RANK2$CLASS), sd(data_SCHOOL1_JUNIOR_RANK2$CLASS))
# D = 1, p-value = 0.004739
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK2$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK2$CLASS
# W = 0.96837, p-value = 6.776e-08
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_RANK2_M4CID0$CLASS)
var(data_SCHOOL1_JUNIOR_RANK2_M4CID1$CLASS)
hist(data_SCHOOL1_JUNIOR_RANK2_M4CID0$CLASS)
hist(data_SCHOOL1_JUNIOR_RANK2_M4CID1$CLASS)
# not simmetric, almost same variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK2) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK2_M4CID0$CLASS)#57
median(data_SCHOOL1_JUNIOR_RANK2_M4CID1$CLASS)#58
wilcox.test(data_SCHOOL1_JUNIOR_RANK2_M4CID0$CLASS,data_SCHOOL1_JUNIOR_RANK2_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_RANK2_M4CID0$CLASS and data_SCHOOL1_JUNIOR_RANK2_M4CID1$CLASS
# W = 20174, p-value = 0.06598
# alternative hypothesis: true location shift is not equal to 0
perm.test(data_SCHOOL1_JUNIOR_RANK2_M4CID0$CLASS,data_SCHOOL1_JUNIOR_RANK2_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.14535

#RANK 3
data_SCHOOL1_JUNIOR_RANK3 <- filter(data_SCHOOL1_JUNIOR,RANK=="3") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_RANK3_M4CID0 <- filter(data_SCHOOL1_JUNIOR_RANK3, M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_RANK3_M4CID1 <- filter(data_SCHOOL1_JUNIOR_RANK3, M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_SCHOOL1_JUNIOR_RANK3_M4CID0)
length(data_SCHOOL1_JUNIOR_RANK3_M4CID0$ID)#67
length(data_SCHOOL1_JUNIOR_RANK3_M4CID1$ID)#118

# TEST
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK3_M4CID0$TEST,60))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK3_M4CID1$TEST,60))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID0.TEST..60.,sample2$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID1.TEST..60.,method="kendall",exact = FALSE)
Kendall(sample1$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID0.TEST..60.,sample2$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID1.TEST..60.)
length(sample1$TEST)
#Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID0.TEST..60. and sample2$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID1.TEST..60.
# z = 0.7897, p-value = 0.4297
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.104641 
# sample independent
hist(data_SCHOOL1_JUNIOR_RANK3$TEST)
qqnorm(data_SCHOOL1_JUNIOR_RANK3$TEST)
qqline(data_SCHOOL1_JUNIOR_RANK3$TEST)
ks.test(data_SCHOOL1_JUNIOR_RANK3$TEST,dnorm(mean(data_SCHOOL1_JUNIOR_RANK3$TEST),sd(data_SCHOOL1_JUNIOR_RANK3$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK3$TEST and dnorm(mean(data_SCHOOL1_JUNIOR_RANK3$TEST), sd(data_SCHOOL1_JUNIOR_RANK3$TEST))
# D = 1, p-value = 0.01075
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK3$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK3$TEST
# W = 0.98645, p-value = 0.07242
#conclusion: normal
var(data_SCHOOL1_JUNIOR_RANK3_M4CID0$TEST)
var(data_SCHOOL1_JUNIOR_RANK3_M4CID1$TEST)
hist(data_SCHOOL1_JUNIOR_RANK3_M4CID0$TEST)
hist(data_SCHOOL1_JUNIOR_RANK3_M4CID1$TEST)
# not simmetric, almost same variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK3) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK3_M4CID0$TEST)#79
median(data_SCHOOL1_JUNIOR_RANK3_M4CID1$TEST)#80
wilcox.test(data_SCHOOL1_JUNIOR_RANK3_M4CID0$TEST,data_SCHOOL1_JUNIOR_RANK3_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_RANK3_M4CID0$TEST and data_SCHOOL1_JUNIOR_RANK3_M4CID1$TEST
# W = 3832.5, p-value = 0.3658
# alternative hypothesis: true location shift is not equal to 0
perm.test(data_SCHOOL1_JUNIOR_RANK3_M4CID0$TEST,data_SCHOOL1_JUNIOR_RANK3_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.62315

# LAB
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK3_M4CID0$LAB,60))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK3_M4CID1$LAB,60))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID0.LAB..60.,sample2$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID1.LAB..60.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID0.LAB..60. and sample2$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID1.LAB..60.
# z = -0.92261, p-value = 0.3562
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1239852 
#sample independent
hist(data_SCHOOL1_JUNIOR_RANK3$LAB)
qqnorm(data_SCHOOL1_JUNIOR_RANK3$LAB)
qqline(data_SCHOOL1_JUNIOR_RANK3$LAB)
ks.test(data_SCHOOL1_JUNIOR_RANK3$LAB,dnorm(mean(data_SCHOOL1_JUNIOR_RANK3$LAB),sd(data_SCHOOL1_JUNIOR_RANK3$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK3$LAB and dnorm(mean(data_SCHOOL1_JUNIOR_RANK3$LAB), sd(data_SCHOOL1_JUNIOR_RANK3$LAB))
# D = 0.99525, value = 0.01075
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK3$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK3$LAB
# W = 0.94974, p-value = 4.06e-06
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_RANK3_M4CID0$LAB)
var(data_SCHOOL1_JUNIOR_RANK3_M4CID1$LAB)
hist(data_SCHOOL1_JUNIOR_RANK3_M4CID0$LAB)
hist(data_SCHOOL1_JUNIOR_RANK3_M4CID1$LAB)
# almost simmetric, different variances, almost same shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK3) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK3_M4CID0$LAB)#73
median(data_SCHOOL1_JUNIOR_RANK3_M4CID1$LAB)#81
wilcox.test(data_SCHOOL1_JUNIOR_RANK3_M4CID0$LAB,data_SCHOOL1_JUNIOR_RANK3_M4CID1$LAB,alternative = "less")
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK3$LAB
# W = 2343.5, p-value = 2.107e-06
perm.test(data_SCHOOL1_JUNIOR_RANK3_M4CID0$LAB,data_SCHOOL1_JUNIOR_RANK3_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# BEHAV
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK3_M4CID0$BEHAV,60))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK3_M4CID1$BEHAV,60))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID0.BEHAV..60.,sample2$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID1.BEHAV..60.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID0.BEHAV..60. and sample2$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID1.BEHAV..60.
# z = -1.3245, p-value = 0.1853
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1739132 
#sample independent
hist(data_SCHOOL1_JUNIOR_RANK3$BEHAV)
qqnorm(data_SCHOOL1_JUNIOR_RANK3$BEHAV)
qqline(data_SCHOOL1_JUNIOR_RANK3$BEHAV)
ks.test(data_SCHOOL1_JUNIOR_RANK3$BEHAV,dnorm(mean(data_SCHOOL1_JUNIOR_RANK3$BEHAV),sd(data_SCHOOL1_JUNIOR_RANK3$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK3$BEHAV and dnorm(mean(data_SCHOOL1_JUNIOR_RANK3$BEHAV), sd(data_SCHOOL1_JUNIOR_RANK3$BEHAV))
# D = 1, p-value = 0.005376
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK3$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK3$BEHAV
# W = 0.91126, p-value = 4.092e-09
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_RANK3_M4CID0$BEHAV)
var(data_SCHOOL1_JUNIOR_RANK3_M4CID1$BEHAV)
hist(data_SCHOOL1_JUNIOR_RANK3_M4CID0$BEHAV)
hist(data_SCHOOL1_JUNIOR_RANK3_M4CID1$BEHAV)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK3) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK3_M4CID0$BEHAV)#94
median(data_SCHOOL1_JUNIOR_RANK3_M4CID1$BEHAV)#91
wilcox.test(data_SCHOOL1_JUNIOR_RANK3_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_RANK3_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_RANK3_M4CID0$BEHAV and data_SCHOOL1_JUNIOR_RANK3_M4CID1$BEHAV
# W = 4871.5, p-value = 0.004318
# alternative hypothesis: true location shift is greater than 0
perm.test(data_SCHOOL1_JUNIOR_RANK3_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_RANK3_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# CLASS
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK3_M4CID0$CLASS,60))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_RANK3_M4CID1$CLASS,60))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID0.CLASS..60.,sample2$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID1.CLASS..60.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID0.CLASS..60. and sample2$sample.data_SCHOOL1_JUNIOR_RANK3_M4CID1.CLASS..60.
# z = 0.62483, p-value = 0.5321
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.08083141 
hist(data_SCHOOL1_JUNIOR_RANK3$CLASS)
qqnorm(data_SCHOOL1_JUNIOR_RANK3$CLASS)
qqline(data_SCHOOL1_JUNIOR_RANK3$CLASS)
ks.test(data_SCHOOL1_JUNIOR_RANK3$CLASS,dnorm(mean(data_SCHOOL1_JUNIOR_RANK3$CLASS),sd(data_SCHOOL1_JUNIOR_RANK3$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_RANK3$CLASS and dnorm(mean(data_SCHOOL1_JUNIOR_RANK3$CLASS), sd(data_SCHOOL1_JUNIOR_RANK3$CLASS))
# D = 1, p-value = 0.01075
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_RANK3$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_RANK3$CLASS
# W = 0.93836, p-value = 4.123e-07
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_RANK3_M4CID0$CLASS)
var(data_SCHOOL1_JUNIOR_RANK3_M4CID1$CLASS)
hist(data_SCHOOL1_JUNIOR_RANK3_M4CID0$CLASS)
hist(data_SCHOOL1_JUNIOR_RANK3_M4CID1$CLASS)
# not simmetric, almost same variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_RANK3) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_RANK3_M4CID0$CLASS)#77
median(data_SCHOOL1_JUNIOR_RANK3_M4CID1$CLASS)#81
wilcox.test(data_SCHOOL1_JUNIOR_RANK3_M4CID0$CLASS,data_SCHOOL1_JUNIOR_RANK3_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_RANK3_M4CID0$CLASS and data_SCHOOL1_JUNIOR_RANK3_M4CID1$CLASS
# W = 3088.5, p-value = 0.006789
# alternative hypothesis: true location shift is not equal to 0
perm.test(data_SCHOOL1_JUNIOR_RANK3_M4CID0$CLASS,data_SCHOOL1_JUNIOR_RANK3_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.01945

#Conclusion for SCHOOL 1 JUNIOR samples
#RANK 1 and 2: TEST, CLASS no effect; LAB positive; negative BEHAV 
#RANK 3: TEST no effect; positive LAB and CLASS; negative BEHAV

########## GENDER
# sample: school 1 JUNIOR, 2009:2017
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

#sample entries: 783; Female = 326 (48%); Male=357 (52%);
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
var(data_junior_SCHOOL1_M4CID0_FEMALE$TEST)
var(data_junior_SCHOOL1_M4CID1_FEMALE$TEST)
var(data_junior_SCHOOL1_M4CID0_MALE$TEST)
var(data_junior_SCHOOL1_M4CID1_MALE$TEST)
hist(data_junior_SCHOOL1_M4CID0_FEMALE$TEST)
hist(data_junior_SCHOOL1_M4CID1_FEMALE$TEST)
hist(data_junior_SCHOOL1_M4CID0_MALE$TEST)
hist(data_junior_SCHOOL1_M4CID1_MALE$TEST)
# not simmetric, almost same variances, diferent shapes: wilcox no applicable
ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=TEST, linetype = factor(GENDER)))
median(data_junior_SCHOOL1_M4CID0_FEMALE$TEST)#54
median(data_junior_SCHOOL1_M4CID1_FEMALE$TEST)#61
median(data_junior_SCHOOL1_M4CID0_MALE$TEST)#54
median(data_junior_SCHOOL1_M4CID1_MALE$TEST)#57
#It seems that the female benefit, less impact on male
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$TEST,data_junior_SCHOOL1_M4CID1_FEMALE$TEST, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$TEST and data_junior_SCHOOL1_M4CID1_FEMALE$TEST
# W = 10924, p-value = 0.002804
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$TEST,data_junior_SCHOOL1_M4CID1_MALE$TEST, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$TEST and data_junior_SCHOOL1_M4CID1_MALE$TEST
# W = 14188, p-value = 0.05675
# alternative hypothesis: true location shift is less than 0
perm.test(data_junior_SCHOOL1_M4CID0_FEMALE$TEST,data_junior_SCHOOL1_M4CID1_FEMALE$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.00315
perm.test(data_junior_SCHOOL1_M4CID0_MALE$TEST,data_junior_SCHOOL1_M4CID1_MALE$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.09625

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
var(data_junior_SCHOOL1_M4CID0_FEMALE$LAB)
var(data_junior_SCHOOL1_M4CID1_FEMALE$LAB)
var(data_junior_SCHOOL1_M4CID0_MALE$LAB)
var(data_junior_SCHOOL1_M4CID1_MALE$LAB)
hist(data_junior_SCHOOL1_M4CID0_FEMALE$LAB)
hist(data_junior_SCHOOL1_M4CID1_FEMALE$LAB)
hist(data_junior_SCHOOL1_M4CID0_MALE$LAB)
hist(data_junior_SCHOOL1_M4CID1_MALE$LAB)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=LAB, linetype = factor(GENDER)))
#It seems that both male/female benefit
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$LAB,data_junior_SCHOOL1_M4CID1_FEMALE$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$LAB and data_junior_SCHOOL1_M4CID1_FEMALE$LAB
# W = 5499.5, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$LAB,data_junior_SCHOOL1_M4CID1_MALE$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$LAB and data_junior_SCHOOL1_M4CID1_MALE$LAB
# W = 11162, p-value = 1.249e-06
# alternative hypothesis: true location shift is less than 0
perm.test(data_junior_SCHOOL1_M4CID0_FEMALE$LAB,data_junior_SCHOOL1_M4CID1_FEMALE$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0
perm.test(data_junior_SCHOOL1_M4CID0_MALE$LAB,data_junior_SCHOOL1_M4CID1_MALE$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

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
var(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV)
var(data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV)
var(data_junior_SCHOOL1_M4CID0_MALE$BEHAV)
var(data_junior_SCHOOL1_M4CID1_MALE$BEHAV)
hist(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV)
hist(data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV)
hist(data_junior_SCHOOL1_M4CID0_MALE$BEHAV)
hist(data_junior_SCHOOL1_M4CID1_MALE$BEHAV)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=BEHAV, linetype = factor(GENDER)))
#It seems that the male/female don't benefit, male is worse
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV,data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV, alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV and data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV
# W = 13459, p-value = 0.8337
# alternative hypothesis: true location shift is not equal to 0
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$BEHAV,data_junior_SCHOOL1_M4CID1_MALE$BEHAV, alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$BEHAV and data_junior_SCHOOL1_M4CID1_MALE$BEHAV
# W = 19768, p-value = 1.463e-05
# alternative hypothesis: true location shift is greater than 0
perm.test(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV,data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.73165
perm.test(data_junior_SCHOOL1_M4CID0_MALE$BEHAV,data_junior_SCHOOL1_M4CID1_MALE$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 5e-05

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
var(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV)
var(data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV)
var(data_junior_SCHOOL1_M4CID0_MALE$BEHAV)
var(data_junior_SCHOOL1_M4CID1_MALE$BEHAV)
hist(data_junior_SCHOOL1_M4CID0_FEMALE$BEHAV)
hist(data_junior_SCHOOL1_M4CID1_FEMALE$BEHAV)
hist(data_junior_SCHOOL1_M4CID0_MALE$BEHAV)
hist(data_junior_SCHOOL1_M4CID1_MALE$BEHAV)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_junior_SCHOOL1) + geom_boxplot(aes(x=factor(M4CID),y=CLASS, linetype = factor(GENDER)))
#It seems that the female have more benefit then the male
wilcox.test(data_junior_SCHOOL1_M4CID0_FEMALE$CLASS,data_junior_SCHOOL1_M4CID1_FEMALE$CLASS, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_FEMALE$CLASS and data_junior_SCHOOL1_M4CID1_FEMALE$CLASS
# W = 8602.5, p-value = 1.925e-08
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_M4CID0_MALE$CLASS,data_junior_SCHOOL1_M4CID1_MALE$CLASS, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_M4CID0_MALE$CLASS and data_junior_SCHOOL1_M4CID1_MALE$CLASS
# W = 13812, p-value = 0.02442
# alternative hypothesis: true location shift is not equal to 0
perm.test(data_junior_SCHOOL1_M4CID0_FEMALE$CLASS,data_junior_SCHOOL1_M4CID1_FEMALE$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0
perm.test(data_junior_SCHOOL1_M4CID0_MALE$CLASS,data_junior_SCHOOL1_M4CID1_MALE$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.0211

# SCHOOL 1 JUNIOR sample 2009:20017: GENDER
# FEMALE: TEST, LAB, CLASS positive; no effect on BEHAV
# MALE: positive impact on LAB and CLASS, negative on BEHAV and no impact on TEST

# Female, overall, benefit more; no different between both samples: school 1 with 
#junior and school 1 with junitor and junior high

# SCHOOL 1
# Inference only for School 1: 2009-2014
# off course, sample not computed
summary(data)
data_SCHOOL1_2014<-filter(data,data$SCHOOL=="1" & DATE <=2014)  %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
length(which(data_SCHOOL1_2014$M4CID=="0"))#352 observations
length(which(data_SCHOOL1_2014$M4CID=="1"))#277 observations
data_SCHOOL1_2014_M4CID0 <- filter(data_SCHOOL1_2014,M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_SCHOOL1_2014_M4CID1 <- filter(data_SCHOOL1_2014,M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_SCHOOL1_2014)
hist(data_SCHOOL1_2014$TEST)
qqnorm(data_SCHOOL1_2014$TEST)
qqline(data_SCHOOL1_2014$TEST)
ks.test(data_SCHOOL1_2014$TEST,dnorm(mean(data_SCHOOL1_2014$TEST),sd(data_SCHOOL1_2014$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_2014$TEST and dnorm(mean(data_SCHOOL1_2014$TEST), sd(data_SCHOOL1_2014$TEST))
# D = 0.99787, p-value = 0.003175
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_2014$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_2014$TEST
# W = 0.98454, p-value = 3.277e-06
#conclusion: Samples not normal, so use Kendall non-parametric correlation test

# TEST
sample1<-data.frame(sample(data_SCHOOL1_2014_M4CID0$TEST,250))
sample2<-data.frame(sample(data_SCHOOL1_2014_M4CID1$TEST,250))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_SCHOOL1_2014_M4CID0.TEST..250.,sample2$sample.data_SCHOOL1_2014_M4CID1.TEST..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_2014_M4CID0.TEST..250. and sample2$sample.data_SCHOOL1_2014_M4CID1.TEST..250.
# z = 0.21033, p-value = 0.8334
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.008279556 
# sample independency, so it is possible to use Wilcoxon non-parametric inference test
var(data_SCHOOL1_2014_M4CID0$TEST)
var(data_SCHOOL1_2014_M4CID1$TEST)
hist(data_SCHOOL1_2014_M4CID0$TEST)
hist(data_SCHOOL1_2014_M4CID1$TEST)
# almost simmetric, almost same variances, almost same shapes: wilcox almost applicable
ggplot(data_SCHOOL1_2014) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_2014_M4CID0$TEST)#54
median(data_SCHOOL1_2014_M4CID1$TEST)#57
wilcox.test(data_SCHOOL1_2014_M4CID0$TEST,data_SCHOOL1_2014_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_2014_M4CID0$TEST and data_SCHOOL1_2014_M4CID1$TEST
# W = 42946, p-value = 0.005137
# alternative hypothesis: true location shift is less than 0
perm.test(data_SCHOOL1_2014_M4CID0$TEST,data_SCHOOL1_2014_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.0033

# LAB
sample1<-data.frame(sample(data_SCHOOL1_2014_M4CID0$LAB,250))
sample2<-data.frame(sample(data_SCHOOL1_2014_M4CID1$LAB,250))
#Not normal, Kendall non-parametric correlation test
cor.test(sample1$sample.data_SCHOOL1_2014_M4CID0.LAB..250.,sample2$sample.data_SCHOOL1_2014_M4CID1.LAB..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_2014_M4CID0.LAB..250. and sample2$sample.data_SCHOOL1_2014_M4CID1.LAB..250.
# z = -1.4187, p-value = 0.156
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.05601023 
hist(data_SCHOOL1_2014$LAB)
qqnorm(data_SCHOOL1_2014$LAB)
qqline(data_SCHOOL1_2014$LAB)
ks.test(data_SCHOOL1_2014$LAB,dnorm(mean(data_SCHOOL1_2014$LAB),sd(data_SCHOOL1_2014$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_2014$LAB and dnorm(mean(data_SCHOOL1_2014$LAB), sd(data_SCHOOL1_2014$LAB))
# D = 0.98507, p-value = 0.02875
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_2014$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_2014$LAB
# W = 0.97789, p-value = 9.766e-11
#conclusion: not normal
var(data_SCHOOL1_2014_M4CID0$LAB)
var(data_SCHOOL1_2014_M4CID1$LAB)
hist(data_SCHOOL1_2014_M4CID0$LAB)
hist(data_SCHOOL1_2014_M4CID1$LAB)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_SCHOOL1_2014) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_2014_M4CID0$LAB)#51
median(data_SCHOOL1_2014_M4CID1$LAB)#63
wilcox.test(data_SCHOOL1_2014_M4CID0$LAB,data_SCHOOL1_2014_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_2014_M4CID0$LAB and data_SCHOOL1_2014_M4CID1$LAB
# W = 31530, p-value = 1.333e-14
# alternative hypothesis: true location shift is less than 0
perm.test(data_SCHOOL1_2014_M4CID0$LAB,data_SCHOOL1_2014_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# BEHAV
sample1<-data.frame(sample(data_SCHOOL1_2014_M4CID0$BEHAV,250))
sample2<-data.frame(sample(data_SCHOOL1_2014_M4CID1$BEHAV,250))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_2014_M4CID0.BEHAV..250.,sample2$sample.data_SCHOOL1_2014_M4CID1.BEHAV..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_2014_M4CID0.BEHAV..250. and sample2$sample.data_SCHOOL1_2014_M4CID1.BEHAV..250.
# z = 0.26, p-value = 0.7949
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01029303 
hist(data_SCHOOL1_2014$BEHAV)
qqnorm(data_SCHOOL1_2014$BEHAV)
qqline(data_SCHOOL1_2014$BEHAV)
ks.test(data_SCHOOL1_2014$BEHAV,dnorm(mean(data_SCHOOL1_2014$BEHAV),sd(data_SCHOOL1_2014$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_2014$BEHAV and dnorm(mean(data_SCHOOL1_2014$BEHAV), sd(data_SCHOOL1_2014$BEHAV))
# D = 1, p-value = 0.001587
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_2014$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_2014$BEHAV
# W = 0.95671, p-value = 1.21e-12
#conclusion: not normal and independent
var(data_SCHOOL1_2014_M4CID0$BEHAV)
var(data_SCHOOL1_2014_M4CID1$BEHAV)
hist(data_SCHOOL1_2014_M4CID0$BEHAV)
hist(data_SCHOOL1_2014_M4CID1$BEHAV)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_SCHOOL1_2014) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_2014_M4CID0$BEHAV)#80
median(data_SCHOOL1_2014_M4CID1$BEHAV)#83
wilcox.test(data_SCHOOL1_2014_M4CID0$BEHAV,data_SCHOOL1_2014_M4CID1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_2014_M4CID0$BEHAV and data_SCHOOL1_2014_M4CID1$BEHAV
# W = 44878, p-value = 0.0434
# alternative hypothesis: true location shift is greater than 0
perm.test(data_SCHOOL1_2014_M4CID0$BEHAV,data_SCHOOL1_2014_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.03665

# CLASS
sample1<-data.frame(sample(data_SCHOOL1_2014_M4CID0$CLASS,250))
sample2<-data.frame(sample(data_SCHOOL1_2014_M4CID1$CLASS,250))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_2014_M4CID0.CLASS..250.,sample2$sample.data_SCHOOL1_2014_M4CID1.CLASS..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_2014_M4CID0.CLASS..250. and sample2$sample.data_SCHOOL1_2014_M4CID1.CLASS..250.
# z = 1.0436, p-value = 0.2967
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau  0.04046901 
hist(data_SCHOOL1_2014$CLASS)
qqnorm(data_SCHOOL1_2014$CLASS)
qqline(data_SCHOOL1_2014$CLASS)
ks.test(data_SCHOOL1_2014$CLASS,dnorm(mean(data_SCHOOL1_2014$CLASS),sd(data_SCHOOL1_2014$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_2014$CLASS and dnorm(mean(data_SCHOOL1_2014$CLASS), sd(data_SCHOOL1_2014$CLASS))
# D = 1, p-value = 0.003175
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_2014$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_2014$CLASS
# W = 0.99074, p-value = 0.0005513
#conclusion: not normal and independent samples
var(data_SCHOOL1_2014_M4CID0$CLASS)
var(data_SCHOOL1_2014_M4CID1$CLASS)
hist(data_SCHOOL1_2014_M4CID0$CLASS)
hist(data_SCHOOL1_2014_M4CID1$CLASS)
# not simmetric, almost equal variances, different shapes: wilcox almost applicable
ggplot(data_SCHOOL1_2014) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_2014_M4CID0$CLASS)#58
median(data_SCHOOL1_2014_M4CID1$CLASS)#63
wilcox.test(data_SCHOOL1_2014_M4CID0$CLASS,data_SCHOOL1_2014_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_2014_M4CID0$CLASS and data_SCHOOL1_2014_M4CID1$CLASS
# W = 36924, p-value = 8.585e-08
# alternative hypothesis: true location shift is less than 0
perm.test(data_SCHOOL1_2014_M4CID0$CLASS,data_SCHOOL1_2014_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.

#Conclusions school 1 junior sample for 2009:2014
# not normal, sample independent
# 4CID as positive effect in all variables

# Sample junior school 1: 2009:2014
data_JUNIOR_SCHOOL1_2014<-filter(data,GRADE <="2" & SCHOOL == "1",DATE<=2014) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_JUNIOR_SCHOOL1_2014_2014)
summary(data_JUNIOR_SCHOOL1_2014_2014)

length(which(data_JUNIOR_SCHOOL1_2014$M4CID=="0"))#318 entrances
length(which(data_JUNIOR_SCHOOL1_2014$M4CID=="1"))#217 entrances

data_JUNIOR_SCHOOL1_2014_M4CID0 <- filter(data_JUNIOR_SCHOOL1_2014,M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_JUNIOR_SCHOOL1_2014_M4CID1 <- filter(data_JUNIOR_SCHOOL1_2014,M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_JUNIOR_SCHOOL1_2014_M4CID0)
summary(data_JUNIOR_SCHOOL1_2014_M4CID1)

# TEST
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$TEST,200))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$TEST,200))
#Kendall's correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.TEST..200.,sample2$sample.data_SCHOOL1_M4CID1.TEST..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.TEST..200. and sample2$sample.data_SCHOOL1_M4CID1.TEST..200.
# z = 1.545, p-value = 0.1223
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.06083573
hist(data_JUNIOR_SCHOOL1_2014$TEST)
qqnorm(data_JUNIOR_SCHOOL1_2014$TEST)
qqline(data_JUNIOR_SCHOOL1_2014$TEST)
ks.test(data_JUNIOR_SCHOOL1_2014$TEST,dnorm(mean(data_JUNIOR_SCHOOL1_2014$TEST),sd(data_JUNIOR_SCHOOL1_2014$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_2014$TEST and dnorm(mean(data_JUNIOR_SCHOOL1_2014$TEST), sd(data_JUNIOR_SCHOOL1_2014$TEST))
# D = 1,  p-value = 0.003731
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_2014$TEST)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_2014$TEST
# W = 0.98407, p-value = 1.364e-05
#conclusion: not normal but independent sample
var(data_SCHOOL1_2014_M4CID0$TEST)
var(data_SCHOOL1_2014_M4CID1$TEST)
hist(data_SCHOOL1_2014_M4CID0$TEST)
hist(data_SCHOOL1_2014_M4CID1$TEST)
# not simmetric, almost equal variances, different shapes: wilcox almost applicable
ggplot(data_JUNIOR_SCHOOL1_2014) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_2014_M4CID0$TEST)#54
median(data_JUNIOR_SCHOOL1_2014_M4CID1$TEST)#58
wilcox.test(data_JUNIOR_SCHOOL1_2014_M4CID0$TEST,data_JUNIOR_SCHOOL1_2014_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_2014_M4CID0$TEST and data_JUNIOR_SCHOOL1_2014_M4CID1$TEST
# W = 29558, p-value = 0.002423
# alternative hypothesis: true location shift is less than 0
perm.test(data_JUNIOR_SCHOOL1_2014_M4CID0$TEST,data_JUNIOR_SCHOOL1_2014_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.00225

# LAB
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$LAB,200))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$LAB,200))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.LAB..200.,sample2$sample.data_SCHOOL1_M4CID1.LAB..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.LAB..200. and sample2$sample.data_SCHOOL1_M4CID1.LAB..200.
# z = -0.8257, p-value = 0.409
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.03263017 
hist(data_JUNIOR_SCHOOL1_2014$LAB)
qqnorm(data_JUNIOR_SCHOOL1_2014$LAB)
qqline(data_JUNIOR_SCHOOL1_2014$LAB)
ks.test(data_JUNIOR_SCHOOL1_2014$LAB,dnorm(mean(data_JUNIOR_SCHOOL1_2014$LAB),sd(data_JUNIOR_SCHOOL1_2014$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_2014$LAB and dnorm(mean(data_JUNIOR_SCHOOL1_2014$LAB), sd(data_JUNIOR_SCHOOL1_2014$LAB))
# D = 0.9757, p-value = 0.05224
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_2014$LAB)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_2014$LAB
# W = 0.97873, p-value = 4.999e-07
#conclusion: not normal and independent
var(data_SCHOOL1_2014_M4CID0$LAB)
var(data_SCHOOL1_2014_M4CID1$LAB)
hist(data_SCHOOL1_2014_M4CID0$LAB)
hist(data_SCHOOL1_2014_M4CID1$LAB)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_JUNIOR_SCHOOL1_2014) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_2014_M4CID0$LAB)#51
median(data_JUNIOR_SCHOOL1_2014_M4CID1$LAB)#64
wilcox.test(data_JUNIOR_SCHOOL1_2014_M4CID0$LAB,data_JUNIOR_SCHOOL1_2014_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_2014_M4CID0$LAB and data_JUNIOR_SCHOOL1_2014_M4CID1$LAB
# W = 21092, p-value = 1.083e-14
# alternative hypothesis: true location shift is less than 0
perm.test(data_JUNIOR_SCHOOL1_2014_M4CID0$LAB,data_JUNIOR_SCHOOL1_2014_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# BEHAV
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$BEHAV,200))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$BEHAV,200))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.BEHAV..200.,sample2$sample.data_SCHOOL1_M4CID1.BEHAV..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.BEHAV..200. and sample2$sample.data_SCHOOL1_M4CID1.BEHAV..200.
# z = -0.34014, p-value = 0.7338
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01346713 
hist(data_JUNIOR_SCHOOL1_2014$BEHAV)
qqnorm(data_JUNIOR_SCHOOL1_2014$BEHAV)
qqline(data_JUNIOR_SCHOOL1_2014$BEHAV)
ks.test(data_JUNIOR_SCHOOL1_2014$BEHAV,dnorm(mean(data_JUNIOR_SCHOOL1_2014$BEHAV),sd(data_JUNIOR_SCHOOL1_2014$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_2014$BEHAV and dnorm(mean(data_JUNIOR_SCHOOL1_2014$BEHAV), sd(data_JUNIOR_SCHOOL1_2014$BEHAV))
# D = 1, p-value = 0.001866
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_2014$BEHAV)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_2014$BEHAV
# W = 0.95651, p-value = 1.84e-11
#conclusion: not normal but independent
var(data_SCHOOL1_2014_M4CID0$BEHAV)
var(data_SCHOOL1_2014_M4CID1$BEHAV)
hist(data_SCHOOL1_2014_M4CID0$BEHAV)
hist(data_SCHOOL1_2014_M4CID1$BEHAV)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_JUNIOR_SCHOOL1_2014) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_2014_M4CID0$BEHAV)#81
median(data_JUNIOR_SCHOOL1_2014_M4CID1$BEHAV)#83
wilcox.test(data_JUNIOR_SCHOOL1_2014_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_2014_M4CID1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_2014_M4CID0$BEHAV and data_JUNIOR_SCHOOL1_2014_M4CID1$BEHAV
# W = 32198, p-value = 0.0945
# alternative hypothesis: true location shift is greater than 0
perm.test(data_JUNIOR_SCHOOL1_2014_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_2014_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.12745

# CLASS
sample1<-data.frame(sample(data_SCHOOL1_M4CID0$CLASS,200))
sample2<-data.frame(sample(data_SCHOOL1_M4CID1$CLASS,200))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_M4CID0.CLASS..200.,sample2$sample.data_SCHOOL1_M4CID1.CLASS..200.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_M4CID0.CLASS..200. and sample2$sample.data_SCHOOL1_M4CID1.CLASS..200.
# z = 0.21828, p-value = 0.8272
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.008464829
hist(data_JUNIOR_SCHOOL1_2014$CLASS)
qqnorm(data_JUNIOR_SCHOOL1_2014$CLASS)
qqline(data_JUNIOR_SCHOOL1_2014$CLASS)
ks.test(data_JUNIOR_SCHOOL1_2014$CLASS,dnorm(mean(data_JUNIOR_SCHOOL1_2014$CLASS),sd(data_JUNIOR_SCHOOL1_2014$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_JUNIOR_SCHOOL1_2014$CLASS and dnorm(mean(data_JUNIOR_SCHOOL1_2014$CLASS), sd(data_JUNIOR_SCHOOL1_2014$CLASS))
# D = 1,  p-value = 0.003731
# alternative hypothesis: two-sided
shapiro.test(data_JUNIOR_SCHOOL1_2014$CLASS)
# Shapiro-Wilk normality test
# data:  data_JUNIOR_SCHOOL1_2014$CLASS
# W = 0.99082, p-value = 0.002088
#conclusion: not normal but independent
var(data_SCHOOL1_2014_M4CID0$CLASS)
var(data_SCHOOL1_2014_M4CID1$CLASS)
hist(data_SCHOOL1_2014_M4CID0$CLASS)
hist(data_SCHOOL1_2014_M4CID1$CLASS)
# not simmetric, almost same variances, different shapes: wilcox almost applicable
ggplot(data_JUNIOR_SCHOOL1_2014) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_JUNIOR_SCHOOL1_2014_M4CID0$CLASS)#58
median(data_JUNIOR_SCHOOL1_2014_M4CID1$CLASS)#64
wilcox.test(data_JUNIOR_SCHOOL1_2014_M4CID0$CLASS,data_JUNIOR_SCHOOL1_2014_M4CID1$CLASS,alternative = "less")
#Wilcoxon rank sum test with continuity correction
# data:  data_JUNIOR_SCHOOL1_2014_M4CID0$CLASS and data_JUNIOR_SCHOOL1_2014_M4CID1$CLASS
# W = 25364, p-value = 9.686e-08
# alternative hypothesis: true location shift is less than 0
perm.test(data_JUNIOR_SCHOOL1_2014_M4CID0$CLASS,data_JUNIOR_SCHOOL1_2014_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# Conclusions for sample junior School 1: 2009:2014
# not normal sample, sample independent
# 4CID as a positive impact on TEST, LAB and CLASS Variable;
# 4CID as a negative impact in BEHAV variable

# GRADE
# Junior SCHOOL 1 sample, Grade 7: 2009:2014
summary (data)
data_7_SCHOOL1_2014<-filter(data,GRADE =="0" & SCHOOL == "1" & DATE<=2014) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_7_SCHOOL1_2014)
summary(data_7_SCHOOL1_2014)

length(which(data_7_SCHOOL1_2014$M4CID=="0"))#123 obs
length(which(data_7_SCHOOL1_2014$M4CID=="1"))#22 obs
# very low number of observations, < 50

data_7_SCHOOL1_2014_M4CID0 <- filter(data_7_SCHOOL1_2014,M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_7_SCHOOL1_2014_M4CID1 <- filter(data_7_SCHOOL1_2014,M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_7_SCHOOL1_2014_M4CID0)
summary(data_7_SCHOOL1_2014_M4CID1)

# TEST
hist(data_7_SCHOOL1_2014$TEST)
qqnorm(data_7_SCHOOL1_2014$TEST)
qqline(data_7_SCHOOL1_2014$TEST)
ks.test(data_7_SCHOOL1_2014$TEST,dnorm(mean(data_7_SCHOOL1_2014$TEST),sd(data_7_SCHOOL1_2014$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_7_SCHOOL1_2014$TEST and dnorm(mean(data_7_SCHOOL1_2014$TEST), sd(data_7_SCHOOL1_2014$TEST))
# D = 1, p-value = 0.0137
# alternative hypothesis: two-sided
shapiro.test(data_7_SCHOOL1_2014$TEST)
# Shapiro-Wilk normality test
# data:  data_7_SCHOOL1_2014$TEST
# W = 0.97871, p-value = 0.02346
#conclusion: not normal
sample1<-data.frame(sample(data_7_SCHOOL1_2014_M4CID0$TEST,20))
sample2<-data.frame(sample(data_7_SCHOOL1_2014_M4CID1$TEST,20))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_2014_M4CID0.TEST..20.,sample2$sample.data_7_SCHOOL1_2014_M4CID1.TEST..20.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_2014_M4CID0.TEST..20. and sample2$sample.data_7_SCHOOL1_2014_M4CID1.TEST..20.
# z = 1.1478, p-value = 0.2511
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.08346056 
# Independet sample
var(data_7_SCHOOL1_2014_M4CID0$TEST)
var(data_7_SCHOOL1_2014_M4CID1$TEST)
hist(data_7_SCHOOL1_2014_M4CID0$TEST)
hist(data_7_SCHOOL1_2014_M4CID1$TEST)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_7_SCHOOL1_2014) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_7_SCHOOL1_2014_M4CID0$TEST)#54
median(data_7_SCHOOL1_2014_M4CID1$TEST)#62
wilcox.test(data_7_SCHOOL1_2014_M4CID0$TEST,data_7_SCHOOL1_2014_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_2014_M4CID0$TEST and data_7_SCHOOL1_2014_M4CID1$TEST
# W = 976, p-value = 0.01896
perm.test(data_JUNIOR_SCHOOL1_2014_M4CID0$TEST,data_JUNIOR_SCHOOL1_2014_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.0016

# LAB
hist(data_7_SCHOOL1_2014$LAB)
qqnorm(data_7_SCHOOL1_2014$LAB)
qqline(data_7_SCHOOL1_2014$LAB)
ks.test(data_7_SCHOOL1_2014$LAB,dnorm(mean(data_7_SCHOOL1_2014$LAB),sd(data_7_SCHOOL1_2014$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_7_SCHOOL1_2014$LAB and dnorm(mean(data_7_SCHOOL1_2014$LAB), sd(data_7_SCHOOL1_2014$LAB))
# D = 1, p-value = 0.006849
# alternative hypothesis: two-sided
shapiro.test(data_7_SCHOOL1_2014$LAB)
# Shapiro-Wilk normality test
# data:  data_7_SCHOOL1_2014$LAB
# W = 0.9825, p-value = 0.06131
#conclusion: normal
sample1<-data.frame(sample(data_7_SCHOOL1_2014_M4CID0$LAB,20))
sample2<-data.frame(sample(data_7_SCHOOL1_2014_M4CID1$LAB,20))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_2014_M4CID0.LAB..90.,sample2$sample.data_7_SCHOOL1_2014_M4CID1.LAB..20.,method="kendall")
# KKendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_2014_M4CID0.LAB..20. and sample2$sample.data_7_SCHOOL1_2014_M4CID1.LAB..20.
# z = 0.15359, p-value = 0.8779
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01121734 
# independent samples
var(data_7_SCHOOL1_2014_M4CID0$LAB)
var(data_7_SCHOOL1_2014_M4CID1$LAB)
hist(data_7_SCHOOL1_2014_M4CID0$LAB)
hist(data_7_SCHOOL1_2014_M4CID1$ALAB)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_7_SCHOOL1_2014) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_7_SCHOOL1_2014_M4CID0$LAB)#56
median(data_7_SCHOOL1_2014_M4CID1$LAB)#71
wilcox.test(data_7_SCHOOL1_2014_M4CID0$LAB,data_7_SCHOOL1_2014_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_2014_M4CID0$LAB and data_7_SCHOOL1_2014_M4CID1$LAB
# W = 778, p-value = 0.000769
# alternative hypothesis: true location shift is less than 0
perm.test(data_JUNIOR_SCHOOL1_2014_M4CID0$LAB,data_JUNIOR_SCHOOL1_2014_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# BEHAV
hist(data_7_SCHOOL1_2014$BEHAV)
qqnorm(data_7_SCHOOL1_2014$BEHAV)
qqline(data_7_SCHOOL1_2014$BEHAV)
ks.test(data_7_SCHOOL1_2014$BEHAV,dnorm(mean(data_7_SCHOOL1_2014$BEHAV),sd(data_7_SCHOOL1_2014$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_7_SCHOOL1_2014$BEHAV and dnorm(mean(data_7_SCHOOL1_2014$BEHAV), sd(data_7_SCHOOL1_2014$BEHAV))
# D = 1, p-value = 0.008969
# alternative hypothesis: two-sided
shapiro.test(data_7_SCHOOL1_2014$BEHAV)
# Shapiro-Wilk normality test
# data:  data_7_SCHOOL1_2014$BEHAV
# W = 0.9766, p-value = 0.0009589
#conclusion: not normal
sample1<-data.frame(sample(data_7_SCHOOL1_2014_M4CID0$BEHAV,20))
sample2<-data.frame(sample(data_7_SCHOOL1_2014_M4CID1$BEHAV,20))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_2014_M4CID0.BEHAV..20.,sample2$sample.data_7_SCHOOL1_2014_M4CID1.BEHAV..20.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_2014_M4CID0.BEHAV..20. and sample2$sample.data_7_SCHOOL1_2014_M4CID1.BEHAV..20.
# z = 0.55146, p-value = 0.5813
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04028563 
# sample independent
var(data_7_SCHOOL1_2014_M4CID0$BEHAV)
var(data_7_SCHOOL1_2014_M4CID1$BEHAV)
hist(data_7_SCHOOL1_2014_M4CID0$BEHAV)
hist(data_7_SCHOOL1_2014_M4CID1$BEHAV)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_7_SCHOOL1_2014) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_7_SCHOOL1_2014_M4CID0$BEHAV)#77
median(data_7_SCHOOL1_2014_M4CID1$BEHAV)#81
wilcox.test(data_7_SCHOOL1_2014_M4CID0$BEHAV,data_7_SCHOOL1_2014_M4CID1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_2014_M4CID0$BEHAV and data_7_SCHOOL1_2014_M4CID1$BEHAV
# W = 1116.5, p-value = 0.09657
# alternative hypothesis: true location shift is greater than 0
perm.test(data_JUNIOR_SCHOOL1_2014_M4CID0$BEHAV,data_JUNIOR_SCHOOL1_2014_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.12485

# CLASS
hist(data_7_SCHOOL1_2014$CLASS)
qqnorm(data_7_SCHOOL1_2014$CLASS)
qqline(data_7_SCHOOL1_2014$CLASS)
ks.test(data_7_SCHOOL1_2014$CLASS,dnorm(mean(data_7_SCHOOL1_2014$CLASS),sd(data_7_SCHOOL1_2014$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_7_SCHOOL1_2014$CLASS and dnorm(mean(data_7_SCHOOL1_2014$CLASS), sd(data_7_SCHOOL1_2014$CLASS))
# D = 1, p-value = 0.008969
# alternative hypothesis: two-sided
shapiro.test(data_7_SCHOOL1_2014$CLASS)
# Shapiro-Wilk normality test
# data:  data_7_SCHOOL1_2014$CLASS
# W = 0.9928, p-value = 0.3536
#conclusion: shapir report normal, while KS no
sample1<-data.frame(sample(data_7_SCHOOL1_2014_M4CID0$CLASS,20))
sample2<-data.frame(sample(data_7_SCHOOL1_2014_M4CID1$CLASS,20))
#Kendall correlation test
cor.test(sample1$sample.data_7_SCHOOL1_2014_M4CID0.CLASS..20.,sample2$sample.data_7_SCHOOL1_2014_M4CID1.CLASS..20.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_7_SCHOOL1_2014_M4CID0.CLASS..20. and sample2$sample.data_7_SCHOOL1_2014_M4CID1.CLASS..20.
# z = 0.0034856, p-value = 0.9972
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.0002501876 
# Independent samples
var(data_7_SCHOOL1_2014_M4CID0$CLASS)
var(data_7_SCHOOL1_2014_M4CID1$CLASS)
hist(data_7_SCHOOL1_2014_M4CID0$CLASS)
hist(data_7_SCHOOL1_2014_M4CID1$CLASS)
# almost simmetric, different variances, almost same shapes: wilcox almost applicable
ggplot(data_7_SCHOOL1_2014) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_7_SCHOOL1_2014_M4CID0$CLASS)#60
median(data_7_SCHOOL1_2014_M4CID1$CLASS)#67
wilcox.test(data_7_SCHOOL1_2014_M4CID0$CLASS,data_7_SCHOOL1_2014_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_7_SCHOOL1_2014_M4CID0$CLASS and data_7_SCHOOL1_2014_M4CID1$CLASS
# W = 845, p-value = 0.002579
# alternative hypothesis: true location shift is less than 0
perm.test(data_JUNIOR_SCHOOL1_2014_M4CID0$CLASS,data_JUNIOR_SCHOOL1_2014_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# Inference SCHOOL 1, Grade 8
# Junior SCHOOL 1 sample, Grade 7: 2009:2014
summary (data)
data_8_SCHOOL1_2014<-filter(data,GRADE =="1" & SCHOOL == "1" & DATE <= 2014) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_8_SCHOOL1_2014)
summary(data_8_SCHOOL1_2014)

length(which(data_8_SCHOOL1_2014$M4CID=="0"))#92 obs
length(which(data_8_SCHOOL1_2014$M4CID=="1"))#76 obs

data_8_SCHOOL1_2014_M4CID0 <- filter(data_8_SCHOOL1_2014,M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_8_SCHOOL1_2014_M4CID1 <- filter(data_8_SCHOOL1_2014,M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_8_SCHOOL1_2014_M4CID0)
summary(data_8_SCHOOL1_2014_M4CID1)

# TEST
hist(data_8_SCHOOL1_2014$TEST)
qqnorm(data_8_SCHOOL1_2014$TEST)
qqline(data_8_SCHOOL1_2014$TEST)
ks.test(data_8_SCHOOL1_2014$TEST,dnorm(mean(data_8_SCHOOL1_2014$TEST),sd(data_8_SCHOOL1_2014$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_8_SCHOOL1_2014$TEST and dnorm(mean(data_8_SCHOOL1_2014$TEST), sd(data_8_SCHOOL1_2014$TEST))
# D = 1, p-value = 0.005917
# alternative hypothesis: two-sided
shapiro.test(data_8_SCHOOL1_2014$TEST)
# Shapiro-Wilk normality test
# data:  data_8_SCHOOL1_2014$TEST
# W = 0.98128, p-value = 0.02288
#conclusion: not normal
sample1<-data.frame(sample(data_8_SCHOOL1_2014_M4CID0$TEST,70))
sample2<-data.frame(sample(data_8_SCHOOL1_2014_M4CID1$TEST,70))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_2014_M4CID0.TEST..70.,sample2$sample.data_8_SCHOOL1_2014_M4CID1.TEST..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_2014_M4CID0.TEST..70. and sample2$sample.data_8_SCHOOL1_2014_M4CID1.TEST..70.
# z = 0.67348, p-value = 0.5006
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04906577 
#samples are independent
var(data_8_SCHOOL1_2014_M4CID0$TEST)
var(data_8_SCHOOL1_2014_M4CID1$TEST)
hist(data_8_SCHOOL1_2014_M4CID0$TEST)
hist(data_8_SCHOOL1_2014_M4CID1$TEST)
# not simmetric, different variances, diferent shapes: wilcox almost applicable
ggplot(data_8_SCHOOL1_2014) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_8_SCHOOL1_2014_M4CID0$TEST)#54
median(data_8_SCHOOL1_2014_M4CID1$TEST)#57
wilcox.test(data_8_SCHOOL1_2014_M4CID0$TEST,data_8_SCHOOL1_2014_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_2014_M4CID0$TEST and data_8_SCHOOL1_2014_M4CID1$TEST
# W = 2680, p-value = 0.004663
# alternative hypothesis: true location shift is less than 0
perm.test(data_8_SCHOOL1_2014_M4CID0$TEST,data_8_SCHOOL1_2014_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.00605

# LAB
hist(data_8_SCHOOL1_2014$LAB)
qqnorm(data_8_SCHOOL1_2014$LAB)
qqline(data_8_SCHOOL1_2014$LAB)
ks.test(data_8_SCHOOL1_2014$LAB,dnorm(mean(data_8_SCHOOL1_2014$LAB),sd(data_8_SCHOOL1_2014$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_8_SCHOOL1_2014$LAB and dnorm(mean(data_8_SCHOOL1_2014$LAB), sd(data_8_SCHOOL1_2014$LAB))
# D = 0.94643, p-value = 0.1124
# alternative hypothesis: two-sided
shapiro.test(data_8_SCHOOL1_2014$LAB)
# Shapiro-Wilk normality test
# data:  data_8_SCHOOL1_2014$LAB
# W = 0.96125, p-value = 0.0001279
#conclusion: not normal by shapiro analysis
sample1<-data.frame(sample(data_8_SCHOOL1_2014_M4CID0$LAB,70))
sample2<-data.frame(sample(data_8_SCHOOL1_2014_M4CID1$LAB,70))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_2014_M4CID0.LAB..70.,sample2$sample.data_8_SCHOOL1_2014_M4CID1.LAB..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_2014_M4CID0.LAB..70. and sample2$sample.data_8_SCHOOL1_2014_M4CID1.LAB..70.
# z = 0.28602, p-value = 0.7749
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.02151758 
# sample are independent
var(data_8_SCHOOL1_2014_M4CID0$LAB)
var(data_8_SCHOOL1_2014_M4CID1$LAB)
hist(data_8_SCHOOL1_2014_M4CID0$LAB)
hist(data_8_SCHOOL1_2014_M4CID1$LAB)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_8_SCHOOL1_2014) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_8_SCHOOL1_2014_M4CID0$LAB)#51
median(data_8_SCHOOL1_2014_M4CID1$LAB)#56
wilcox.test(data_8_SCHOOL1_2014_M4CID0$LAB,data_8_SCHOOL1_2014_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_2014_M4CID0$LAB and data_8_SCHOOL1_2014_M4CID1$LAB
# W = 2659.5, p-value = 0.003794
# alternative hypothesis: true location shift is less than 0
perm.test(data_8_SCHOOL1_2014_M4CID0$LAB,data_8_SCHOOL1_2014_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.002

# BEHAV
hist(data_8_SCHOOL1_2014$BEHAV)
qqnorm(data_8_SCHOOL1_2014$BEHAV)
qqline(data_8_SCHOOL1_2014$BEHAV)
ks.test(data_8_SCHOOL1_2014$BEHAV,dnorm(mean(data_8_SCHOOL1_2014$BEHAV),sd(data_8_SCHOOL1_2014$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_8_SCHOOL1_2014$BEHAV and dnorm(mean(data_8_SCHOOL1_2014$BEHAV), sd(data_8_SCHOOL1_2014$BEHAV))
# D = 1, p-value = 0.004167
# alternative hypothesis: two-sided
shapiro.test(data_8_SCHOOL1_2014$BEHAV)
# Shapiro-Wilk normality test
# data:  data_8_SCHOOL1_2014$BEHAV
# W = 0.94447, p-value = 6.821e-08
#conclusion: not normal
sample1<-data.frame(sample(data_8_SCHOOL1_2014_M4CID0$BEHAV,70))
sample2<-data.frame(sample(data_8_SCHOOL1_2014_M4CID1$BEHAV,70))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_2014_M4CID0.BEHAV..70.,sample2$sample.data_8_SCHOOL1_2014_M4CID1.BEHAV..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_2014_M4CID0.BEHAV..70. and sample2$sample.data_8_SCHOOL1_2014_M4CID1.BEHAV..70.
# z = 1.4214, p-value = 0.1552
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.105448
var(data_8_SCHOOL1_2014_M4CID0$BEHAV)
var(data_8_SCHOOL1_2014_M4CID1$BEHAV)
hist(data_8_SCHOOL1_2014_M4CID0$BEHAV)
hist(data_8_SCHOOL1_2014_M4CID1$BEHAV)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_8_SCHOOL1_2014) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_8_SCHOOL1_2014_M4CID0$BEHAV)#88
median(data_8_SCHOOL1_2014_M4CID1$BEHAV)#83
wilcox.test(data_8_SCHOOL1_2014_M4CID0$BEHAV,data_8_SCHOOL1_2014_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_2014_M4CID0$BEHAV and data_8_SCHOOL1_2014_M4CID1$BEHAV
# W = 4307, p-value = 0.00486
# alternative hypothesis: true location shift is greater than 0
perm.test(data_8_SCHOOL1_2014_M4CID0$BEHAV,data_8_SCHOOL1_2014_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.01645

# CLASS
hist(data_8_SCHOOL1_2014$CLASS)
qqnorm(data_8_SCHOOL1_2014$CLASS)
qqline(data_8_SCHOOL1_2014$CLASS)
ks.test(data_8_SCHOOL1_2014$CLASS,dnorm(mean(data_8_SCHOOL1_2014$CLASS),sd(data_8_SCHOOL1_2014$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_8_SCHOOL1_2014$CLASS and dnorm(mean(data_8_SCHOOL1_2014$CLASS), sd(data_8_SCHOOL1_2014$CLASS))
# D = 1, p-value = 0.008333
# alternative hypothesis: two-sided
shapiro.test(data_8_SCHOOL1_2014$CLASS)
# Shapiro-Wilk normality test
# data:  data_8_SCHOOL1_2014$CLASS
# W = 0.98399, p-value = 0.008599
#conclusion: not normal
sample1<-data.frame(sample(data_8_SCHOOL1_2014_M4CID0$CLASS,70))
sample2<-data.frame(sample(data_8_SCHOOL1_2014_M4CID1$CLASS,70))
#Kendall correlation test
cor.test(sample1$sample.data_8_SCHOOL1_2014_M4CID0.CLASS..70.,sample2$sample.data_8_SCHOOL1_2014_M4CID1.CLASS..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_8_SCHOOL1_2014_M4CID0.CLASS..70. and sample2$sample.data_8_SCHOOL1_2014_M4CID1.CLASS..70.
# z = 0.055772, p-value = 0.9555
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.004006516 
# Sample independent
var(data_8_SCHOOL1_2014_M4CID0$CLASS)
var(data_8_SCHOOL1_2014_M4CID1$CLASS)
hist(data_8_SCHOOL1_2014_M4CID0$CLASS)
hist(data_8_SCHOOL1_2014_M4CID1$CLASS)
# simmetric, almost same variances, almost same shapes: wilcox almost applicable
ggplot(data_8_SCHOOL1_2014) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_8_SCHOOL1_2014_M4CID0$CLASS)#58
median(data_8_SCHOOL1_2014_M4CID1$CLASS)#60
wilcox.test(data_8_SCHOOL1_2014_M4CID0$CLASS,data_8_SCHOOL1_2014_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_8_SCHOOL1_2014_M4CID0$CLASS and data_8_SCHOOL1_2014_M4CID1$CLASS
# W = 2785.5, p-value = 0.01183
# alternative hypothesis: true location shift is less than 0
perm.test(data_8_SCHOOL1_2014_M4CID0$CLASS,data_8_SCHOOL1_2014_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.00935

# Inference for SCHOOL 1, Grade 9
summary (data)
data_9_SCHOOL1_2014<-filter(data,GRADE =="2" & SCHOOL == "1" & DATE<=2014) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_9_SCHOOL1_2014)
summary(data_9_SCHOOL1_2014)

length(which(data_9_SCHOOL1_2014$M4CID=="0"))#103 obs
length(which(data_9_SCHOOL1_2014$M4CID=="1"))#119 obs

data_9_SCHOOL1_2014_M4CID0 <- filter(data_9_SCHOOL1_2014,M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_9_SCHOOL1_2014_M4CID1 <- filter(data_9_SCHOOL1_2014,M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_9_SCHOOL1_2014_M4CID0)
summary(data_9_SCHOOL1_2014_M4CID1)

# TEST
hist(data_9_SCHOOL1_2014$TEST)
qqnorm(data_9_SCHOOL1_2014$TEST)
qqline(data_9_SCHOOL1_2014$TEST)
ks.test(data_9_SCHOOL1_2014$TEST,dnorm(mean(data_9_SCHOOL1_2014$TEST),sd(data_9_SCHOOL1_2014$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_9_SCHOOL1_2014$TEST and dnorm(mean(data_9_SCHOOL1_2014$TEST), sd(data_9_SCHOOL1_2014$TEST))
# D = 1, p-value = 0.008969
# alternative hypothesis: two-sided
shapiro.test(data_9_SCHOOL1_2014$TEST)
# Shapiro-Wilk normality test
# data:  data_9_SCHOOL1_2014$TEST
# W = 0.97502, p-value = 0.0005698
#conclusion: not normal
sample1<-data.frame(sample(data_9_SCHOOL1_2014_M4CID0$TEST,100))
sample2<-data.frame(sample(data_9_SCHOOL1_2014_M4CID1$TEST,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_2014_M4CID0.TEST..100.,sample2$sample.data_9_SCHOOL1_2014_M4CID1.TEST..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_2014_M4CID0.TEST..100. and sample2$sample.data_9_SCHOOL1_2014_M4CID1.TEST..100.
# z = 0.67348, p-value = 0.5006
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04906577 
#samples are independent
var(data_9_SCHOOL1_2014_M4CID0$TEST)
var(data_9_SCHOOL1_2014_M4CID1$TEST)
hist(data_9_SCHOOL1_2014_M4CID0$TEST)
hist(data_9_SCHOOL1_2014_M4CID1$TEST)
# not simmetric, different variances, diferent shapes: wilcox almost applicable
ggplot(data_9_SCHOOL1_2014) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_9_SCHOOL1_2014_M4CID0$TEST)#56
median(data_9_SCHOOL1_2014_M4CID1$TEST)#58
wilcox.test(data_9_SCHOOL1_2014_M4CID0$TEST,data_9_SCHOOL1_2014_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_9_SCHOOL1_2014_M4CID0$TEST and data_9_SCHOOL1_2014_M4CID1$TEST
# W = 5568.5, p-value = 0.1205
# alternative hypothesis: true location shift is less than 0
perm.test(data_9_SCHOOL1_2014_M4CID0$TEST,data_9_SCHOOL1_2014_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.2055

# LAB
hist(data_9_SCHOOL1_2014$LAB)
qqnorm(data_9_SCHOOL1_2014$LAB)
qqline(data_9_SCHOOL1_2014$LAB)
ks.test(data_9_SCHOOL1_2014$LAB,dnorm(mean(data_9_SCHOOL1_2014$LAB),sd(data_9_SCHOOL1_2014$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_9_SCHOOL1_2014$LAB and dnorm(mean(data_9_SCHOOL1_2014$LAB), sd(data_9_SCHOOL1_2014$LAB))
# D = 0.94643, p-value = 0.1124
# alternative hypothesis: two-sided
shapiro.test(data_9_SCHOOL1_2014$LAB)
# Shapiro-Wilk normality test
# data:  data_9_SCHOOL1_2014$LAB
# W = 0.96125, p-value = 0.0001279
#conclusion: not normal by shapiro analysis
sample1<-data.frame(sample(data_9_SCHOOL1_2014_M4CID0$LAB,100))
sample2<-data.frame(sample(data_9_SCHOOL1_2014_M4CID1$LAB,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_2014_M4CID0.LAB..100.,sample2$sample.data_9_SCHOOL1_2014_M4CID1.LAB..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_2014_M4CID0.LAB..100. and sample2$sample.data_9_SCHOOL1_2014_M4CID1.LAB..100.
# z = 0.28602, p-value = 0.7749
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.02151758 
# sample are independent
var(data_9_SCHOOL1_2014_M4CID0$LAB)
var(data_9_SCHOOL1_2014_M4CID1$LAB)
hist(data_9_SCHOOL1_2014_M4CID0$LAB)
hist(data_9_SCHOOL1_2014_M4CID1$LAB)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_9_SCHOOL1_2014) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_9_SCHOOL1_2014_M4CID0$LAB)#44
median(data_9_SCHOOL1_2014_M4CID1$LAB)#66
wilcox.test(data_9_SCHOOL1_2014_M4CID0$LAB,data_9_SCHOOL1_2014_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_9_SCHOOL1_2014_M4CID0$LAB and data_9_SCHOOL1_2014_M4CID1$LAB
# W = 2193.5, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0
perm.test(data_9_SCHOOL1_2014_M4CID0$LAB,data_9_SCHOOL1_2014_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# BEHAV
hist(data_9_SCHOOL1_2014$BEHAV)
qqnorm(data_9_SCHOOL1_2014$BEHAV)
qqline(data_9_SCHOOL1_2014$BEHAV)
ks.test(data_9_SCHOOL1_2014$BEHAV,dnorm(mean(data_9_SCHOOL1_2014$BEHAV),sd(data_9_SCHOOL1_2014$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_9_SCHOOL1_2014$BEHAV and dnorm(mean(data_9_SCHOOL1_2014$BEHAV), sd(data_9_SCHOOL1_2014$BEHAV))
# D = 1, p-value = 0.004167
# alternative hypothesis: two-sided
shapiro.test(data_9_SCHOOL1_2014$BEHAV)
# Shapiro-Wilk normality test
# data:  data_9_SCHOOL1_2014$BEHAV
# W = 0.94447, p-value = 6.821e-08
#conclusion: not normal
sample1<-data.frame(sample(data_9_SCHOOL1_2014_M4CID0$BEHAV,100))
sample2<-data.frame(sample(data_9_SCHOOL1_2014_M4CID1$BEHAV,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_2014_M4CID0.BEHAV..100.,sample2$sample.data_9_SCHOOL1_2014_M4CID1.BEHAV..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_2014_M4CID0.BEHAV..100. and sample2$sample.data_9_SCHOOL1_2014_M4CID1.BEHAV..100.
# z = 1.4214, p-value = 0.1552
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.105448
var(data_9_SCHOOL1_2014_M4CID0$BEHAV)
var(data_9_SCHOOL1_2014_M4CID1$BEHAV)
hist(data_9_SCHOOL1_2014_M4CID0$BEHAV)
hist(data_9_SCHOOL1_2014_M4CID1$BEHAV)
# not simmetric, different variances, different shapes: wilcox almost applicable
ggplot(data_9_SCHOOL1_2014) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_9_SCHOOL1_2014_M4CID0$BEHAV)#80
median(data_9_SCHOOL1_2014_M4CID1$BEHAV)#85
wilcox.test(data_9_SCHOOL1_2014_M4CID0$BEHAV,data_9_SCHOOL1_2014_M4CID1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_9_SCHOOL1_2014_M4CID0$BEHAV and data_9_SCHOOL1_2014_M4CID1$BEHAV
# W = 5249.5, p-value = 0.03274
# alternative hypothesis: true location shift is greater than 0
perm.test(data_9_SCHOOL1_2014_M4CID0$BEHAV,data_9_SCHOOL1_2014_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.03105

# CLASS
hist(data_9_SCHOOL1_2014$CLASS)
qqnorm(data_9_SCHOOL1_2014$CLASS)
qqline(data_9_SCHOOL1_2014$CLASS)
ks.test(data_9_SCHOOL1_2014$CLASS,dnorm(mean(data_9_SCHOOL1_2014$CLASS),sd(data_9_SCHOOL1_2014$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_9_SCHOOL1_2014$CLASS and dnorm(mean(data_9_SCHOOL1_2014$CLASS), sd(data_9_SCHOOL1_2014$CLASS))
# D = 1, p-value = 0.008333
# alternative hypothesis: two-sided
shapiro.test(data_9_SCHOOL1_2014$CLASS)
# Shapiro-Wilk normality test
# data:  data_9_SCHOOL1_2014$CLASS
# W = 0.98399, p-value = 0.008599
#conclusion: not normal
sample1<-data.frame(sample(data_9_SCHOOL1_2014_M4CID0$CLASS,100))
sample2<-data.frame(sample(data_9_SCHOOL1_2014_M4CID1$CLASS,100))
#Kendall correlation test
cor.test(sample1$sample.data_9_SCHOOL1_2014_M4CID0.CLASS..100.,sample2$sample.data_9_SCHOOL1_2014_M4CID1.CLASS..100.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_9_SCHOOL1_2014_M4CID0.CLASS..100. and sample2$sample.data_9_SCHOOL1_2014_M4CID1.CLASS..100.
# z = 0.055772, p-value = 0.9555
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.004006516 
# Sample independent
var(data_9_SCHOOL1_2014_M4CID0$CLASS)
var(data_9_SCHOOL1_2014_M4CID1$CLASS)
hist(data_9_SCHOOL1_2014_M4CID0$CLASS)
hist(data_9_SCHOOL1_2014_M4CID1$CLASS)
# simmetric, almost same variances, almost same shapes: wilcox almost applicable
ggplot(data_9_SCHOOL1_2014) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_9_SCHOOL1_2014_M4CID0$CLASS)#55
median(data_9_SCHOOL1_2014_M4CID1$CLASS)#64
wilcox.test(data_9_SCHOOL1_2014_M4CID0$CLASS,data_9_SCHOOL1_2014_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_9_SCHOOL1_2014_M4CID0$CLASS and data_9_SCHOOL1_2014_M4CID1$CLASS
# W = 4003.5, p-value = 4.264e-06
# alternative hypothesis: true location shift is less than 0
perm.test(data_9_SCHOOL1_2014_M4CID0$CLASS,data_9_SCHOOL1_2014_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# GRADE conclusion
# GRADE 7
# TEST, LAB and CLASS positive
# BEHAV no effect

# GRADE 8
# TEST, LAB and CLASS positive
# BEHAV negative

# GRADE 9
# TEST no effect
# LAB, BEHAV and CLASS positive

######### RANK

########## RANK
# 2009:2014
#This analysis uses school 1 junior and junior high grade
summary(data)
data_SCHOOL1_JUNIOR_2014<-filter(data,SCHOOL == "1" & GRADE <= 2 & DATE <=2014) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
head(data_SCHOOL1_JUNIOR_2014)
data_SCHOOL1_JUNIOR_2014$RANK <-
  ifelse(data_SCHOOL1_JUNIOR_2014$CLASS <=45, 1,
         ifelse((data_SCHOOL1_JUNIOR_2014$CLASS >45 & data_SCHOOL1_JUNIOR_2014$CLASS<70), 2,
                ifelse(data_SCHOOL1_JUNIOR_2014$CLASS >=70, 3,NA)))
summary(data_SCHOOL1_JUNIOR_2014)
length(data_SCHOOL1_JUNIOR_2014$ID)#535 entrances
length(which(data_SCHOOL1_JUNIOR_2014$RANK=="1"))#63
length(which(data_SCHOOL1_JUNIOR_2014$RANK=="1"))/length(data_SCHOOL1_JUNIOR_2014$ID)#12%
length(which(data_SCHOOL1_JUNIOR_2014$RANK=="2"))#330
length(which(data_SCHOOL1_JUNIOR_2014$RANK=="2"))/length(data_SCHOOL1_JUNIOR_2014$ID)#62%
length(which(data_SCHOOL1_JUNIOR_2014$RANK=="3"))#142
length(which(data_SCHOOL1_JUNIOR_2014$RANK=="3"))/length(data_SCHOOL1_JUNIOR_2014$ID)#27%

# 35 obs.
#RANK 1, 63,12%
#RANK 2, 330, 62%
#RANK 3, 142, 27% 

#RANK 1
data_SCHOOL1_JUNIOR_2014_RANK1 <- filter(data_SCHOOL1_JUNIOR_2014,RANK=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0 <- filter(data_SCHOOL1_JUNIOR_2014_RANK1, M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1 <- filter(data_SCHOOL1_JUNIOR_2014_RANK1, M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0)
length(data_SCHOOL1_JUNIOR_2014_RANK1$ID)#63
length(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$ID)#56
length(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$ID)#8 
#note: low number of obs: permutation techniques

# TEST
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$TEST,10))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$TEST,10))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.TEST..10.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.TEST..10.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.TEST..10. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.TEST..10.
# z = 0.72415, p-value = 0.469
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.1839202 
library("Kendall")
Kendall(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.TEST..10.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.TEST..10.)
#Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.TEST..30. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.TEST..30.
# z = 0.7897, p-value = 0.4297
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.104641 
# sample independent
hist(data_SCHOOL1_JUNIOR_2014_RANK1$TEST)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK1$TEST)
qqline(data_SCHOOL1_JUNIOR_2014_RANK1$TEST)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK1$TEST,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK1$TEST),sd(data_SCHOOL1_JUNIOR_2014_RANK1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK1$TEST and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK1$TEST), sd(data_SCHOOL1_JUNIOR_2014_RANK1$TEST))
# D = 0.97849, p-value = 0.03125
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK1$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK1$TEST
# W = 0.97386, p-value = 0.1992
#conclusion: normal
var(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$TEST)
var(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$TEST)
hist(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$TEST)
hist(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$TEST)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$TEST)#33
median(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$TEST)#36
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$TEST,data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$TEST and data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$TEST
# W = 227, p-value = 0.5616
# alternative hypothesis: true location shift is not equal to 0
perm.test( data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$TEST,data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.3991

# LAB
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$LAB,10))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$LAB,10))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.LAB..10.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.LAB..10.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.LAB..30. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.LAB..30.
# z = -0.92261, p-value = 0.3562
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1239852 
#sample independent
hist(data_SCHOOL1_JUNIOR_2014_RANK1$LAB)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK1$LAB)
qqline(data_SCHOOL1_JUNIOR_2014_RANK1$LAB)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK1$LAB,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK1$LAB),sd(data_SCHOOL1_JUNIOR_2014_RANK1$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK1$LAB and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK1$LAB), sd(data_SCHOOL1_JUNIOR_2014_RANK1$LAB))
# D = 0.8254, p-value = 0.375
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK1$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK1$LAB
# W = 0.92288, p-value = 0.0007297
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$LAB)
var(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$LAB)
hist(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$LAB)
hist(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$LAB)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$LAB)#26
median(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$LAB)#35
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$LAB,data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$LAB,alternative = "less")
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK1$LAB
# W = 144, p-value = 0.05893
perm.test( data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$LAB,data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value =  0.11485

# BEHAV
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$BEHAV,10))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$BEHAV,10))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.BEHAV..10.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.BEHAV..10.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.BEHAV..30. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.BEHAV..30.
# z = -1.3245, p-value = 0.1853
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1739132 
#sample independent
hist(data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV)
qqline(data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV),sd(data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV), sd(data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV))
# D = 1, p-value = 0.03125
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK1$BEHAV
# W = 0.98978, p-value = 0.8823
#conclusion: normal
var(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$BEHAV)
var(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$BEHAV)
hist(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$BEHAV)
hist(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$BEHAV)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$BEHAV)#65
median(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$BEHAV)#60
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$BEHAV and data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$BEHAV
# W = 273, p-value = 0.8655
# alternative hypothesis: true location shift is greater than 0
perm.test( data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.3851

# CLASS
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$CLASS,10))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$CLASS,10))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.CLASS..10.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.CLASS..10.,method="kendall",exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0.CLASS..30. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1.CLASS..30.
# z = 0.62483, p-value = 0.5321
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.08083141 
hist(data_SCHOOL1_JUNIOR_2014_RANK1$CLASS)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK1$CLASS)
qqline(data_SCHOOL1_JUNIOR_2014_RANK1$CLASS)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK1$CLASS,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK1$CLASS),sd(data_SCHOOL1_JUNIOR_2014_RANK1$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK1$CLASS and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK1$CLASS), sd(data_SCHOOL1_JUNIOR_2014_RANK1$CLASS))
# D = 1, p-value = 0.01562
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK1$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK1$CLASS
# W = 0.87193, p-value = 9.393e-06
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$CLASS)
var(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$CLASS)
hist(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$CLASS)
hist(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$CLASS)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$CLASS)#39
median(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$CLASS)#39
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$CLASS,data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$CLASS,alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$CLASS and data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$CLASS
# W = 244, p-value = 0.6276
# alternative hypothesis: true location shift is not equal to 0
perm.test( data_SCHOOL1_JUNIOR_2014_RANK1_M4CID0$CLASS,data_SCHOOL1_JUNIOR_2014_RANK1_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.95525

#RANK 2
data_SCHOOL1_JUNIOR_2014_RANK2 <- filter(data_SCHOOL1_JUNIOR_2014,RANK=="2") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0 <- filter(data_SCHOOL1_JUNIOR_2014_RANK2, M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1 <- filter(data_SCHOOL1_JUNIOR_2014_RANK2, M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0)
length(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$ID)#196
length(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$ID)#134

# TEST
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$TEST,150))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$TEST,150))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0.TEST..150.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1.TEST..150.,method="kendall")
#KKendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0.TEST..150. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1.TEST..150.
# z = -0.19579, p-value = 0.8448
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.009561547 
# sample independent
hist(data_SCHOOL1_JUNIOR_2014_RANK2$TEST)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK2$TEST)
qqline(data_SCHOOL1_JUNIOR_2014_RANK2$TEST)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK2$TEST,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK2$TEST),sd(data_SCHOOL1_JUNIOR_2014_RANK2$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK2$TEST and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK2$TEST), sd(data_SCHOOL1_JUNIOR_2014_RANK2$TEST))
# D = 1, p-value = 0.006042
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK2$TEST)
# Exact two-sample Kolmogorov-Smirnov test
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK2$TEST
# W = 0.99574, p-value = 0.5109
#conclusion: normal
var(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$TEST)
var(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$TEST)
hist(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$TEST)
hist(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$TEST)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK2) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$TEST)#53
median(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$TEST)#51
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$TEST,data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$TEST and data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$TEST
# W = 14540, p-value = 0.04908
# alternative hypothesis: true location shift is greater than 0
perm.test(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$TEST,data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.1002

# LAB
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$LAB,150))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$LAB,150))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0.LAB..150.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1.LAB..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0.LAB..150. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1.LAB..150.
# z = 0.96981, p-value = 0.3321
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04735476 
#sample independent
hist(data_SCHOOL1_JUNIOR_2014_RANK2$LAB)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK2$LAB)
qqline(data_SCHOOL1_JUNIOR_2014_RANK2$LAB)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK2$LAB,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK2$LAB),sd(data_SCHOOL1_JUNIOR_2014_RANK2$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK2$LAB and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK2$LAB), sd(data_SCHOOL1_JUNIOR_2014_RANK2$LAB))
# D = 0.99394, p-value = 0.01511
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK2$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK2$LAB
# W = 0.96562, p-value = 4.909e-07
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$LAB)
var(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$LAB)
hist(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$LAB)
hist(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$LAB)
# almost simmetric, different variances, same shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK2) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$LAB)#51
median(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$LAB)#58
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$LAB,data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$LAB and data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$LAB
# W = 9728.5, p-value = 3.16e-05
# alternative hypothesis: true location shift is less than 0
perm.test(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$LAB,data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

# BEHAV
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$BEHAV,150))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$BEHAV,150))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0.BEHAV..150.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1.BEHAV..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0.BEHAV..150. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1.BEHAV..150.
# z = 0.075125, p-value = 0.9401
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.003660025 
#sample independent
hist(data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV)
qqline(data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV),sd(data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV), sd(data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV))
# D = 1,  p-value = 0.003021
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK2$BEHAV
# W  = 0.98156, p-value = 0.000306
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$BEHAV)
var(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$BEHAV)
hist(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$BEHAV)
hist(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$BEHAV)
# not simmetric, same variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK2) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$BEHAV)#80
median(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$BEHAV)#79
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$BEHAV and data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$BEHAV
# W = 14216, p-value = 0.1015
# alternative hypothesis: true location shift is greater than 0
perm.test(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.1605

# CLASS
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$CLASS,150))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$CLASS,150))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0.CLASS..150.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1.CLASS..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0.CLASS..150. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1.CLASS..150.
# z = 0.51255, p-value = 0.6083
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.02445235 
hist(data_SCHOOL1_JUNIOR_2014_RANK2$CLASS)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK2$CLASS)
qqline(data_SCHOOL1_JUNIOR_2014_RANK2$CLASS)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK2$CLASS,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK2$CLASS),sd(data_SCHOOL1_JUNIOR_2014_RANK2$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK2$CLASS and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK2$CLASS), sd(data_SCHOOL1_JUNIOR_2014_RANK2$CLASS))
# D = 1,  p-value = 0.006042
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK2$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK2$CLASS
# W = 0.96409, p-value = 2.897e-07
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$CLASS)
var(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$CLASS)
hist(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$CLASS)
hist(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$CLASS)
# not simmetric, similar variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK2) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$CLASS)#57
median(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$CLASS)#59
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$CLASS,data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$CLASS and data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$CLASS
# W = 12198, p-value = 0.1365
# alternative hypothesis: true location shift is less than 0
perm.test(data_SCHOOL1_JUNIOR_2014_RANK2_M4CID0$CLASS,data_SCHOOL1_JUNIOR_2014_RANK2_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.29695

#RANK 3
data_SCHOOL1_JUNIOR_2014_RANK3 <- filter(data_SCHOOL1_JUNIOR_2014,RANK=="3") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0 <- filter(data_SCHOOL1_JUNIOR_2014_RANK3, M4CID=="0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1 <- filter(data_SCHOOL1_JUNIOR_2014_RANK3, M4CID=="1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0)
length(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$ID)#67
length(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$ID)#75

# TEST
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$TEST,70))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$TEST,70))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0.TEST..70.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1.TEST..70.,method="kendall")
#Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0.TEST..70. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1.TEST..70.
# z = -0.8738, p-value = 0.3822
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.07306728  sample independent
hist(data_SCHOOL1_JUNIOR_2014_RANK3$TEST)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK3$TEST)
qqline(data_SCHOOL1_JUNIOR_2014_RANK3$TEST)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK3$TEST,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK3$TEST),sd(data_SCHOOL1_JUNIOR_2014_RANK3$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK3$TEST and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK3$TEST), sd(data_SCHOOL1_JUNIOR_2014_RANK3$TEST))
# D = 1, p-value = 0.01399
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK3$TEST)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK3$TEST
# W = 0.98568, p-value = 0.1472
#conclusion: normal
var(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$TEST)
var(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$TEST)
hist(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$TEST)
hist(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$TEST)
# not simmetric, similar variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK3) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$TEST)#79
median(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$TEST)#81
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$TEST,data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$TEST and data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$TEST
# W = 2331.5, p-value = 0.2302
# alternative hypothesis: true location shift is less than 0
perm.test(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$TEST,data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.4077

# LAB
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$LAB,70))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$LAB,70))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0.LAB..70.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1.LAB..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0.LAB..70. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1.LAB..70.
# z = 0.45751, p-value = 0.6473
# alternative hypothesis: true tau is not equal to 0
# sample estimates:tau 0.03836334 
#sample independent
hist(data_SCHOOL1_JUNIOR_2014_RANK3$LAB)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK3$LAB)
qqline(data_SCHOOL1_JUNIOR_2014_RANK3$LAB)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK3$LAB,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK3$LAB),sd(data_SCHOOL1_JUNIOR_2014_RANK3$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK3$LAB and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK3$LAB), sd(data_SCHOOL1_JUNIOR_2014_RANK3$LAB))
# D = 1, p-value = 0.01399
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK3$LAB)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK3$LAB
# W = 0.96526, p-value = 0.001137
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$LAB)
var(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$LAB)
hist(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$LAB)
hist(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$LAB)
# not simmetric, diferent variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK3) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$LAB)#73
median(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$LAB)#80
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$LAB,data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$LAB and data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$LAB
# W = 1630.5, p-value = 0.0001566
# alternative hypothesis: true location shift is less than 0
perm.test(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$LAB,data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 5e-05

# BEHAV
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$BEHAV,70))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$BEHAV,70))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0.BEHAV..70.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1.BEHAV..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0.BEHAV..70. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1.BEHAV..70.
# z = -1.7015, p-value = 0.08885
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1456001 
#sample independent
hist(data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV)
qqline(data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV),sd(data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV), sd(data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV))
# D = 1, p-value = 0.006993
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK3$BEHAV
# W = 0.91537, p-value = 2.049e-07
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$BEHAV)
var(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$BEHAV)
hist(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$BEHAV)
hist(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$BEHAV)
# not simmetric, diferent variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK3) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$BEHAV)#94
median(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$BEHAV)#92
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$BEHAV and data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$BEHAV
# W = 2691.5, p-value = 0.2323
# alternative hypothesis: true location shift is greater than 0
perm.test(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$BEHAV,data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.64225

# CLASS
# independency
sample1<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$CLASS,70))
sample2<-data.frame(sample(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$CLASS,70))
#Kendall correlation test
cor.test(sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0.CLASS..70.,sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1.CLASS..70.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0.CLASS..70. and sample2$sample.data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1.CLASS..70.
# z = 0.49191, p-value = 0.6228
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04034947 
hist(data_SCHOOL1_JUNIOR_2014_RANK3$CLASS)
qqnorm(data_SCHOOL1_JUNIOR_2014_RANK3$CLASS)
qqline(data_SCHOOL1_JUNIOR_2014_RANK3$CLASS)
ks.test(data_SCHOOL1_JUNIOR_2014_RANK3$CLASS,dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK3$CLASS),sd(data_SCHOOL1_JUNIOR_2014_RANK3$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_SCHOOL1_JUNIOR_2014_RANK3$CLASS and dnorm(mean(data_SCHOOL1_JUNIOR_2014_RANK3$CLASS), sd(data_SCHOOL1_JUNIOR_2014_RANK3$CLASS))
# D = 1, p-value = 0.0122
# alternative hypothesis: two-sided
shapiro.test(data_SCHOOL1_JUNIOR_2014_RANK3$CLASS)
# Shapiro-Wilk normality test
# data:  data_SCHOOL1_JUNIOR_2014_RANK3$CLASS
# W = 0.93157, p-value = 5.132e-07
#conclusion: not normal
var(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$CLASS)
var(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$CLASS)
hist(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$CLASS)
hist(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$CLASS)
# not simmetric, diferent variances, diferent shapes: wilcox no applicable
ggplot(data_SCHOOL1_JUNIOR_2014_RANK3) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$CLASS)#77
median(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$CLASS)#81
wilcox.test(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$CLASS,data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$CLASS and data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$CLASS
# W = 1839.5, p-value = 0.002995
# alternative hypothesis: true location shift is less than 0
perm.test(data_SCHOOL1_JUNIOR_2014_RANK3_M4CID0$CLASS,data_SCHOOL1_JUNIOR_2014_RANK3_M4CID1$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.00895

#??? confirmed???
# RANK 1, school all sample: 2009:2014
# Negative effect for BEHAV;
# no effect for TEST, LAB and CLASS

# RANK 2, school all sample: 2009:2014
# Positive effect for LAB;
# negative for TEST, BEHAV 
# no effect for CLASS

# RANK 3, school all sample: 2009:2014
# Positive effect for LAB and CLASS;
# no effect for TEST, BEHAV 


########## GENDER
# sample: school 1 JUNIOR, 2009:2014
#This analysis uses school 1 and junior grade sample
summary(data)
data_junior_SCHOOL1_2014<-filter(data,GRADE <="2" & SCHOOL=="1" & DATE<=2014) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_2014_FEMALE<-filter(data,GRADE<="2" & SCHOOL=="1" & GENDER=="F" & DATE<=2014) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_2014_MALE<-filter(data,GRADE<="2" & SCHOOL=="1"& GENDER=="M" & DATE<=2014) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_2014_M4CID0_MALE<-filter(data,GRADE <="2" & SCHOOL=="1" & M4CID =="0"  & GENDER == "M") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_2014_M4CID0_FEMALE<-filter(data,GRADE<="2" & SCHOOL=="1" & M4CID =="0" & GENDER == "F") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_2014_M4CID1_MALE<-filter(data, GRADE <="2" & SCHOOL=="1" & M4CID =="1"  & GENDER == "M") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_junior_SCHOOL1_2014_M4CID1_FEMALE<-filter(data,GRADE <="2" & SCHOOL=="1" & M4CID =="1" & GENDER == "F") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_junior_SCHOOL1_2014)
length(data_junior_SCHOOL1_2014$ID)#535
length(which(data_junior_SCHOOL1_2014$GENDER=="F"))#263
length(which(data_junior_SCHOOL1_2014$GENDER=="M"))#272
length(which(data_junior_SCHOOL1_2014$GENDER=="F"))/length(data_junior_SCHOOL1_2014$ID)#49%
length(which(data_junior_SCHOOL1_2014$GENDER=="M"))/length(data_junior_SCHOOL1_2014$ID)#51%
length(data_junior_SCHOOL1_2014_M4CID0_FEMALE$ID)#160
length(data_junior_SCHOOL1_2014_M4CID1_FEMALE$ID)#166
length(data_junior_SCHOOL1_2014_M4CID0_MALE$ID)#158
length(data_junior_SCHOOL1_2014_M4CID1_MALE$ID)#199

# sample entries: 535; Female = 263 (49%); Male=272 (51%);
# female: M4CID0 = 160; M4CID1=166
# male: M4CID0 = 158; M4CID1=199
# data sample not normal but independent

#TEST
# independency
sample1<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID0_FEMALE$TEST,150))
sample2<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID1_FEMALE$TEST,150))
#Kendall correlation test
cor.test(sample1$sample.data_junior_SCHOOL1_2014_M4CID0_FEMALE.TEST..150.,sample2$sample.data_junior_SCHOOL1_2014_M4CID1_FEMALE.TEST..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_junior_SCHOOL1_2014_M4CID0_FEMALE.TEST..150. and sample2$sample.data_junior_SCHOOL1_2014_M4CID1_FEMALE.TEST..150.
# z = 1.405, p-value = 0.16
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.07853475 
sample3<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID0_MALE$TEST,150))
sample4<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID1_MALE$TEST,150))
#Kendall correlation test
cor.test(sample3$sample.data_junior_SCHOOL1_2014_M4CID0_MALE.TEST..150.,sample4$sample.data_junior_SCHOOL1_2014_M4CID1_MALE.TEST..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_junior_SCHOOL1_2014_M4CID0_MALE.TEST..150. and sample4$sample.data_junior_SCHOOL1_2014_M4CID1_MALE.TEST..150.
# z = 0.55939, p-value = 0.5759
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.03126279
# samples are independent
var(data_junior_SCHOOL1_2014_M4CID0_FEMALE$TEST)
var(data_junior_SCHOOL1_2014_M4CID1_FEMALE$TEST)
var(data_junior_SCHOOL1_2014_M4CID0_MALE$TEST)
var(data_junior_SCHOOL1_2014_M4CID1_MALE$TEST)
hist(data_junior_SCHOOL1_2014_M4CID0_FEMALE$TEST)
hist(data_junior_SCHOOL1_2014_M4CID1_FEMALE$TEST)
hist(data_junior_SCHOOL1_2014_M4CID0_MALE$TEST)
hist(data_junior_SCHOOL1_2014_M4CID1_MALE$TEST)
# not simmetric, almost same variances, diferent shapes: wilcox no applicable
ggplot(data_junior_SCHOOL1_2014) + geom_boxplot(aes(x=factor(M4CID),y=TEST, linetype = factor(GENDER)))
median(data_junior_SCHOOL1_2014_M4CID0_FEMALE$TEST)#54
median(data_junior_SCHOOL1_2014_M4CID1_FEMALE$TEST)#61
median(data_junior_SCHOOL1_2014_M4CID0_MALE$TEST)#54
median(data_junior_SCHOOL1_2014_M4CID1_MALE$TEST)#57
#It seems that the female benefit, less impact on male
wilcox.test(data_junior_SCHOOL1_2014_M4CID0_FEMALE$TEST,data_junior_SCHOOL1_2014_M4CID1_FEMALE$TEST, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_2014_M4CID0_FEMALE$TEST and data_junior_SCHOOL1_2014_M4CID1_FEMALE$TEST
# W = 10924, p-value = 0.002804
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_2014_M4CID0_MALE$TEST,data_junior_SCHOOL1_2014_M4CID1_MALE$TEST, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_2014_M4CID0_MALE$TEST and data_junior_SCHOOL1_2014_M4CID1_MALE$TEST
# W = 14188, p-value = 0.05675
# alternative hypothesis: true location shift is less than 0
perm.test(data_junior_SCHOOL1_2014_M4CID0_FEMALE$TEST,data_junior_SCHOOL1_2014_M4CID1_FEMALE$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.00285
perm.test(data_junior_SCHOOL1_2014_M4CID0_MALE$TEST,data_junior_SCHOOL1_2014_M4CID1_MALE$TEST)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.0972

#LAB
sample1<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID0_FEMALE$LAB,150))
sample2<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID1_FEMALE$LAB,150))
#Kendall correlation test
cor.test(sample1$sample.data_junior_SCHOOL1_2014_M4CID0_FEMALE.LAB..150.,sample2$sample.data_junior_SCHOOL1_2014_M4CID1_FEMALE.LAB..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_junior_SCHOOL1_2014_M4CID0_FEMALE.LAB..150. and sample2$sample.data_junior_SCHOOL1_2014_M4CID1_FEMALE.LAB..150.
# z = -1.2413, p-value = 0.2145
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.06971223 
sample3<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID0_MALE$LAB,150))
sample4<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID1_MALE$LAB,150))
#Kendall correlation test
cor.test(sample3$sample.data_junior_SCHOOL1_2014_M4CID0_MALE.LAB..150.,sample4$sample.data_junior_SCHOOL1_2014_M4CID1_MALE.LAB..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_junior_SCHOOL1_2014_M4CID0_MALE.LAB..150. and sample4$sample.data_junior_SCHOOL1_2014_M4CID1_MALE.LAB..150.
# z = 1.2979, p-value = 0.1943
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.07271416 
# samples are independent
median(data_junior_SCHOOL1_2014_M4CID0_FEMALE$LAB)#50
median(data_junior_SCHOOL1_2014_M4CID1_FEMALE$LAB)#70
median(data_junior_SCHOOL1_2014_M4CID0_MALE$LAB)#53
median(data_junior_SCHOOL1_2014_M4CID1_MALE$LAB)#61
#It seems that the female benefit, less impact on male
var(data_junior_SCHOOL1_2014_M4CID0_FEMALE$LAB)
var(data_junior_SCHOOL1_2014_M4CID1_FEMALE$LAB)
var(data_junior_SCHOOL1_2014_M4CID0_MALE$LAB)
var(data_junior_SCHOOL1_2014_M4CID1_MALE$LAB)
hist(data_junior_SCHOOL1_2014_M4CID0_FEMALE$LAB)
hist(data_junior_SCHOOL1_2014_M4CID1_FEMALE$LAB)
hist(data_junior_SCHOOL1_2014_M4CID0_MALE$LAB)
hist(data_junior_SCHOOL1_2014_M4CID1_MALE$LAB)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_junior_SCHOOL1_2014) + geom_boxplot(aes(x=factor(M4CID),y=LAB, linetype = factor(GENDER)))
#It seems that both male/female benefit
wilcox.test(data_junior_SCHOOL1_2014_M4CID0_FEMALE$LAB,data_junior_SCHOOL1_2014_M4CID1_FEMALE$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_2014_M4CID0_FEMALE$LAB and data_junior_SCHOOL1_2014_M4CID1_FEMALE$LAB
# W = 5499.5, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_2014_M4CID0_MALE$LAB,data_junior_SCHOOL1_2014_M4CID1_MALE$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_2014_M4CID0_MALE$LAB and data_junior_SCHOOL1_2014_M4CID1_MALE$LAB
# W = 11162, p-value = 1.249e-06
# alternative hypothesis: true location shift is less than 0
perm.test(data_junior_SCHOOL1_2014_M4CID0_FEMALE$LAB,data_junior_SCHOOL1_2014_M4CID1_FEMALE$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0
perm.test(data_junior_SCHOOL1_2014_M4CID0_MALE$LAB,data_junior_SCHOOL1_2014_M4CID1_MALE$LAB)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0

#BEHAV
# Independency
sample1<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID0_FEMALE$BEHAV,150))
sample2<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID1_FEMALE$BEHAV,150))
#Kendall correlation test
cor.test(sample1$sample.data_junior_SCHOOL1_2014_M4CID0_FEMALE.BEHAV..150.,sample2$sample.data_junior_SCHOOL1_2014_M4CID1_FEMALE.BEHAV..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_junior_SCHOOL1_2014_M4CID0_FEMALE.BEHAV..150. and sample2$sample.data_junior_SCHOOL1_2014_M4CID1_FEMALE.BEHAV..150.
# z = 0.087917, p-value = 0.9299
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.004975126 
sample3<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID0_MALE$BEHAV,150))
sample4<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID1_MALE$BEHAV,150))
#Kendall correlation test
cor.test(sample3$sample.data_junior_SCHOOL1_2014_M4CID0_MALE.BEHAV..150.,sample4$sample.data_junior_SCHOOL1_2014_M4CID1_MALE.BEHAV..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_junior_SCHOOL1_2014_M4CID0_MALE.BEHAV..150. and sample4$sample.data_junior_SCHOOL1_2014_M4CID1_MALE.BEHAV..150.
# z = 0.34496, p-value = 0.7301
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01941931 
# sampleas are independent
median(data_junior_SCHOOL1_2014_M4CID0_FEMALE$BEHAV)#82
median(data_junior_SCHOOL1_2014_M4CID1_FEMALE$BEHAV)#82
median(data_junior_SCHOOL1_2014_M4CID0_MALE$BEHAV)#80
median(data_junior_SCHOOL1_2014_M4CID1_MALE$BEHAV)#72
var(data_junior_SCHOOL1_2014_M4CID0_FEMALE$BEHAV)
var(data_junior_SCHOOL1_2014_M4CID1_FEMALE$BEHAV)
var(data_junior_SCHOOL1_2014_M4CID0_MALE$BEHAV)
var(data_junior_SCHOOL1_2014_M4CID1_MALE$BEHAV)
hist(data_junior_SCHOOL1_2014_M4CID0_FEMALE$BEHAV)
hist(data_junior_SCHOOL1_2014_M4CID1_FEMALE$BEHAV)
hist(data_junior_SCHOOL1_2014_M4CID0_MALE$BEHAV)
hist(data_junior_SCHOOL1_2014_M4CID1_MALE$BEHAV)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_junior_SCHOOL1_2014) + geom_boxplot(aes(x=factor(M4CID),y=BEHAV, linetype = factor(GENDER)))
#It seems that the male/female don't benefit, male is worse
wilcox.test(data_junior_SCHOOL1_2014_M4CID0_FEMALE$BEHAV,data_junior_SCHOOL1_2014_M4CID1_FEMALE$BEHAV, alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_2014_M4CID0_FEMALE$BEHAV and data_junior_SCHOOL1_2014_M4CID1_FEMALE$BEHAV
# W = 13459, p-value = 0.8337
# alternative hypothesis: true location shift is not equal to 0
wilcox.test(data_junior_SCHOOL1_2014_M4CID0_MALE$BEHAV,data_junior_SCHOOL1_2014_M4CID1_MALE$BEHAV, alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_2014_M4CID0_MALE$BEHAV and data_junior_SCHOOL1_2014_M4CID1_MALE$BEHAV
# W = 19768, p-value = 1.463e-05
# alternative hypothesis: true location shift is greater than 0
perm.test(data_junior_SCHOOL1_2014_M4CID0_FEMALE$BEHAV,data_junior_SCHOOL1_2014_M4CID1_FEMALE$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.73165
perm.test(data_junior_SCHOOL1_2014_M4CID0_MALE$BEHAV,data_junior_SCHOOL1_2014_M4CID1_MALE$BEHAV)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 5e-05

#CLASS
#Independency
sample1<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID0_FEMALE$CLASS,150))
sample2<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID1_FEMALE$CLASS,150))
#Kendall correlation test
cor.test(sample1$sample.data_junior_SCHOOL1_2014_M4CID0_FEMALE.CLASS..150.,sample2$sample.data_junior_SCHOOL1_2014_M4CID1_FEMALE.CLASS..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_junior_SCHOOL1_2014_M4CID0_FEMALE.CLASS..150. and sample2$sample.data_junior_SCHOOL1_2014_M4CID1_FEMALE.CLASS..150.
# z = 0.32176, p-value = 0.7476
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.0177475 
sample3<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID0_MALE$CLASS,150))
sample4<-data.frame(sample(data_junior_SCHOOL1_2014_M4CID1_MALE$CLASS,150))
#Kendall correlation test
cor.test(sample3$sample.data_junior_SCHOOL1_2014_M4CID0_MALE.CLASS..150.,sample4$sample.data_junior_SCHOOL1_2014_M4CID1_MALE.CLASS..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_junior_SCHOOL1_2014_M4CID0_MALE.CLASS..150. and sample4$sample.data_junior_SCHOOL1_2014_M4CID1_MALE.CLASS..150.
# z = 0.45176, p-value = 0.6514
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.02490928 
#samples are independent
median(data_junior_SCHOOL1_2014_M4CID0_FEMALE$CLASS)#58
median(data_junior_SCHOOL1_2014_M4CID1_FEMALE$CLASS)#67
median(data_junior_SCHOOL1_2014_M4CID0_MALE$CLASS)#58
median(data_junior_SCHOOL1_2014_M4CID1_MALE$CLASS)#60
var(data_junior_SCHOOL1_2014_M4CID0_FEMALE$BEHAV)
var(data_junior_SCHOOL1_2014_M4CID1_FEMALE$BEHAV)
var(data_junior_SCHOOL1_2014_M4CID0_MALE$BEHAV)
var(data_junior_SCHOOL1_2014_M4CID1_MALE$BEHAV)
hist(data_junior_SCHOOL1_2014_M4CID0_FEMALE$BEHAV)
hist(data_junior_SCHOOL1_2014_M4CID1_FEMALE$BEHAV)
hist(data_junior_SCHOOL1_2014_M4CID0_MALE$BEHAV)
hist(data_junior_SCHOOL1_2014_M4CID1_MALE$BEHAV)
# not simmetric, different variances, diferent shapes: wilcox no applicable
ggplot(data_junior_SCHOOL1_2014) + geom_boxplot(aes(x=factor(M4CID),y=CLASS, linetype = factor(GENDER)))
#It seems that the female have more benefit then the male
wilcox.test(data_junior_SCHOOL1_2014_M4CID0_FEMALE$CLASS,data_junior_SCHOOL1_2014_M4CID1_FEMALE$CLASS, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_2014_M4CID0_FEMALE$CLASS and data_junior_SCHOOL1_2014_M4CID1_FEMALE$CLASS
# W = 8602.5, p-value = 1.925e-08
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_junior_SCHOOL1_2014_M4CID0_MALE$CLASS,data_junior_SCHOOL1_2014_M4CID1_MALE$CLASS, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_junior_SCHOOL1_2014_M4CID0_MALE$CLASS and data_junior_SCHOOL1_2014_M4CID1_MALE$CLASS
# W = 13812, p-value = 0.02442
# alternative hypothesis: true location shift is not equal to 0
perm.test(data_junior_SCHOOL1_2014_M4CID0_FEMALE$CLASS,data_junior_SCHOOL1_2014_M4CID1_FEMALE$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0
perm.test(data_junior_SCHOOL1_2014_M4CID0_MALE$CLASS,data_junior_SCHOOL1_2014_M4CID1_MALE$CLASS)
# "Unpaired two-sample permutation test was performed."
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "two.sided" $mu [1] 0 $p.value = 0.0211

# SCHOOL 1 JUNIOR sample 2009:2014: GENDER
# FEMALE: TEST, LAB and CLASS positive; no effect on BEHAV
# MALE: positive impact on LAB and CLASS, negative on BEHAV and no impact on TEST

# Female, overall, benefit more; no different between both samples: school 1 with 
# junior and school 1 with junior and junior high

