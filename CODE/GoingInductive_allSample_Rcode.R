#INDEX
# Libraries and data upload
# RESULTS 2003:2017 - 41
  ## all sample Inference - 41
  ## school effect testing - 206
  ## !!! computing for school effect - 290
  ## all sample with school effect: 2003-2017 - 373
  ## Inference junior sample with school effect - 532
  ## Inference for treatment effect all sample, including School 1 just with 4C/ID - 649
  ## GRADE 7 (School effect) - 767
  ## GRADE 8 (School effect) - 880
  ## GRADE 9 (School effect) - 992
  ## RANK - 1104
  ## GENDER - 1714
# RESULTS 2003:2014 - 2017
## all sample Inference - 2017
## all sample with school effect: 2003-2017 - 2175
## Inference junior sample with school effect - 2327
## Inference for treatment effect all sample, including School 1 just with 4C/ID - 
## GRADE 7 (School effect) - 
## GRADE 8 (School effect) - 
## GRADE 9 (School effect) - 
## RANK - 
## GENDER - 

# libraries 
library(dplyr) #work with data frames
#library(tidyverse) #include dplyr
library(ggplot2) # graphs
library(gridExtra) #arrange graphs in rows

# data upload
#Use this step before any of the analysis bellow
# 2003 to 2017
#perhaps you need to set your set working directory: setwd: setwd("/Users/novo/Desktop/Doutoramento/GoingInductive")
setwd("/Users/novo/Desktop/Doutoramento/GoingInductivePaper/GoingInductive/DATA")
data<-read.csv("./DATA_4CID_2023.csv") #the date 2023 on this csv file means that the data is used for 2023 paper going indcutive
data<-data %>% mutate(CLASS=0.5*TEST+0.3*LAB+0.2*BEHAV)
colnames(data) <- c("ID","DATE" ,"SCHOOL" ,"GENDER" ,"TEST" ,"LAB" ,"BEHAV" ,"GRADE" ,"M4CID","CLASS")
head(data)
# sample differences between school 0 and school 1
summary(data)

# RESULTS: 2003 - 2017
#Inference (and independency analysis)
#Inference treatment and control group for all sample, no matter which school, 
#for each data variables: TEST, LAB and BEHAV
data_M4CID0<- filter(data,M4CID =="0") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
data_M4CID1<-filter(data,M4CID =="1") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
length(data_M4CID0$ID)#828
length(data_M4CID1$ID)#586
# Along the way, sample independency will be tested and non normality will be considered

#TEST
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
# normality
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

# independency
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
# normality
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

# independency
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
# Normality
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

#independency
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
wilcox.test(data_M4CID0$CLASS,data_M4CID1$CLASS,alternative = "greater",conf.int = TRUE)
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0$CLASS and data_M4CID1$CLASS
# W = 252126, p-value = 0.104
# alternative hypothesis: true location shift is greater than 0

# Conclusion
# Samples are independent and not normal, which implies Wilcoxon inference non-parametric test
# for the global TEST and BEHAV, negative 4CID effect
# no effect for CLASS variable;
# LAB variable, positive effect

#Table ???: independency and Wilcoxon rank sum test with continuity correction

#Hypothesis: does the patterns showed in the last graphs where there was a negative bump during the transition to the second 
#between the schools showed earlier in the graphs should be considered?
#Is there a school effect on the results?
#should the data between schools in the same 4CID conditions 0 be assessed? yes,
#but only the junior school should be compared
#we should use the CLASS to study the differences between schools and not the other
#variables; if we use the variable because it is a math relation qith the others
# just with class variable

# all sample
# Testing the school effect: is there one?
# Inference for the differences in schools (control group), without 4CID, for each variable: TEST, LAB, BEHAV and CLASS
## DATA
summary(data)
data_M4CID0<- r(data,data$M4CID=="0")  %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
summary(data_M4CID0)
length(which(data_M4CID0$SCHOOL=="0"))#476 entrances
length(which(data_M4CID0$SCHOOL=="1"))#352 entrances

data_M4CID0_SCHOOL0 <- filter(data,SCHOOL == "0") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
data_M4CID0_SCHOOL1 <- filter(data,M4CID=="0" & SCHOOL == "1") %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
summary(data_M4CID0_SCHOOL0)
summary(data_M4CID0_SCHOOL1)

# TEST
# not normal and independent (see tests above)
ggplot(data_M4CID0) + geom_boxplot(aes(y=TEST, x=factor(SCHOOL)))
median(data_M4CID0_SCHOOL0$TEST)#68
median(data_M4CID0_SCHOOL1$TEST)#54
wilcox.test(data_M4CID0_SCHOOL0$TEST,data_M4CID0_SCHOOL1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$TEST and data_M4CID0_SCHOOL1$TEST
# W = 117771, p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0

# LAB
# not normal and independent (see tests above)
ggplot(data_M4CID0) + geom_boxplot(aes(y=LAB, x=factor(SCHOOL)))
median(data_M4CID0_SCHOOL0$LAB)#62
median(data_M4CID0_SCHOOL1$LAB)#51
wilcox.test(data_M4CID0_SCHOOL0$LAB,data_M4CID0_SCHOOL1$LAB,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$LAB and data_M4CID0_SCHOOL1$LAB
# W = 112552, p-value < 2.2e-16
# alternative hypothesis: true location shift is greater than 0

# BEHAV
# not normal and independent (see tests above)
ggplot(data_M4CID0) + geom_boxplot(aes(y=BEHAV, x=factor(SCHOOL)))
median(data_M4CID0_SCHOOL0$BEHAV)#77
median(data_M4CID0_SCHOOL1$BEHAV)#80
wilcox.test(data_M4CID0_SCHOOL0$BEHAV,data_M4CID0_SCHOOL1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_M4CID0_SCHOOL0$BEHAV and data_M4CID0_SCHOOL1$BEHAV
# W = 70972, p-value = 8.34e-05
# alternative hypothesis: true location shift is less than 0

# CLASS
# not normal and independent (see tests above)
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

## all sample with school effect: 2003-2017
# after the computadtion , are they from the same distrbution?
# calculus
summary(data)
data_SCHOOL0<-filter(data,SCHOOL=="0")  %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
data_computed_M4CID0<-filter(data,GRADE<="2",SCHOOL=="1",M4CID=="0")  %>% select(ID, DATE, SCHOOL, GENDER, TEST,LAB, BEHAV, GRADE, CLASS, M4CID)
dif_mean_TEST <- mean(data_SCHOOL0$TEST)-mean(data_computed_M4CID0$TEST)
dif_median_TEST <- median(data_SCHOOL0$TEST)-median(data_computed_M4CID0$TEST)
dif_mean_TEST
dif_median_TEST
dif_mean_LAB <- mean(data_SCHOOL0$LAB)-mean(data_computed_M4CID0$LAB)
dif_median_LAB <- median(data_SCHOOL0$LAB)-median(data_computed_M4CID0$LAB)
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

# table ??? : computed school differences for the junior sample school 0 and 1 without 4C/ID methodology

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
data_computed$CLASS<-ifelse(data_computed$SCHOOL=="1",data_computed$CLASS+dif_mean_CLASS,data_computed$CLASS)
data_computed$BEHAV<-ifelse(data_computed$SCHOOL=="1",data_computed$BEHAV+dif_mean_BEHAV,data_computed$BEHAV)

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
# z = -0.85793, p-value = 0.3909
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.02599663 
# sample independent
ggplot(data_computed) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_M4CID0$TEST)#67
median(data_computed_M4CID1$TEST)#69
wilcox.test(data_computed_M4CID0$TEST,data_computed_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_M4CID0$TEST and data_computed_M4CID1$TEST
# W = 223254, p-value = 0.005261
# alternative hypothesis: true location shift is less than 0

#LAB
summary(data_computed)
hist(data_computed$LAB)
qqnorm(data_computed$LAB)
qqline(data_computed$LAB)
ks.test(data_computed$LAB,dnorm(mean(data_computed$LAB),sd(data_computed$LAB)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$TEST and dnorm(mean(data_computed$TEST), sd(data_computed$TEST))
# D = 1, p-value = 0.0007067
# alternative hypothesis: two-sided
shapiro.test(data_computed$LAB)
# Shapiro-Wilk normality test
# data:  data_computed$LAB
# W = 0.98723, p-value = 8.131e-10
#conclusion: not normal
sample1<-data.frame(sample(data_computed_M4CID0$LAB,500))
sample2<-data.frame(sample(data_computed_M4CID1$LAB,500))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_computed_M4CID0.LAB..500.,sample2$sample.data_computed_M4CID1.LAB..500.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_M4CID0.LAB..500. and sample2$sample.data_computed_M4CID1.LAB..500.
# z = 0.12712, p-value = 0.8988
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.003875564
# sample independent
ggplot(data_computed) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_M4CID0$LAB)#63
median(data_computed_M4CID1$LAB)#78
wilcox.test(data_computed_M4CID0$LAB,data_computed_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_M4CID0$LAB and data_computed_M4CID1$LAB
# W = 140102, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

#BEHAV
summary(data_computed)
hist(data_computed$BEHAV)
qqnorm(data_computed$BEHAV)
qqline(data_computed$BEHAV)
ks.test(data_computed$BEHAV,dnorm(mean(data_computed$BEHAV),sd(data_computed$BEHAV)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$TEST and dnorm(mean(data_computed$TEST), sd(data_computed$TEST))
# D = 1, p-value = 0.0007067
# alternative hypothesis: two-sided
shapiro.test(data_computed$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed$BEHAV
# W = 0.97601, p-value = 1.333e-14
#conclusion: not normal
sample1<-data.frame(sample(data_computed_M4CID0$BEHAV,500))
sample2<-data.frame(sample(data_computed_M4CID1$BEHAV,500))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_computed_M4CID0.BEHAV..500.,sample2$sample.data_computed_M4CID1.BEHAV..500.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_M4CID0.BEHAV..500. and sample2$sample.data_computed_M4CID1.BEHAV..500.
# z = 0.3849, p-value = 0.7003
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01169047  
# sample independent
ggplot(data_computed) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_M4CID0$BEHAV)#77
median(data_computed_M4CID1$BEHAV)#73
wilcox.test(data_computed_M4CID0$BEHAV,data_computed_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_M4CID0$BEHAV and data_computed_M4CID1$BEHAV
# W = 287377, p-value = 1.613e-09
# alternative hypothesis: true location shift is greater than 0

#CLASS
summary(data_computed)
hist(data_computed$CLASS)
qqnorm(data_computed$CLASS)
qqline(data_computed$CLASS)
ks.test(data_computed$CLASS,dnorm(mean(data_computed$CLASS),sd(data_computed$CLASS)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$CLASS and dnorm(mean(data_computed$CLASS), sd(data_computed$CLASS))
# D = 1, p-value = 0.001413
# alternative hypothesis: two-sided
shapiro.test(data_computed$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed$CLASS
# W = 0.99646, p-value = 0.002478
#conclusion: not normal
sample1<-data.frame(sample(data_computed_M4CID0$CLASS,500))
sample2<-data.frame(sample(data_computed_M4CID1$CLASS,500))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_computed_M4CID0.CLASS..500.,sample2$sample.data_computed_M4CID1.CLASS..500.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_M4CID0.CLASS..500. and sample2$sample.data_computed_M4CID1.CLASS..500.
# z = 0.37135, p-value = 0.7104
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01112458
# sample independent
ggplot(data_computed) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_M4CID0$CLASS)#68
median(data_computed_M4CID1$CLASS)#71
wilcox.test(data_computed_M4CID0$CLASS,data_computed_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_M4CID0$CLASS and data_computed_M4CID1$CLASS
# W = 199597, p-value = 6.514e-09
# alternative hypothesis: true location shift is less than 0

# conclusion
# TEST, LAB, CLASS positive effect;´
# BEHAV negative;

# Inference junior sample with school effect
data_junior_computed<-filter(data,GRADE <= "2") %>% select(ID,GENDER,BEHAV,M4CID,SCHOOL,GRADE,TEST,LAB, CLASS)

data_junior_computed$TEST<-ifelse(data_junior_computed$SCHOOL=="1",data_junior_computed$TEST+dif_mean_TEST,data_junior_computed$TEST)
data_junior_computed$LAB<-ifelse(data_junior_computed$SCHOOL=="1",data_junior_computed$LAB+dif_mean_LAB,data_junior_computed$LAB)
data_junior_computed$CLASS<-ifelse(data_junior_computed$SCHOOL=="1",data_junior_computed$CLASS+dif_mean_CLASS,data_junior_computed$CLASS)
data_junior_computed$BEHAV<-ifelse(data_junior_computed$SCHOOL=="1",data_junior_computed$BEHAV+dif_mean_BEHAV,data_junior_computed$BEHAV)

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

# Inference for computed junior treatment effect with school effect
# conclusion
# negative on BEHAV
# Positive effect for LAB, TEST and CLASS

# Inference for treatment effect all sample, including School 1 just with 4C/ID
# use the calculus about the computed differences already made 
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
ggplot(data_computed) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
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

# GRADE 7 (School effect)
data_computed_7<-filter(data,GRADE=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

# values added to variables only for school 1
data_computed_7$TEST<-ifelse(data_computed_7$SCHOOL==1,data_computed_7$TEST+dif_mean_TEST,data_computed_7$TEST)
data_computed_7$LAB<-ifelse(data_computed_7$SCHOOL==1,data_computed_7$LAB+dif_mean_LAB,data_computed_7$LAB)
data_computed_7$CLASS<-ifelse(data_computed_7$SCHOOL==1,data_computed_7$CLASS+dif_mean_CLASS,data_computed_7$CLASS)
data_computed_7$BEHAV<-ifelse(data_computed_7$SCHOOL==1,data_computed_7$BEHAV+dif_mean_BEHAV,data_computed_7$BEHAV)

data_computed_7_M4CID0<-filter(data_computed_7,M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_7_M4CID1<-filter(data_computed_7,M4CID == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_computed_7)

length(data_computed_7_M4CID0$ID)#253
length(data_computed_7_M4CID1$ID)#99

#TEST
hist(data_computed_7$TEST)
qqnorm(data_computed_7$TEST)
qqline(data_computed_7$TEST)
ks.test(data_computed_7$TEST,dnorm(mean(data_computed_7$TEST),sd(data_computed_7$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_7$TEST and dnorm(mean(data_computed_7$TEST), sd(data_computed_7$TEST))
# D = 1, p-value = 0.005666
# alternative hypothesis: two-sided
shapiro.test(data_computed_7$TEST)
# Shapiro-Wilk normality test
# data:  data_computed_7$TEST
# W = 0.99283, p-value = 0.09028
#conclusion: normal (shapiro)
ggplot(data_computed_7) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_7_M4CID0$TEST)#66
median(data_computed_7_M4CID1$TEST)#70
wilcox.test(data_computed_7_M4CID0$TEST,data_computed_7_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_7_M4CID0$TEST and data_computed_7_M4CID1$TEST
# W = 11256, p-value = 0.07002
# alternative hypothesis: true location shift is less than 0

#LAB
hist(data_computed_7$LAB)
qqnorm(data_computed_7$LAB)
qqline(data_computed_7$LAB)
ks.test(data_computed_7$LAB,dnorm(mean(data_computed_7$LAB),sd(data_computed_7$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_7$LAB and dnorm(mean(data_computed_7$LAB), sd(data_computed_7$LAB))
# D = 1, p-value = 0.005666
# alternative hypothesis: two-sided
shapiro.test(data_computed_7$LAB)
# Shapiro-Wilk normality test
# data:  data_computed_7$LAB
# W = 0.98217, p-value = 0.0002387
#conclusion: not normal
ggplot(data_computed_7) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_7_M4CID0$LAB)#63
median(data_computed_7_M4CID1$LAB)#78
wilcox.test(data_computed_7_M4CID0$LAB,data_computed_7_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_7_M4CID0$LAB and data_computed_7_M4CID1$LAB
# W = 7002, p-value = 6.273e-11
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_computed_7$BEHAV)
qqnorm(data_computed_7$BEHAV)
qqline(data_computed_7$BEHAV)
ks.test(data_computed_7$BEHAV,dnorm(mean(data_computed_7$BEHAV),sd(data_computed_7$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_7$BEHAV and dnorm(mean(data_computed_7$BEHAV), sd(data_computed_7$BEHAV))
# D = 1, p-value = 0.002833
# alternative hypothesis: two-sided
shapiro.test(data_computed_7$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed_7$BEHAV
# W = 0.979, p-value = 5.218e-05
#conclusion: not normal
ggplot(data_computed_7) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_7_M4CID0$BEHAV)#76
median(data_computed_7_M4CID1$BEHAV)#70
wilcox.test(data_computed_7_M4CID0$BEHAV,data_computed_7_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_7_M4CID0$BEHAV and data_computed_7_M4CID1$BEHAV
# W = 15329, p-value = 0.0005411
# alternative hypothesis: true location shift is greater than 0

# CLASS
hist(data_computed_7$CLASS)
qqnorm(data_computed_7$CLASS)
qqline(data_computed_7$CLASS)
ks.test(data_computed_7$CLASS,dnorm(mean(data_computed_7$CLASS),sd(data_computed_all$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_7$CLASS and dnorm(mean(data_computed_7$CLASS), sd(data_computed_all$CLASS))
# D = 1, p-value = 0.005666
# alternative hypothesis: two-sided
shapiro.test(data_computed_7$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed_7$CLASS
# W = 0.99573, p-value = 0.456
#conclusion: normal
ggplot(data_computed_7) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_7_M4CID0$CLASS)#69
median(data_computed_7_M4CID1$CLASS)#73
wilcox.test(data_computed_7_M4CID0$CLASS,data_computed_7_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_7_M4CID0$CLASS and data_computed_7_M4CID1$CLASS
# W = 10010, p-value = 0.001708
# alternative hypothesis: true location shift is less than 0

# conclusion: all sample, grade 7 (school effect)
# no effect for TEST
# BEHAV negative
# LAB, CLASS positive

# GRADE 8 (School effect)
data_computed_8<-filter(data,GRADE=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

# values added to variables only for school 1
data_computed_8$TEST<-ifelse(data_computed_8$SCHOOL==1,data_computed_8$TEST+dif_mean_TEST,data_computed_8$TEST)
data_computed_8$LAB<-ifelse(data_computed_8$SCHOOL==1,data_computed_8$LAB+dif_mean_LAB,data_computed_8$LAB)
data_computed_8$CLASS<-ifelse(data_computed_8$SCHOOL==1,data_computed_8$CLASS+dif_mean_CLASS,data_computed_8$CLASS)
data_computed_8$BEHAV<-ifelse(data_computed_8$SCHOOL==1,data_computed_8$BEHAV+dif_mean_BEHAV,data_computed_8$BEHAV)

data_computed_8_M4CID0<-filter(data_computed_8,M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_8_M4CID1<-filter(data_computed_8,M4CID == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_computed_8)

length(data_computed_8_M4CID0$ID)#264
length(data_computed_8_M4CID1$ID)#147

#TEST
hist(data_computed_8$TEST)
qqnorm(data_computed_8$TEST)
qqline(data_computed_8$TEST)
ks.test(data_computed_8$TEST,dnorm(mean(data_computed_8$TEST),sd(data_computed_8$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_8$TEST and dnorm(mean(data_computed_8$TEST), sd(data_computed_8$TEST))
# D = 1, p-value = 0.002427
# alternative hypothesis: two-sided
shapiro.test(data_computed_8$TEST)
# Shapiro-Wilk normality test
# data:  data_computed_8$TEST
# W = 0.99176, p-value = 0.02226
#conclusion: normal (shapiro)
ggplot(data_computed_8) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_8_M4CID0$TEST)#66
median(data_computed_8_M4CID1$TEST)#70
wilcox.test(data_computed_8_M4CID0$TEST,data_computed_8_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_8_M4CID0$TEST and data_computed_8_M4CID1$TEST
# W = 15210, p-value = 0.0001402
# alternative hypothesis: true location shift is less than 0

#LAB
hist(data_computed_8$LAB)
qqnorm(data_computed_8$LAB)
qqline(data_computed_8$LAB)
ks.test(data_computed_8$LAB,dnorm(mean(data_computed_8$LAB),sd(data_computed_8$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_8$LAB and dnorm(mean(data_computed_8$LAB), sd(data_computed_8$LAB))
# D = 1, p-value = 0.004854
# alternative hypothesis: two-sided
shapiro.test(data_computed_8$LAB)
# Shapiro-Wilk normality test
# data:  data_computed_8$LAB
# W = 0.98437, p-value = 0.0002044
#conclusion: not normal
ggplot(data_computed_8) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_8_M4CID0$LAB)#60
median(data_computed_8_M4CID1$LAB)#75
wilcox.test(data_computed_8_M4CID0$LAB,data_computed_8_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_8_M4CID0$LAB and data_computed_8_M4CID1$LAB
# W = 10940, p-value = 6.273e-11
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_computed_8$BEHAV)
qqnorm(data_computed_8$BEHAV)
qqline(data_computed_8$BEHAV)
ks.test(data_computed_8$BEHAV,dnorm(mean(data_computed_8$BEHAV),sd(data_computed_8$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_8$BEHAV and dnorm(mean(data_computed_8$BEHAV), sd(data_computed_8$BEHAV))
# D = 1, p-value = 0.002427
# alternative hypothesis: two-sided
shapiro.test(data_computed_8$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed_8$BEHAV
# W = 0.965, p-value = 2.445e-08
#conclusion: not normal
ggplot(data_computed_8) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_8_M4CID0$BEHAV)#80
median(data_computed_8_M4CID1$BEHAV)#69
wilcox.test(data_computed_8_M4CID0$BEHAV,data_computed_8_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_8_M4CID0$BEHAV and data_computed_8_M4CID1$BEHAV
# W = 26533, p-value = 3.277e-10
# alternative hypothesis: true location shift is greater than 0

# CLASS
hist(data_computed_8$CLASS)
qqnorm(data_computed_8$CLASS)
qqline(data_computed_8$CLASS)
ks.test(data_computed_8$CLASS,dnorm(mean(data_computed_8$CLASS),sd(data_computed_all$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_8$CLASS and dnorm(mean(data_computed_8$CLASS), sd(data_computed_all$CLASS))
# D = 1, p-value = 0.004854
# alternative hypothesis: two-sided
shapiro.test(data_computed_8$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed_8$CLASS
# W = 0.99245, p-value = 0.03579
#conclusion: normal
ggplot(data_computed_8) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_8_M4CID0$CLASS)#67
median(data_computed_8_M4CID1$CLASS)#70
wilcox.test(data_computed_8_M4CID0$CLASS,data_computed_8_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_8_M4CID0$CLASS and data_computed_8_M4CID1$CLASS
# W = 15496, p-value = 0.0003562
# alternative hypothesis: true location shift is less than 0

# Conclusion
# positive impact for TEST, LAB and CLASS, negative for BEHAV

# GRADE 9 (School effect)
data_computed_9<-filter(data,GRADE=="2") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

# values added to variables only for school 1
data_computed_9$TEST<-ifelse(data_computed_9$SCHOOL==1,data_computed_9$TEST+dif_mean_TEST,data_computed_9$TEST)
data_computed_9$LAB<-ifelse(data_computed_9$SCHOOL==1,data_computed_9$LAB+dif_mean_LAB,data_computed_9$LAB)
data_computed_9$CLASS<-ifelse(data_computed_9$SCHOOL==1,data_computed_9$CLASS+dif_mean_CLASS,data_computed_9$CLASS)
data_computed_9$BEHAV<-ifelse(data_computed_9$SCHOOL==1,data_computed_9$BEHAV+dif_mean_BEHAV,data_computed_9$BEHAV)

data_computed_9_M4CID0<-filter(data_computed_9,M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_9_M4CID1<-filter(data_computed_9,M4CID == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
summary(data_computed_9)

length(data_computed_9_M4CID0$ID)#277
length(data_computed_9_M4CID1$ID)#119

#TEST
hist(data_computed_9$TEST)
qqnorm(data_computed_9$TEST)
qqline(data_computed_9$TEST)
ks.test(data_computed_9$TEST,dnorm(mean(data_computed_9$TEST),sd(data_computed_9$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_9$TEST and dnorm(mean(data_computed_9$TEST), sd(data_computed_9$TEST))
# D = 1, p-value = 0.005038
# alternative hypothesis: two-sided
shapiro.test(data_computed_9$TEST)
# Shapiro-Wilk normality test
# data:  data_computed_9$TEST
# W = 0.98776, p-value = 0.002072
#conclusion: normal (shapiro)
ggplot(data_computed_9) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_9_M4CID0$TEST)#71
median(data_computed_9_M4CID1$TEST)#70
wilcox.test(data_computed_9_M4CID0$TEST,data_computed_9_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_9_M4CID0$TEST and data_computed_9_M4CID1$TEST
# W = 15458, p-value = 0.8367
# alternative hypothesis: true location shift is less than 0

#LAB
hist(data_computed_9$LAB)
qqnorm(data_computed_9$LAB)
qqline(data_computed_9$LAB)
ks.test(data_computed_9$LAB,dnorm(mean(data_computed_9$LAB),sd(data_computed_9$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_9$LAB and dnorm(mean(data_computed_9$LAB), sd(data_computed_9$LAB))
# D = 1, p-value = 0.002519
# alternative hypothesis: two-sided
shapiro.test(data_computed_9$LAB)
# Shapiro-Wilk normality test
# data:  data_computed_9$LAB
# W = 0.98053, p-value = 3.572e-05
#conclusion: not normal
ggplot(data_computed_9) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_9_M4CID0$LAB)#60
median(data_computed_9_M4CID1$LAB)#75
wilcox.test(data_computed_9_M4CID0$LAB,data_computed_9_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_9_M4CID0$LAB and data_computed_9_M4CID1$LAB
# W = 7351.5, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
hist(data_computed_9$BEHAV)
qqnorm(data_computed_9$BEHAV)
qqline(data_computed_9$BEHAV)
ks.test(data_computed_9$BEHAV,dnorm(mean(data_computed_9$BEHAV),sd(data_computed_9$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_9$BEHAV and dnorm(mean(data_computed_9$BEHAV), sd(data_computed_9$BEHAV))
# D = 1, p-value = 0.002519
# alternative hypothesis: two-sided
shapiro.test(data_computed_9$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed_9$BEHAV
# W = 0.96926, p-value = 2.118e-07
#conclusion: not normal
ggplot(data_computed_9) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_9_M4CID0$BEHAV)#76
median(data_computed_9_M4CID1$BEHAV)#82
wilcox.test(data_computed_9_M4CID0$BEHAV,data_computed_9_M4CID1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_9_M4CID0$BEHAV and data_computed_9_M4CID1$BEHAV
# W = 13648, p-value = 0.003335
# alternative hypothesis: true location shift is greater than 0

# CLASS
hist(data_computed_9$CLASS)
qqnorm(data_computed_9$CLASS)
qqline(data_computed_9$CLASS)
ks.test(data_computed_9$CLASS,dnorm(mean(data_computed_9$CLASS),sd(data_computed_all$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_9$CLASS and dnorm(mean(data_computed_9$CLASS), sd(data_computed_all$CLASS))
# D = 1, p-value = 0.005038
# alternative hypothesis: two-sided
shapiro.test(data_computed_9$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed_9$CLASS
# W = 0.99584, p-value = 0.3823
#conclusion: normal
ggplot(data_computed_9) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_9_M4CID0$CLASS)#68
median(data_computed_9_M4CID1$CLASS)#73
wilcox.test(data_computed_9_M4CID0$CLASS,data_computed_9_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_9_M4CID0$CLASS and data_computed_9_M4CID1$CLASS
# W = 11778, p-value = 3.348e-06
# alternative hypothesis: true location shift is less than 0

# Conclusion
# no impact for TEST, positive for LAB, CLASS, and BEHAV

########## RANK
data_computed <- data
data_computed$TEST<-ifelse(data_computed$SCHOOL=="1",data_computed$TEST+dif_mean_TEST,data_computed$TEST)
data_computed$LAB<-ifelse(data_computed$SCHOOL=="1",data_computed$LAB+dif_mean_LAB,data_computed$LAB)
data_computed$CLASS<-ifelse(data_computed$SCHOOL=="1",data_computed$CLASS+dif_mean_CLASS,data_computed$CLASS)
data_computed$BEHAV<-ifelse(data_computed$SCHOOL=="1",data_computed$BEHAV+dif_mean_BEHAV,data_computed$BEHAV)

summary(data_computed)
head(data_computed)
data_computed$RANK <-
  ifelse(data_computed$CLASS <=45, 1,
         ifelse((data_computed$CLASS >45 & data_computed$CLASS<70), 2,
                ifelse(data_computed$CLASS >=70, 3,NA)))
summary(data_computed)
length(data_computed$ID)#1414 entrances
length(which(data_computed$RANK=="1"))#45
length(which(data_computed$RANK=="1"))/length(data_computed$ID)#3%
length(which(data_computed$RANK=="2"))#711
length(which(data_computed$RANK=="2"))/length(data_computed$ID)#50%
length(which(data_computed$RANK=="3"))#658
length(which(data_computed$RANK=="3"))/length(data_computed$ID)#27%

#RANK 1, 45, 3%
#RANK 2, 711, 50%
#RANK 3, 658, 47% 

#RANK 1
#This analysis uses all sample with school effect for RANK 1
data_computed_RANK1<-filter(data_computed,RANK == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS, RANK)
data_computed_RANK1_M4CID0 <- filter(data_computed_RANK1, M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_computed_RANK1_M4CID1 <- filter(data_computed_RANK1, M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_computed_RANK1_M4CID0)
summary(data_computed_RANK1_M4CID1)
length(data_computed_RANK1_M4CID0$ID)#31
length(data_computed_RANK1_M4CID1$ID)#14, note: very low sample

# TEST
# independency
sample1<-data.frame(sample(data_computed_RANK1_M4CID0$TEST,14))
sample2<-data.frame(sample(data_computed_RANK1_M4CID1$TEST,14))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK1_M4CID0.TEST..14.,sample2$sample.data_computed_RANK1_M4CID1.TEST..14.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK1_M4CID0.TEST..14. and sample2$sample.data_computed_RANK1_M4CID1.TEST..14.
# z = -0.60581, p-value = 0.5446
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1236033 
# independent!

hist(data_computed_RANK1$TEST)
qqnorm(data_computed_RANK1$TEST)
qqline(data_computed_RANK1$TEST)
ks.test(data_computed_RANK1$TEST,dnorm(mean(data_computed_RANK1$TEST),sd(data_computed_RANK1$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK1$TEST and dnorm(mean(data_computed_RANK1$TEST), sd(data_computed_RANK1$TEST))
# D = 1, p-value = 0.04348
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK1$TEST)
# Shapiro-Wilk normality test
# data:  data_computed_RANK1$TEST
# W = 0.97169, p-value = 0.333
#conclusion: normal
ggplot(data_computed_RANK1) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_RANK1_M4CID0$TEST)#43
median(data_computed_RANK1_M4CID1$TEST)#37
wilcox.test(data_computed_RANK1_M4CID0$TEST,data_computed_RANK1_M4CID1$TEST,alternative = "greater")
wilcox.test(sample1$sample.data_computed_RANK1_M4CID0.TEST..14.,sample2$sample.data_computed_RANK1_M4CID1.TEST..14.,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK1_M4CID0$TEST and data_computed_RANK1_M4CID1$TEST
# W = 266, p-value = 0.1171
# alternative hypothesis: true location shift is greater than 0
# Warning message:
#   In wilcox.test.default(data_computed_RANK1_M4CID0$TEST, data_computed_RANK1_M4CID1$TEST,  :
#                            cannot compute exact p-value with ties
# t-test? Permutation test?
?t.test
t.test(data_computed_RANK1_M4CID0$TEST,data_computed_RANK1_M4CID1$TEST,alternative = "greater")
# Welch Two Sample t-test
# data:  data_computed_RANK1_M4CID0$TEST and data_computed_RANK1_M4CID1$TEST
# t = 1.6193, df = 18.222, p-value = 0.06128
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:-0.4536453        Inf
# sample estimates:   mean of x mean of y: 41.41793  34.94992 
t.test(sample1$sample.data_computed_RANK1_M4CID0.TEST..14.,sample2$sample.data_computed_RANK1_M4CID1.TEST..14.,alternative = "greater")
# Welch Two Sample t-test
# data:  sample1$sample.data_computed_RANK1_M4CID0.TEST..14. and sample2$sample.data_computed_RANK1_M4CID1.TEST..14.
# t = 1.847, df = 23.255, p-value = 0.03876
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval: 0.5972487       Inf
# sample estimates: mean of x mean of y, 43.18872  34.94992 
library(jmuOutlier)
perm.test(data_computed_RANK1_M4CID0$TEST,data_computed_RANK1_M4CID1$TEST,alternative = "greater")
# "Unpaired two-sample permutation test was performed."
# "p-value was estimated based on 20000 simulations."
# $alternative [1] "greater"
# $mu [1] 0
# $p.value [1] 0.03425
perm.test(sample1$sample.data_computed_RANK1_M4CID0.TEST..14.,sample2$sample.data_computed_RANK1_M4CID1.TEST..14.,alternative = "greater")
# [[2]][1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "greater"
# $mu [1] 0
# $p.value [1] 0.11625

# LAB
# independency
sample1<-data.frame(sample(data_computed_RANK1_M4CID0$LAB,14))
sample2<-data.frame(sample(data_computed_RANK1_M4CID1$LAB,14))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK1_M4CID0.LAB..14.,sample2$sample.data_computed_RANK1_M4CID1.LAB..14.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK1_M4CID0.LAB..14. and sample2$sample.data_computed_RANK1_M4CID1.LAB..14.
# z = -0.89492, p-value = 0.3708
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.1871633 
# independent!
hist(data_computed_RANK1$LAB)
qqnorm(data_computed_RANK1$LAB)
qqline(data_computed_RANK1$LAB)
ks.test(data_computed_RANK1$LAB,dnorm(mean(data_computed_RANK1$LAB),sd(data_computed_RANK1$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK1$LAB and dnorm(mean(data_computed_RANK1$LAB), sd(data_computed_RANK1$LAB))
# D = 1, p-value = 0.04348
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK1$LAB)
# Shapiro-Wilk normality test
# data:  data_computed_RANK1$LAB
# W = 0.91501, p-value = 0.002876
#conclusion: not normal
ggplot(data_computed_RANK1) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_RANK1_M4CID0$LAB)#30
median(data_computed_RANK1_M4CID1$LAB)#45
wilcox.test(data_computed_RANK1_M4CID0$LAB,data_computed_RANK1_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK1_M4CID0$LAB and data_computed_RANK1_M4CID1$LAB
# W = 95.5, p-value = 0.001484
# alternative hypothesis: true location shift is less than 0
# Warning message:
#   In wilcox.test.default(data_computed_RANK1_M4CID0$LAB, data_computed_RANK1_M4CID1$LAB,  :
#                            cannot compute exact p-value with ties
# t-test? Permutation test?
wilcox.test(sample1$sample.data_computed_RANK1_M4CID0.LAB..14.,sample2$sample.data_computed_RANK1_M4CID1.LAB..14.,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  sample1$sample.data_computed_RANK1_M4CID0.LAB..14. and sample2$sample.data_computed_RANK1_M4CID1.LAB..14.
# W = 44.5, p-value = 0.007346
# alternative hypothesis: true location shift is less than 0
# Warning message:
#   In wilcox.test.default(sample1$sample.data_computed_RANK1_M4CID0.LAB..14.,  :
#                            cannot compute exact p-value with ties
library(jmuOutlier)
perm.test(data_computed_RANK1_M4CID0$LAB,data_computed_RANK1_M4CID1$LAB,alternative = "less")
# "Unpaired two-sample permutation test was performed."
# [[2]]
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "less"
# $mu[1] 0 $p.value [1] 0.00025
perm.test(sample1$sample.data_computed_RANK1_M4CID0.LAB..14.,sample2$sample.data_computed_RANK1_M4CID1.LAB..14.,alternative = "less")
# [[1]] [1] "Unpaired two-sample permutation test was performed."
# [[2]] [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "greater"
# $mu [1] 0 $p.value [1] 0.0013

# BEHAV
# independency
sample1<-data.frame(sample(data_computed_RANK1_M4CID0$BEHAV,14))
sample2<-data.frame(sample(data_computed_RANK1_M4CID1$BEHAV,14))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK1_M4CID0.BEHAV..14.,sample2$sample.data_computed_RANK1_M4CID1.BEHAV..14.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK1_M4CID0.BEHAV..14. and sample2$sample.data_computed_RANK1_M4CID1.BEHAV..14.
# z = 0.98984, p-value = 0.3223
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.2011205 
# independent!
hist(data_computed_RANK1$BEHAV)
qqnorm(data_computed_RANK1$BEHAV)
qqline(data_computed_RANK1$BEHAV)
ks.test(data_computed_RANK1$BEHAV,dnorm(mean(data_computed_RANK1$BEHAV),sd(data_computed_RANK1$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK1$BEHAV and dnorm(mean(data_computed_RANK1$BEHAV), sd(data_computed_RANK1$BEHAV))
# D = 1, p-value = 0.04348
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK1$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed_RANK1$BEHAV
# W = 0.98191, p-value = 0.6976
#conclusion: normal
ggplot(data_computed_RANK1) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_RANK1_M4CID0$BEHAV)#56
median(data_computed_RANK1_M4CID1$BEHAV)#46
wilcox.test(data_computed_RANK1_M4CID0$BEHAV,data_computed_RANK1_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK1_M4CID0$BEHAV and data_computed_RANK1_M4CID1$BEHAV
# W = 315.5, p-value = 0.008112
# alternative hypothesis: true location shift is greater than 0
# Warning message:
#   In wilcox.test.default(data_computed_RANK1_M4CID0$BEHAV, data_computed_RANK1_M4CID1$BEHAV,  :
#                            cannot compute exact p-value with ties
# t-test? Permutation test?
wilcox.test(sample1$sample.data_computed_RANK1_M4CID0.BEHAV..14.,sample2$sample.data_computed_RANK1_M4CID1.BEHAV..14.,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  sample1$sample.data_computed_RANK1_M4CID0.BEHAV..14. and sample2$sample.data_computed_RANK1_M4CID1.BEHAV..14.
# W = 122, p-value = 0.1398
# alternative hypothesis: true location shift is greater than 0
# Warning message:
#   In wilcox.test.default(sample1$sample.data_computed_RANK1_M4CID0.BEHAV..14.,  :
#                            cannot compute exact p-value with ties
library(jmuOutlier)
perm.test(data_computed_RANK1_M4CID0$BEHAV,data_computed_RANK1_M4CID1$BEHAV,alternative = "greater")
# "Unpaired two-sample permutation test was performed."
# [[2]]
# [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "greater"
# $mu[1] 0 $p.value [1] 0.0137
perm.test(sample1$sample.data_computed_RANK1_M4CID0.BEHAV..14.,sample2$sample.data_computed_RANK1_M4CID1.BEHAV..14.,alternative = "greater")
# [[1]] [1] "Unpaired two-sample permutation test was performed."
# [[2]] [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "greater"
# $mu [1] 0 $p.value [1] 0.17335

# CLASS
# independency
sample1<-data.frame(sample(data_computed_RANK1_M4CID0$CLASS,14))
sample2<-data.frame(sample(data_computed_RANK1_M4CID1$CLASS,14))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK1_M4CID0.CLASS..14.,sample2$sample.data_computed_RANK1_M4CID1.CLASS..14.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK1_M4CID0.CLASS..14. and sample2$sample.data_computed_RANK1_M4CID1.CLASS..14.
# z = 0.604, p-value = 0.5458
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau # 0.1222222 
# independent!
hist(data_computed_RANK1$CLASS)
qqnorm(data_computed_RANK1$CLASS)
qqline(data_computed_RANK1$CLASS)
ks.test(data_computed_RANK1$CLASS,dnorm(mean(data_computed_RANK1$CLASS),sd(data_computed_RANK1$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK1$CLASS and dnorm(mean(data_computed_RANK1$CLASS), sd(data_computed_RANK1$CLASS))
# D = 1, p-value = 0.04348
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK1$CLASS)
# # hapiro-Wilk normality test
# data:  data_computed_RANK1$CLASS
# W = 0.82781, p-value = 1.032e-05
#conclusion: not normal
ggplot(data_computed_RANK1) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_RANK1_M4CID0$CLASS)#42
median(data_computed_RANK1_M4CID1$CLASS)#42
wilcox.test(data_computed_RANK1_M4CID0$CLASS,data_computed_RANK1_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK1_M4CID0$CLASS and data_computed_RANK1_M4CID1$CLASS
# W = 218, p-value = 0.5147
# alternative hypothesis: true location shift is less than 0
# Warning message:
#   In wilcox.test.default(data_computed_RANK1_M4CID0$CLASS, data_computed_RANK1_M4CID1$CLASS,  :
#                            cannot compute exact p-value with ties
# t-test? Permutation test?
wilcox.test(sample1$sample.data_computed_RANK1_M4CID0.CLASS..14.,sample2$sample.data_computed_RANK1_M4CID1.CLASS..14.,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  sample1$sample.data_computed_RANK1_M4CID0.CLASS..14. and sample2$sample.data_computed_RANK1_M4CID1.CLASS..14.
# W = 89.5, p-value = 0.3565
# alternative hypothesis: true location shift is less than 0
# Warning message:
#   In wilcox.test.default(sample1$sample.data_computed_RANK1_M4CID0.CLASS..14.,  :
#                            cannot compute exact p-value with ties
library(jmuOutlier)
perm.test(data_computed_RANK1_M4CID0$CLASS,data_computed_RANK1_M4CID1$CLASS,alternative = "less")
#  [[2]] [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "less"
# $mu [1] 0 $p.value [1] 0.54905
perm.test(sample1$sample.data_computed_RANK1_M4CID0.CLASS..14.,sample2$sample.data_computed_RANK1_M4CID1.CLASS..14.,alternative = "less")
# [[1]] [1] "Unpaired two-sample permutation test was performed."
# [[2]] [1] "p-value was estimated based on 20000 simulations."
# $alternative [1] "less"
# $mu [1] 0 $p.value [1] 0.30815

#RANK 2
#This analysis uses all sample with school effect for RANK 2
data_computed_RANK2<-filter(data_computed,RANK == "2") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS, RANK)
data_computed_RANK2_M4CID0 <- filter(data_computed_RANK2, M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_computed_RANK2_M4CID1 <- filter(data_computed_RANK2, M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_computed_RANK2_M4CID0)
summary(data_computed_RANK2_M4CID1)
length(data_computed_RANK2_M4CID0$ID)#445
length(data_computed_RANK2_M4CID1$ID)#266

# TEST
# independency
sample1<-data.frame(sample(data_computed_RANK2_M4CID0$TEST,200))
sample2<-data.frame(sample(data_computed_RANK2_M4CID1$TEST,200))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK2_M4CID0.TEST..200.,sample2$sample.data_computed_RANK2_M4CID1.TEST..200.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK2_M4CID0.TEST..200. and sample2$sample.data_computed_RANK2_M4CID1.TEST..200.
# z = 0.19362, p-value = 0.8465
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau # 0.0094123 
# independent!
hist(data_computed_RANK2$TEST)
qqnorm(data_computed_RANK2$TEST)
qqline(data_computed_RANK2$TEST)
ks.test(data_computed_RANK2$TEST,dnorm(mean(data_computed_RANK2$TEST),sd(data_computed_RANK2$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK2$TEST and dnorm(mean(data_computed_RANK2$TEST), sd(data_computed_RANK2$TEST))
# D = 1, p-value = 0.002809
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK2$TEST)
# Shapiro-Wilk normality test
# data:  data_computed_RANK2$TEST
# W = 0.99755, p-value = 0.3808
#conclusion: normal
ggplot(data_computed_RANK2) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_RANK2_M4CID0$TEST)#59
median(data_computed_RANK2_M4CID1$TEST)#58
wilcox.test(data_computed_RANK2_M4CID0$TEST,data_computed_RANK2_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK2_M4CID0$TEST and data_computed_RANK2_M4CID1$TEST
# W = 62018, p-value = 0.1425
# alternative hypothesis: true location shift is greater than 0
wilcox.test(sample1$sample.data_computed_RANK2_M4CID0.TEST..200.,sample2$sample.data_computed_RANK2_M4CID1.TEST..200.,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  sample1$sample.data_computed_RANK2_M4CID0.TEST..200. and sample2$sample.data_computed_RANK2_M4CID1.TEST..200.
# W = 20582, p-value = 0.3074
# alternative hypothesis: true location shift is greater than 0

# normal distribution
t.test(data_computed_RANK2_M4CID0$TEST,data_computed_RANK2_M4CID1$TEST,alternative = "greater")
# Welch Two Sample t-test
# data:  data_computed_RANK2_M4CID0$TEST and data_computed_RANK2_M4CID1$TEST
# t = 1.0223, df = 645.98, p-value = 0.1535
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval: -0.4658913        Inf
# sample estimates: mean of x mean of y 58.88883  58.12662 
t.test(sample1$sample.data_computed_RANK2_M4CID0.TEST..200.,sample2$sample.data_computed_RANK2_M4CID1.TEST..200.,alternative = "greater")
#Welch Two Sample t-test
# data:  sample1$sample.data_computed_RANK2_M4CID0.TEST..200. and sample2$sample.data_computed_RANK2_M4CID1.TEST..200.
# t = 0.45525, df = 388.53, p-value = 0.3246
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval: -1.15722      Inf
# sample estimates:mean of x mean of y 58.31061  57.86921 

# LAB
# independency
sample1<-data.frame(sample(data_computed_RANK2_M4CID0$LAB,200))
sample2<-data.frame(sample(data_computed_RANK2_M4CID1$LAB,200))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK2_M4CID0.LAB..200.,sample2$sample.data_computed_RANK2_M4CID1.LAB..200.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK2_M4CID0.LAB..200. and sample2$sample.data_computed_RANK2_M4CID1.LAB..200.
# z = 1.3176, p-value = 0.1876
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.06403335  
# independent!
hist(data_computed_RANK2$LAB)
qqnorm(data_computed_RANK2$LAB)
qqline(data_computed_RANK2$LAB)
ks.test(data_computed_RANK2$LAB,dnorm(mean(data_computed_RANK2$LAB),sd(data_computed_RANK2$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK2$LAB and dnorm(mean(data_computed_RANK2$LAB), sd(data_computed_RANK2$LAB))
# D = 1, p-value = 0.002809
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK2$LAB)
# Shapiro-Wilk normality test
# data:  data_computed_RANK2$LAB
# W = 0.97714, p-value = 4.261e-09
#conclusion: not normal
ggplot(data_computed_RANK2) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_RANK2_M4CID0$LAB)#57
median(data_computed_RANK2_M4CID1$LAB)#66
wilcox.test(data_computed_RANK2_M4CID0$LAB,data_computed_RANK2_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK2_M4CID0$LAB and data_computed_RANK2_M4CID1$LAB
# W = 33122, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_computed_RANK2_M4CID0$BEHAV,200))
sample2<-data.frame(sample(data_computed_RANK2_M4CID1$BEHAV,200))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK2_M4CID0.BEHAV..200.,sample2$sample.data_computed_RANK2_M4CID1.BEHAV..200.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK2_M4CID0.BEHAV..200. and sample2$sample.data_computed_RANK2_M4CID1.BEHAV..200.
# z = -0.50439, p-value = 0.614
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.02437033 
# independent!
hist(data_computed_RANK2$BEHAV)
qqnorm(data_computed_RANK2$BEHAV)
qqline(data_computed_RANK2$BEHAV)
ks.test(data_computed_RANK2$BEHAV,dnorm(mean(data_computed_RANK2$BEHAV),sd(data_computed_RANK2$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK2$BEHAV and dnorm(mean(data_computed_RANK2$BEHAV), sd(data_computed_RANK2$BEHAV))
# D = 1, p-value = 0.001404
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK2$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed_RANK2$BEHAV
# W = 0.9926, p-value = 0.001326
#conclusion: not normal
ggplot(data_computed_RANK2) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_RANK2_M4CID0$BEHAV)#72
median(data_computed_RANK2_M4CID1$BEHAV)#64
wilcox.test(data_computed_RANK2_M4CID0$BEHAV,data_computed_RANK2_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK2_M4CID0$BEHAV and data_computed_RANK2_M4CID1$BEHAV
# W = 77830, p-value = 9.882e-13
# alternative hypothesis: true location shift is greater than 0

# CLASS
# independency
sample1<-data.frame(sample(data_computed_RANK2_M4CID0$CLASS,200))
sample2<-data.frame(sample(data_computed_RANK2_M4CID1$CLASS,200))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK2_M4CID0.CLASS..200.,sample2$sample.data_computed_RANK2_M4CID1.CLASS..200.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK2_M4CID0.CLASS..200. and sample2$sample.data_computed_RANK2_M4CID1.CLASS..200.
# z = -0.0021135, p-value = 0.9983
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau  -0.0001007862  
# independent!
hist(data_computed_RANK2$CLASS)
qqnorm(data_computed_RANK2$CLASS)
qqline(data_computed_RANK2$CLASS)
ks.test(data_computed_RANK2$CLASS,dnorm(mean(data_computed_RANK2$CLASS),sd(data_computed_RANK2$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK2$CLASS and dnorm(mean(data_computed_RANK2$CLASS), sd(data_computed_RANK2$CLASS))
# D = 1, p-value = 0.001404
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK2$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed_RANK2$CLASS
# W = 0.95817, p-value = 2.367e-13
#conclusion: not normal
ggplot(data_computed_RANK2) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_RANK2_M4CID0$CLASS)#61
median(data_computed_RANK2_M4CID1$CLASS)#63
wilcox.test(data_computed_RANK2_M4CID0$CLASS,data_computed_RANK2_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK2_M4CID0$CLASS and data_computed_RANK2_M4CID1$CLASS
# W = 50833, p-value = 0.0008126
# alternative hypothesis: true location shift is less than 0

#RANK 3
#This analysis uses all sample with school effect for RANK 3
data_computed_RANK3<-filter(data_computed,RANK == "3") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS, RANK)
data_computed_RANK3_M4CID0 <- filter(data_computed_RANK3, M4CID=="0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
data_computed_RANK3_M4CID1 <- filter(data_computed_RANK3, M4CID=="1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS,RANK)
summary(data_computed_RANK3_M4CID0)
summary(data_computed_RANK3_M4CID1)
length(data_computed_RANK3_M4CID0$ID)#352
length(data_computed_RANK3_M4CID1$ID)#306

# TEST
# independency
sample1<-data.frame(sample(data_computed_RANK3_M4CID0$TEST,300))
sample2<-data.frame(sample(data_computed_RANK3_M4CID1$TEST,300))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK3_M4CID0.TEST..300.,sample2$sample.data_computed_RANK3_M4CID1.TEST..300.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK3_M4CID0.TEST..300. and sample2$sample.data_computed_RANK3_M4CID1.TEST..300.
# z = -0.79532, p-value = 0.4264
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.03134321 
# independent!
hist(data_computed_RANK3$TEST)
qqnorm(data_computed_RANK3$TEST)
qqline(data_computed_RANK3$TEST)
ks.test(data_computed_RANK3$TEST,dnorm(mean(data_computed_RANK3$TEST),sd(data_computed_RANK3$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK3$TEST and dnorm(mean(data_computed_RANK3$TEST), sd(data_computed_RANK3$TEST))
# D = 1, p-value = 0.001517
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK3$TEST)
# Shapiro-Wilk normality test
# data:  data_computed_RANK3$TEST
# W = 0.98771, p-value = 2.477e-05
#conclusion: not normal
ggplot(data_computed_RANK3) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_RANK3_M4CID0$TEST)#80
median(data_computed_RANK3_M4CID1$TEST)#81
wilcox.test(data_computed_RANK3_M4CID0$TEST,data_computed_RANK3_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK3_M4CID0$TEST and data_computed_RANK3_M4CID1$TEST
# W = 49778, p-value = 0.04678
# alternative hypothesis: true location shift is less than 0
wilcox.test(sample1$sample.data_computed_RANK3_M4CID0.TEST..300.,sample2$sample.data_computed_RANK3_M4CID1.TEST..300.,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  sample1$sample.data_computed_RANK3_M4CID0.TEST..300. and sample2$sample.data_computed_RANK3_M4CID1.TEST..300.
# W = 41827, p-value = 0.06752
# alternative hypothesis: true location shift is less than 0

# normal distribution
t.test(data_computed_RANK3_M4CID0$TEST,data_computed_RANK3_M4CID1$TEST,alternative = "less")
# Welch Two Sample t-test
# data:  data_computed_RANK3_M4CID0$TEST and data_computed_RANK3_M4CID1$TEST
# t = -2.2997, df = 595.79, p-value = 0.01091
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval: -Inf -0.6062119
# sample estimates:  mean of x mean of y 80.08897  82.22630 
t.test(sample1$sample.data_computed_RANK3_M4CID0.TEST..300.,sample2$sample.data_computed_RANK3_M4CID1.TEST..300.,alternative = "greater")
#Welch Two Sample t-test
# data:  sample1$sample.data_computed_RANK3_M4CID0.TEST..300. and sample2$sample.data_computed_RANK3_M4CID1.TEST..300.
# t = -2.1066, df = 578.36, p-value = 0.9822
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval: -3.622846       Inf
# sample estimates: mean of x mean of y 80.22792  82.26088

# LAB
# independency
sample1<-data.frame(sample(data_computed_RANK3_M4CID0$LAB,300))
sample2<-data.frame(sample(data_computed_RANK3_M4CID1$LAB,300))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK3_M4CID0.LAB..300.,sample2$sample.data_computed_RANK3_M4CID1.LAB..300.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK3_M4CID0.LAB..300. and sample2$sample.data_computed_RANK3_M4CID1.LAB..300.
# z = 0.10213, p-value = 0.9187
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.004060305
# independent!
hist(data_computed_RANK3$LAB)
qqnorm(data_computed_RANK3$LAB)
qqline(data_computed_RANK3$LAB)
ks.test(data_computed_RANK3$LAB,dnorm(mean(data_computed_RANK3$LAB),sd(data_computed_RANK3$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK3$LAB and dnorm(mean(data_computed_RANK3$LAB), sd(data_computed_RANK3$LAB))
# D = 1, p-value = 0.001517
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK3$LAB)
# Shapiro-Wilk normality test
# data:  data_computed_RANK3$LAB
# W = 0.99153, p-value = 0.0008128
#conclusion: not normal
ggplot(data_computed_RANK3) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_RANK3_M4CID0$LAB)#77
median(data_computed_RANK3_M4CID1$LAB)#88
wilcox.test(data_computed_RANK3_M4CID0$LAB,data_computed_RANK3_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK3_M4CID0$LAB and data_computed_RANK3_M4CID1$LAB
# W = 27279, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# BEHAV
# independency
sample1<-data.frame(sample(data_computed_RANK3_M4CID0$BEHAV,300))
sample2<-data.frame(sample(data_computed_RANK3_M4CID1$BEHAV,300))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK3_M4CID0.BEHAV..300.,sample2$sample.data_computed_RANK3_M4CID1.BEHAV..300.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK3_M4CID0.BEHAV..300. and sample2$sample.data_computed_RANK3_M4CID1.BEHAV..300.
# z = 0.81047, p-value = 0.4177
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.03206462 
# independent!
hist(data_computed_RANK3$BEHAV)
qqnorm(data_computed_RANK3$BEHAV)
qqline(data_computed_RANK3$BEHAV)
ks.test(data_computed_RANK3$BEHAV,dnorm(mean(data_computed_RANK3$BEHAV),sd(data_computed_RANK3$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK3$BEHAV and dnorm(mean(data_computed_RANK3$BEHAV), sd(data_computed_RANK3$BEHAV))
# D = 1, p-value = 0.001517
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK3$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed_RANK3$BEHAV
# W = 0.955, p-value = 2.689e-13
#conclusion: not normal
ggplot(data_computed_RANK3) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_RANK3_M4CID0$BEHAV)#85
median(data_computed_RANK3_M4CID1$BEHAV)#80
wilcox.test(data_computed_RANK3_M4CID0$BEHAV,data_computed_RANK3_M4CID1$BEHAV,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK3_M4CID0$BEHAV and data_computed_RANK3_M4CID1$BEHAV
# W = 68407, p-value = 1.09e-09
# alternative hypothesis: true location shift is greater than 0

# CLASS
# independency
sample1<-data.frame(sample(data_computed_RANK3_M4CID0$CLASS,300))
sample2<-data.frame(sample(data_computed_RANK3_M4CID1$CLASS,300))
#Kendall correlation test
cor.test(sample1$sample.data_computed_RANK3_M4CID0.CLASS..300.,sample2$sample.data_computed_RANK3_M4CID1.CLASS..300.,method="kendall", exact = FALSE)
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_RANK3_M4CID0.CLASS..300. and sample2$sample.data_computed_RANK3_M4CID1.CLASS..300.
# z = 1.4128, p-value = 0.1577
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.05484567  
# independent!
hist(data_computed_RANK3$CLASS)
qqnorm(data_computed_RANK3$CLASS)
qqline(data_computed_RANK3$CLASS)
ks.test(data_computed_RANK3$CLASS,dnorm(mean(data_computed_RANK3$CLASS),sd(data_computed_RANK3$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_RANK3$CLASS and dnorm(mean(data_computed_RANK3$CLASS), sd(data_computed_RANK3$CLASS))
# D = 1, p-value = 0.003035
# alternative hypothesis: two-sided
shapiro.test(data_computed_RANK3$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed_RANK3$CLASS
# W = 0.9209, p-value < 2.2e-16
#conclusion: not normal
ggplot(data_computed_RANK3) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_RANK3_M4CID0$CLASS)#78
median(data_computed_RANK3_M4CID1$CLASS)#81
wilcox.test(data_computed_RANK3_M4CID0$CLASS,data_computed_RANK3_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_RANK3_M4CID0$CLASS and data_computed_RANK3_M4CID1$CLASS
# W = 43756, p-value = 1.644e-05
# alternative hypothesis: true location shift is less than 0

########## GENDER
#This analysis uses all sample with school effect for gender inference analyis
data_computed<-data

data_computed$TEST<-ifelse(data_computed$SCHOOL=="1",data_computed$TEST+dif_mean_TEST,data_computed$TEST)
data_computed$LAB<-ifelse(data_computed$SCHOOL=="1",data_computed$LAB+dif_mean_LAB,data_computed$LAB)
data_computed$CLASS<-ifelse(data_computed$SCHOOL=="1",data_computed$CLASS+dif_mean_CLASS,data_computed$CLASS)
data_computed$BEHAV<-ifelse(data_computed$SCHOOL=="1",data_computed$BEHAV+dif_mean_BEHAV,data_computed$BEHAV)
summary(data_computed)
length(data_computed$ID)#1414 entrances
length(which(data_computed$GENDER=="F"))#602
length(which(data_computed$GENDER=="F"))/length(data_computed$ID)#42%
length(which(data_computed$GENDER=="M"))#812
length(which(data_computed$GENDER=="M"))/length(data_computed$ID)#57%

#FEMALE, 602, 42%
#MALE, 812, 57%

data_computed_FEMALE<-filter(data_computed,GENDER=="F") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_FEMALE_M4CID0<-filter(data_computed,GENDER=="F" & M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_FEMALE_M4CID1<-filter(data_computed,GENDER=="F" & M4CID == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_MALE<-filter(data_computed,GENDER=="F") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_MALE_M4CID0<-filter(data_computed,GENDER=="M" & M4CID == "0") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_MALE_M4CID1<-filter(data_computed,GENDER=="M" & M4CID == "1") %>% select(ID,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

length(data_computed_FEMALE_M4CID0$ID)#410
length(data_computed_FEMALE_M4CID1$ID)#192
length(data_computed_MALE_M4CID0$ID)#418
length(data_computed_MALE_M4CID1$ID)#394

#TEST
# independency
sample1<-data.frame(sample(data_computed_FEMALE_M4CID0$TEST,150))
sample2<-data.frame(sample(data_computed_FEMALE_M4CID1$TEST,150))
#Kendall correlation test
cor.test(sample1$sample.data_computed_FEMALE_M4CID0.TEST..150.,sample2$sample.data_computed_FEMALE_M4CID1.TEST..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_FEMALE_M4CID0.TEST..150. and sample2$sample.data_computed_FEMALE_M4CID1.TEST..150.
# z = -0.06503, p-value = 0.9481
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.0036214 
sample3<-data.frame(sample(data_computed_MALE_M4CID0$TEST,150))
sample4<-data.frame(sample(data_computed_MALE_M4CID1$TEST,150))
#Kendall correlation test
cor.test(sample3$sample.data_computed_MALE_M4CID0.TEST..150.,sample4$sample.data_computed_MALE_M4CID1.TEST..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_computed_MALE_M4CID0.TEST..150. and sample4$sample.data_computed_MALE_M4CID1.TEST..150.
# z = -0.048786, p-value = 0.9611
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.002725184 
# samples are independent
# Normality
hist(data_computed_FEMALE$TEST)
qqnorm(data_computed_FEMALE$TEST)
qqline(data_computed_FEMALE$TEST)
ks.test(data_computed_FEMALE$TEST,dnorm(mean(data_computed_FEMALE$TEST),sd(data_computed_FEMALE$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_FEMALE$TEST and dnorm(mean(data_computed_FEMALE$TEST), sd(data_computed_FEMALE$TEST))
# D = 1, p-value = 0.003317
# alternative hypothesis: two-sided
shapiro.test(data_computed_FEMALE$TEST)
# Shapiro-Wilk normality test
# data:  data_computed_FEMALE$TEST
# W = 0.99166, p-value = 0.00182
hist(data_computed_MALE$TEST)
qqnorm(data_computed_MALE$TEST)
qqline(data_computed_MALE$TEST)
ks.test(data_computed_MALE$TEST,dnorm(mean(data_computed_MALE$TEST),sd(data_computed_MALE$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_MALE$TEST and dnorm(mean(data_computed_MALE$TEST), sd(data_computed_MALE$TEST))
# D = 1, p-value = 0.003317
# alternative hypothesis: two-sided
shapiro.test(data_computed_MALE$TEST)
# Shapiro-Wilk normality test
# data:  data_computed_FEMALE$TEST
# W = 0.99166, p-value = 0.00182
# not normal

ggplot(data_computed) + geom_boxplot(aes(x=factor(M4CID),y=TEST, linetype = factor(GENDER)))
median(data_computed_FEMALE_M4CID0$TEST)#67
median(data_computed_FEMALE_M4CID1$TEST)#72
median(data_computed_MALE_M4CID0$TEST)#67
median(data_computed_MALE_M4CID1$TEST)#67
#It seems that the female benefit, less impact on male
wilcox.test(data_computed_FEMALE_M4CID0$TEST,data_computed_FEMALE_M4CID1$TEST, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_FEMALE_M4CID0$TEST and data_computed_FEMALE_M4CID1$TEST
# W = 32801, p-value = 0.0004873
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_computed_MALE_M4CID0$TEST,data_computed_MALE_M4CID1$TEST, alternative = "two.sided")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_MALE_M4CID0$TEST and data_computed_MALE_M4CID1$TEST
# W = 79087, p-value = 0.3293
# alternative hypothesis: true location shift is less than 0

#LAB
sample1<-data.frame(sample(data_computed_FEMALE_M4CID0$LAB,150))
sample2<-data.frame(sample(data_computed_FEMALE_M4CID1$LAB,150))
#Kendall correlation test
cor.test(sample1$sample.data_computed_FEMALE_M4CID0.LAB..150.,sample2$sample.data_computed_FEMALE_M4CID1.LAB..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_FEMALE_M4CID0.LAB..150. and sample2$sample.data_computed_FEMALE_M4CID1.LAB..150.
# z = -0.54493, p-value = 0.5858
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.03053227 
sample3<-data.frame(sample(data_computed_MALE_M4CID0$LAB,150))
sample4<-data.frame(sample(data_computed_MALE_M4CID1$LAB,150))
#Kendall correlation test
cor.test(sample3$sample.data_computed_MALE_M4CID0.LAB..150.,sample4$sample.data_computed_MALE_M4CID1.LAB..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_computed_MALE_M4CID0.LAB..150. and sample4$sample.data_computed_MALE_M4CID1.LAB..150.
# z = -0.52251, p-value = 0.6013
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.02937151  
# samples are independent

# Normality
hist(data_computed_FEMALE$LAB)
qqnorm(data_computed_FEMALE$LAB)
qqline(data_computed_FEMALE$LAB)
ks.test(data_computed_FEMALE$LAB,dnorm(mean(data_computed_FEMALE$LAB),sd(data_computed_FEMALE$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_FEMALE$LAB and dnorm(mean(data_computed_FEMALE$LAB), sd(data_computed_FEMALE$LAB))
# D = 1, p-value = 0.003317
# alternative hypothesis: two-sided
shapiro.test(data_computed_FEMALE$LAB)
# Shapiro-Wilk normality test
# data:  data_computed_FEMALE$LAB
# W = 0.98513, p-value = 8.234e-06
hist(data_computed_MALE$LAB)
qqnorm(data_computed_MALE$LAB)
qqline(data_computed_MALE$LAB)
ks.test(data_computed_MALE$LAB,dnorm(mean(data_computed_MALE$LAB),sd(data_computed_MALE$LAB)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_MALE$LAB and dnorm(mean(data_computed_MALE$LAB), sd(data_computed_MALE$LAB))
# D = 1, p-value = 0.003317
# alternative hypothesis: two-sided
shapiro.test(data_computed_MALE$LAB)
# Shapiro-Wilk normality test
# data:  data_computed_FEMALE$LAB
# W = 0.98513, p-value = 8.234e-06
# not normal

median(data_computed_FEMALE_M4CID0$LAB)#63
median(data_computed_FEMALE_M4CID1$LAB)#83
median(data_computed_MALE_M4CID0$LAB)#62
median(data_computed_MALE_M4CID1$LAB)#76
#It seems that both benefit
ggplot(data_computed) + geom_boxplot(aes(x=factor(M4CID),y=LAB, linetype = factor(GENDER)))
#It seems that both male/female benefit
wilcox.test(data_computed_FEMALE_M4CID0$LAB,data_computed_FEMALE_M4CID1$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_FEMALE_M4CID0$LAB and data_computed_FEMALE_M4CID1$LAB
# W = 17308, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0
wilcox.test(data_computed_MALE_M4CID0$LAB,data_computed_MALE_M4CID1$LAB, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_MALE_M4CID0$LAB and data_computed_MALE_M4CID1$LAB
# W = 52084, p-value = 1.716e-05
# alternative hypothesis: true location shift is less than 0

#BEHAV
# Independency
sample1<-data.frame(sample(data_computed_FEMALE_M4CID0$BEHAV,150))
sample2<-data.frame(sample(data_computed_FEMALE_M4CID1$BEHAV,150))
#Kendall correlation test
cor.test(sample1$sample.data_computed_FEMALE_M4CID0.BEHAV..150.,sample2$sample.data_computed_FEMALE_M4CID1.BEHAV..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_FEMALE_M4CID0.BEHAV..150. and sample2$sample.data_computed_FEMALE_M4CID1.BEHAV..150.
# z = 0.087917, p-value = 0.9299
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.004975126 
sample3<-data.frame(sample(data_computed_MALE_M4CID0$BEHAV,150))
sample4<-data.frame(sample(data_computed_MALE_M4CID1$BEHAV,150))
#Kendall correlation test
cor.test(sample3$sample.data_computed_MALE_M4CID0.BEHAV..150.,sample4$sample.data_computed_MALE_M4CID1.BEHAV..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_computed_MALE_M4CID0.BEHAV..150. and sample4$sample.data_computed_MALE_M4CID1.BEHAV..150.
# z = 0.34496, p-value = 0.7301
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.01941931 
# sampleas are independent

# Normality
hist(data_computed_FEMALE$BEHAV)
qqnorm(data_computed_FEMALE$BEHAV)
qqline(data_computed_FEMALE$BEHAV)
ks.test(data_computed_FEMALE$BEHAV,dnorm(mean(data_computed_FEMALE$BEHAV),sd(data_computed_FEMALE$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_FEMALE$BEHAV and dnorm(mean(data_computed_FEMALE$BEHAV), sd(data_computed_FEMALE$BEHAV))
# D = 1, p-value = 0.001658
# alternative hypothesis: two-sided
shapiro.test(data_computed_FEMALE$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed_FEMALE$BEHAV
# W = 0.97252, p-value = 3.385e-09
hist(data_computed_MALE$BEHAV)
qqnorm(data_computed_MALE$BEHAV)
qqline(data_computed_MALE$BEHAV)
ks.test(data_computed_MALE$BEHAV,dnorm(mean(data_computed_MALE$BEHAV),sd(data_computed_MALE$BEHAV)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_MALE$BEHAV and dnorm(mean(data_computed_MALE$BEHAV), sd(data_computed_MALE$BEHAV))
# D = 1, p-value = 0.001658
# alternative hypothesis: two-sided
shapiro.test(data_computed_MALE$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed_FEMALE$BEHAV
# W = 0.97252, p-value = 3.385e-09
# not normal

median(data_computed_FEMALE_M4CID0$BEHAV)#78
median(data_computed_FEMALE_M4CID1$BEHAV)#80
median(data_computed_MALE_M4CID0$BEHAV)#77
median(data_computed_MALE_M4CID1$BEHAV)#70
ggplot(data_computed) + geom_boxplot(aes(x=factor(M4CID),y=BEHAV, linetype = factor(GENDER)))
#It seems that the male/female don't benefit, male has a negative impact
wilcox.test(data_computed_FEMALE_M4CID0$BEHAV,data_computed_FEMALE_M4CID1$BEHAV, alternative = "less")
# #Wilcoxon rank sum test with continuity correction
# data:  data_computed_FEMALE_M4CID0$BEHAV and data_computed_FEMALE_M4CID1$BEHAV
# W = 38246, p-value = 0.2879
# alternative hypothesis: true location shift is greater than 0
wilcox.test(data_computed_MALE_M4CID0$BEHAV,data_computed_MALE_M4CID1$BEHAV, alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_MALE_M4CID0$BEHAV and data_computed_MALE_M4CID1$BEHAV
# W = 105232, p-value = 3.647e-12
# alternative hypothesis: true location shift is greater than 0

#CLASS
#Independency
sample1<-data.frame(sample(data_computed_FEMALE_M4CID0$CLASS,150))
sample2<-data.frame(sample(data_computed_FEMALE_M4CID1$CLASS,150))
#Kendall correlation test
cor.test(sample1$sample.data_computed_FEMALE_M4CID0.CLASS..150.,sample2$sample.data_computed_FEMALE_M4CID1.CLASS..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_FEMALE_M4CID0.CLASS..150. and sample2$sample.data_computed_FEMALE_M4CID1.CLASS..150.
# z = 1.0173, p-value = 0.309
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.05608063  
sample3<-data.frame(sample(data_computed_MALE_M4CID0$CLASS,150))
sample4<-data.frame(sample(data_computed_MALE_M4CID1$CLASS,150))
#Kendall correlation test
cor.test(sample3$sample.data_computed_MALE_M4CID0.CLASS..150.,sample4$sample.data_computed_MALE_M4CID1.CLASS..150.,method="kendall")
# Kendall's rank correlation tau
# data:  sample3$sample.data_computed_MALE_M4CID0.CLASS..150. and sample4$sample.data_computed_MALE_M4CID1.CLASS..150.
# z = 1.391, p-value = 0.1642
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.07670595  
#samples are independent

# Normality
hist(data_computed_FEMALE$CLASS)
qqnorm(data_computed_FEMALE$CLASS)
qqline(data_computed_FEMALE$CLASS)
ks.test(data_computed_FEMALE$CLASS,dnorm(mean(data_computed_FEMALE$CLASS),sd(data_computed_FEMALE$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_FEMALE$CLASS and dnorm(mean(data_computed_FEMALE$CLASS), sd(data_computed_FEMALE$CLASS))
# D = 1, p-value = 0.003317
# alternative hypothesis: two-sided
shapiro.test(data_computed_FEMALE$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed_FEMALE$CLASS
# W = 0.99434, p-value = 0.02469
hist(data_computed_MALE$CLASS)
qqnorm(data_computed_MALE$CLASS)
qqline(data_computed_MALE$CLASS)
ks.test(data_computed_MALE$CLASS,dnorm(mean(data_computed_MALE$CLASS),sd(data_computed_MALE$CLASS)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed_MALE$CLASS and dnorm(mean(data_computed_MALE$CLASS), sd(data_computed_MALE$CLASS))
# D = 1, p-value = 0.003317
# alternative hypothesis: two-sided
shapiro.test(data_computed_MALE$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed_FEMALE$CLASS
# W = 0.99434, p-value = 0.02469
# not normal

median(data_computed_FEMALE_M4CID0$CLASS)#68
median(data_computed_FEMALE_M4CID1$CLASS)#75
median(data_computed_MALE_M4CID0$CLASS)#68
median(data_computed_MALE_M4CID1$CLASS)#69
ggplot(data_computed) + geom_boxplot(aes(x=factor(M4CID),y=CLASS, linetype = factor(GENDER)))
#It seems that the female have more benefit then the male
wilcox.test(data_computed_FEMALE_M4CID0$CLASS,data_computed_FEMALE_M4CID1$CLASS, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_FEMALE_M4CID0$CLASS and data_computed_FEMALE_M4CID1$CLASS
# W = 26457, p-value = 4.369e-11
# alternative hypothesis: true location shift is less than 0

wilcox.test(data_computed_MALE_M4CID0$CLASS,data_computed_MALE_M4CID1$CLASS, alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_MALE_M4CID0$CLASS and data_computed_MALE_M4CID1$CLASS
# W = 73271, p-value = 0.003297
# alternative hypothesis: true location shift is not equal to 0

# Figure 15: junior sample boxplots for GENDER analysis 
grid.arrange(p60, p61, p62, p63,  nrow = 2,top="Box plot 4C/ID treatment effect", bottom="Figure 15: junior sample boxplots for GENDER analysis")

# Table 18: Inference sample junior school 1 for GENDER

#Conclusion
# Male for Test no impact; female positive impact
# Both benefit in LAB variable
# In the BEHAV variable negative impact for MALE and no impact for FEMALE
# positive impact on female Class variable but no impact for the male
# Female, averall, benefit more

# Inference for all sample: 2003 - 2014
summary(data)
data_2014<-filter(data,DATE <= 2014) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_2014_M4CID0<-filter(data,DATE <= 2014,M4CID==0) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_2014_M4CID1<-filter(data,DATE <= 2014,M4CID==1) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

length(data_2014_M4CID0$ID)#828
length(data_2014_M4CID1$ID)#277

#TEST
summary(data_2014)
hist(data_2014$TEST)
qqnorm(data_2014$TEST)
qqline(data_2014$TEST)
ks.test(data_2014$TEST,dnorm(mean(data_2014$TEST),sd(data_2014$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_2014$TEST and dnorm(mean(data_2014$TEST), sd(data_2014$TEST))
# D = 1, p-value = 0.001808
# alternative hypothesis: two-sided
shapiro.test(data_2014$TEST)
# Shapiro-Wilk normality test
# data:  data_2014$TEST
# W = 0.99021, p-value = 1.003e-06
#conclusion: not normal
sample1<-data.frame(sample(data_2014_M4CID0$TEST,250))
sample2<-data.frame(sample(data_2014_M4CID1$TEST,250))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_2014_M4CID0.TEST..250.,sample2$sample.data_2014_M4CID1.TEST..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_2014_M4CID0.TEST..250. and sample2$sample.data_2014_M4CID1.TEST..250.
# z = -0.41874, p-value = 0.6754
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01808312 
# sample independent
ggplot(data_2014) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_2014_M4CID0$TEST)#62
median(data_2014_M4CID1$TEST)#56
wilcox.test(data_2014_M4CID0$TEST,data_2014_M4CID1$TEST,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_2014_M4CID0$TEST and data_2014_M4CID1$TEST
# W = 127568, p-value = 0.002525
# alternative hypothesis: true location shift is greater than 0

#LAB
summary(data_2014)
hist(data_2014$LAB)
qqnorm(data_2014$LAB)
qqline(data_2014$LAB)
ks.test(data_2014$LAB,dnorm(mean(data_2014$LAB),sd(data_2014$LAB)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_2014$LAB and dnorm(mean(data_2014$LAB), sd(data_2014$LAB))
# D = 0.98733, p-value = 0.02532
# alternative hypothesis: two-sided
shapiro.test(data_2014$LAB)
# Shapiro-Wilk normality test
# data:  data_2014$LAB
# W = 0.97928, p-value = 1.84e-11
#conclusion: not normal
sample1<-data.frame(sample(data_2014_M4CID0$LAB,250))
sample2<-data.frame(sample(data_2014_M4CID1$LAB,250))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_2014_M4CID0.LAB..250.,sample2$sample.data_2014_M4CID1.LAB..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_2014_M4CID0.LAB..250. and sample2$sample.data_2014_M4CID1.LAB..250.
# z = -0.99282, p-value = 0.3208
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.04330126 
# sample independent
ggplot(data_2014) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_2014_M4CID0$LAB)#60
median(data_2014_M4CID1$LAB)#63
wilcox.test(data_2014_M4CID0$LAB,data_2014_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_2014_M4CID0$LAB and data_2014_M4CID1$LAB
# W = 97254, p-value = 7.459e-05
# alternative hypothesis: true location shift is less than 0

#BEHAV
summary(data_2014)
hist(data_2014$BEHAV)
qqnorm(data_2014$BEHAV)
qqline(data_2014$BEHAV)
ks.test(data_2014$BEHAV,dnorm(mean(data_2014$BEHAV),sd(data_2014$BEHAV)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_2014$BEHAV and dnorm(mean(data_2014$BEHAV), sd(data_2014$BEHAV))
# D = 1, p-value = 0.0009042
# alternative hypothesis: two-sided
shapiro.test(data_2014$BEHAV)
# Shapiro-Wilk normality test
# data:  data_2014$BEHAV
# W = 0.97215, p-value = 9.725e-14
#conclusion: not normal
sample1<-data.frame(sample(data_2014_M4CID0$BEHAV,250))
sample2<-data.frame(sample(data_2014_M4CID1$BEHAV,250))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_2014_M4CID0.BEHAV..250.,sample2$sample.data_2014_M4CID1.BEHAV..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_2014_M4CID0.BEHAV..250. and sample2$sample.data_2014_M4CID1.BEHAV..250.
# z = -0.3121, p-value = 0.755
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01356111 
# sample independent
ggplot(data_2014) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_2014_M4CID0$BEHAV)#79
median(data_2014_M4CID1$BEHAV)#83
wilcox.test(data_2014_M4CID0$BEHAV,data_2014_M4CID1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_2014_M4CID0$BEHAV and data_2014_M4CID1$BEHAV
# W = 94666, p-value = 6.69e-06
# alternative hypothesis: true location shift is less than 0

#CLASS
summary(data_2014)
hist(data_2014$CLASS)
qqnorm(data_2014$CLASS)
qqline(data_2014$CLASS)
ks.test(data_2014$CLASS,dnorm(mean(data_2014$CLASS),sd(data_2014$CLASS)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_2014$CLASS and dnorm(mean(data_2014$CLASS), sd(data_2014$CLASS))
# D = 1, p-value = 0.001808
# alternative hypothesis: two-sided
shapiro.test(data_2014$CLASS)
# Shapiro-Wilk normality test
# data:  data_2014$CLASS
# W = 0.99621, p-value = 0.008341
#conclusion: normal
sample1<-data.frame(sample(data_2014_M4CID0$CLASS,250))
sample2<-data.frame(sample(data_2014_M4CID1$CLASS,250))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_2014_M4CID0.CLASS..250.,sample2$sample.data_2014_M4CID1.CLASS..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_2014_M4CID0.CLASS..250. and sample2$sample.data_2014_M4CID1.CLASS..250.
# z = 1.0791, p-value = 0.2806
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04589267 
# sample independent
ggplot(data_2014) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_2014_M4CID0$CLASS)#64
median(data_2014_M4CID1$CLASS)#63
wilcox.test(data_2014_M4CID0$CLASS,data_2014_M4CID1$CLASS,alternative = "greater")
# Wilcoxon rank sum test with continuity correction
# data:  data_2014_M4CID0$CLASS and data_2014_M4CID1$CLASS
# W = 111986, p-value = 0.7209
# alternative hypothesis: true location shift is less than 0

# conclusion all sample with school effect 2003-2014
# TEST negative
# LAB, BEHAV positive 
# CLASS no effect;
# comparing to the sample from 2003 to 2017, it is clear that the last two years of the sample 
# make the difference

# Inference for all sample with school effect: 2003 - 2015
summary(data)
data_computed<-filter(data,DATE <= 2015) %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

# add to school 1
data_computed$TEST<-ifelse(data_computed$M4CID==1,data_computed$TEST+dif_mean_TEST,data_computed$TEST)
data_computed$LAB<-ifelse(data_computed$M4CID==1,data_computed$LAB+dif_mean_LAB,data_computed$LAB)
data_computed$CLASS<-ifelse(data_computed$M4CID==1,data_computed$CLASS+dif_mean_CLASS,data_computed$CLASS)
data_computed$BEHAV<-ifelse(data_computed$M4CID==1,data_computed$BEHAV+dif_mean_BEHAV,data_computed$BEHAV)

data_computed_M4CID0<-filter(data_computed,M4CID == "0") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)
data_computed_M4CID1<-filter(data_computed,M4CID == "1") %>% select(ID,DATE,GENDER,M4CID,GRADE,SCHOOL,TEST,LAB, BEHAV, CLASS)

length(data_computed_M4CID0$ID)#828
length(data_computed_M4CID1$ID)#277

#TEST
summary(data_computed)
hist(data_computed$TEST)
qqnorm(data_computed$TEST)
qqline(data_computed$TEST)
ks.test(data_computed$TEST,dnorm(mean(data_computed$TEST),sd(data_computed$TEST)))
# Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$TEST and dnorm(mean(data_computed$TEST), sd(data_computed$TEST))
# D = 1, p-value = 0.0009042
# alternative hypothesis: two-sided
shapiro.test(data_computed$TEST)
# Shapiro-Wilk normality test
# data:  data_computed$TEST
# W = 0.99651, p-value = 0.01438
#conclusion: not normal
sample1<-data.frame(sample(data_computed_M4CID0$TEST,250))
sample2<-data.frame(sample(data_computed_M4CID1$TEST,250))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_computed_M4CID0.TEST..250.,sample2$sample.data_computed_M4CID1.TEST..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_M4CID0.TEST..250. and sample2$sample.data_computed_M4CID1.TEST..250.
# z = -0.41874, p-value = 0.6754
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01808312 
# sample independent
ggplot(data_computed) + geom_boxplot(aes(y=TEST, x=factor(M4CID)))
median(data_computed_M4CID0$TEST)#62
median(data_computed_M4CID1$TEST)#68
wilcox.test(data_computed_M4CID0$TEST,data_computed_M4CID1$TEST,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_M4CID0$TEST and data_computed_M4CID1$TEST
# W = 85538, p-value = 1.161e-10
# alternative hypothesis: true location shift is less than 0

#LAB
summary(data_computed)
hist(data_computed$LAB)
qqnorm(data_computed$LAB)
qqline(data_computed$LAB)
ks.test(data_computed$LAB,dnorm(mean(data_computed$LAB),sd(data_computed$LAB)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$LAB and dnorm(mean(data_computed$LAB), sd(data_computed$LAB))
# D = 0.98733, p-value = 0.02712
# alternative hypothesis: two-sided
shapiro.test(data_computed$LAB)
# Shapiro-Wilk normality test
# data:  data_computed$LAB
# W = 0.98329, p-value = 5.933e-10
#conclusion: not normal
sample1<-data.frame(sample(data_computed_M4CID0$LAB,250))
sample2<-data.frame(sample(data_computed_M4CID1$LAB,250))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_computed_M4CID0.LAB..250.,sample2$sample.data_computed_M4CID1.LAB..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_M4CID0.LAB..250. and sample2$sample.data_computed_M4CID1.LAB..250.
# z = -0.99282, p-value = 0.3208
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.04330126 
# sample independent
ggplot(data_computed) + geom_boxplot(aes(y=LAB, x=factor(M4CID)))
median(data_computed_M4CID0$LAB)#60
median(data_computed_M4CID1$LAB)#76
wilcox.test(data_computed_M4CID0$LAB,data_computed_M4CID1$LAB,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_M4CID0$LAB and data_computed_M4CID1$LAB
# W = 56037, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

#BEHAV
summary(data_computed)
hist(data_computed$BEHAV)
qqnorm(data_computed$BEHAV)
qqline(data_computed$BEHAV)
ks.test(data_computed$BEHAV,dnorm(mean(data_computed$BEHAV),sd(data_computed$BEHAV)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$BEHAV and dnorm(mean(data_computed$BEHAV), sd(data_computed$BEHAV))
# D = 1, p-value = 0.0009042
# alternative hypothesis: two-sided
shapiro.test(data_computed$BEHAV)
# Shapiro-Wilk normality test
# data:  data_computed$BEHAV
# W = 0.97614, p-value = 1.614e-12
#conclusion: not normal
sample1<-data.frame(sample(data_computed_M4CID0$BEHAV,250))
sample2<-data.frame(sample(data_computed_M4CID1$BEHAV,250))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_computed_M4CID0.BEHAV..250.,sample2$sample.data_computed_M4CID1.BEHAV..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_M4CID0.BEHAV..250. and sample2$sample.data_computed_M4CID1.BEHAV..250.
# z = -0.3121, p-value = 0.755
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau -0.01356111 
# sample independent
ggplot(data_computed) + geom_boxplot(aes(y=BEHAV, x=factor(M4CID)))
median(data_computed_M4CID0$BEHAV)#79
median(data_computed_M4CID1$BEHAV)#80
wilcox.test(data_computed_M4CID0$BEHAV,data_computed_M4CID1$BEHAV,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_M4CID0$BEHAV and data_computed_M4CID1$BEHAV
# W = 111868, p-value = 0.2705
# alternative hypothesis: true location shift is less than 0

#CLASS
summary(data_computed)
hist(data_computed$CLASS)
qqnorm(data_computed$CLASS)
qqline(data_computed$CLASS)
ks.test(data_computed$CLASS,dnorm(mean(data_computed$CLASS),sd(data_computed$CLASS)))
#Exact two-sample Kolmogorov-Smirnov test
# data:  data_computed$CLASS and dnorm(mean(data_computed$CLASS), sd(data_computed$CLASS))
# D = 1, p-value = 0.001808
# alternative hypothesis: two-sided
shapiro.test(data_computed$CLASS)
# Shapiro-Wilk normality test
# data:  data_computed$CLASS
# W = 0.99857, p-value = 0.513
#conclusion: normal
sample1<-data.frame(sample(data_computed_M4CID0$CLASS,250))
sample2<-data.frame(sample(data_computed_M4CID1$CLASS,250))
#For Kendall, samples need to have the same length
cor.test(sample1$sample.data_computed_M4CID0.CLASS..250.,sample2$sample.data_computed_M4CID1.CLASS..250.,method="kendall")
# Kendall's rank correlation tau
# data:  sample1$sample.data_computed_M4CID0.CLASS..250. and sample2$sample.data_computed_M4CID1.CLASS..250.
# z = 1.0791, p-value = 0.2806
# alternative hypothesis: true tau is not equal to 0
# sample estimates: tau 0.04589267 
# sample independent
ggplot(data_computed) + geom_boxplot(aes(y=CLASS, x=factor(M4CID)))
median(data_computed_M4CID0$CLASS)#64
median(data_computed_M4CID1$CLASS)#72
wilcox.test(data_computed_M4CID0$CLASS,data_computed_M4CID1$CLASS,alternative = "less")
# Wilcoxon rank sum test with continuity correction
# data:  data_computed_M4CID0$CLASS and data_computed_M4CID1$CLASS
# W = 71619, p-value < 2.2e-16
# alternative hypothesis: true location shift is less than 0

# conclusion all sample with school effect 2003-2014
# TEST, LAB, CLASS positive effect;´
# BEHAV no effect;
# comparing to the sample from 2003 to 2017, it is clear that the last two years of the sample 
# make the difference


