#INDEX
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

# data upload
#Use this step before any of the analysis bellow
#perhaps you need to set your set working directory: setwd: setwd("/Users/novo/Desktop/Doutoramento/GoingInductive")
setwd("/Users/novo/Desktop/Doutoramento/GoingInductivePaper/GoingInductive/DATA")
data<-read.csv("./DATA_4CID_2023.csv") #the date 2023 on this csv file means that the data is used for 2023 paper going indcutive
data<-data %>% mutate(CLASS=0.5*TEST+0.3*LAB+0.2*BEHAV)
colnames(data) <- c("ID","DATE" ,"SCHOOL" ,"GENDER" ,"TEST" ,"LAB" ,"BEHAV" ,"GRADE" ,"M4CID","CLASS")
head(data)
# sample differences between school 0 and school 1
summary(data)

# RESULTS
#Inference (and independency analysis)
#Inference treatment and control group for all sample, no matter which school, 
#for each data variables: TEST, LAB and BEHAV
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

# Inference for computed junior treatment effect with school effect
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

# Inference for treatment effect all sample, excluding School 1 with no 4C/ID
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

