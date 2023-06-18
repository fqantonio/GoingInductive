#INDEX
# Summary results all sample (with school effect) - 2003 to 2017 - 15
  # variable change - 53
# School 1 - 2003 to 2017 - 91
  # variable change - 135
# Summary results all sample (with school effect) - 2003 to 2014 - 175
  # variable change - 212
# School 1 - 2003 to 2014 - 250
  # variable change - ???
# All sample - ???
  # Variable change - ???

# libraries 
library(dplyr) #work with data frames
library(tidyverse) #include dplyr
library(ggplot2) # graphs
library(gridExtra) #arrange graphs in rows

# NOTE: https://r-charts.com/part-whole/diverging-bar-chart-ggplot2/
# for plotting changes
# the graphs with varables chnge are based on the sama data frame "results" which means
# hat you need to run results and resultsD for diferent graphs after running the data

########## Summary results all sample (with school effect) - 2003 to 2017
# for changes that have p-value that we can't reject the null I use the mean value

results <- data.frame(
  samples=c("all sample", "all sample (school effect)" , "JUNIOR (school effect)", "JUNIOR (school effect)(S1 with 4CID)", "GRADE 7 (school effect)","GRADE 8 (school effect)", "GRADE 9 (school effect)", "RANK 1 (school effect)", "RANK 2 (school effect)","RANK 3 (school effect)","FEMALE (school effect)","MALE (school effect)"),
  TEST1=c(-57,67,67,68,68,66,70,-37,58,80,67,67),
  TEST2=c(-62,69,70,69,68,70,70,-43,58,81,72,67),
  LAB1=c(60,63,63,62,63,60,60,30,57,77,63,62),
  LAB2=c(65,78,79,78,78,75,75,45,66,88,83,76),
  BEHAV1=c(-76,-73,-73,-73,-70,-69,76,-46,-64,-80,79,-70),
  BEHAV2=c(-79,-77,-77,-77,-76,-80,82,-56,-72,-85,79,-77),
  CLASS1=c(63,68,68,69,69,67,68,42,61,78,68,68),
  CLASS2=c(63,71,72,71,73,70,73,42,63,81,75,69)
)
results

ggplot(results,aes(group=TEST1)) +
  geom_errorbar(aes(x=samples, ymin=TEST1, ymax=TEST2,color='TEST'), width = ifelse(results$TEST1==results$TEST2, 1, 0.3))+
  geom_errorbar(aes(x=samples, ymin=LAB1, ymax=LAB2,color='LAB'), width = ifelse(results$LAB1==results$LAB2, 1, 0.3)) +
  geom_errorbar(aes(x=samples, ymin=BEHAV1, ymax=BEHAV2, color='BEHAV'), width = ifelse(results$BEHAV1==results$BEHAV2, 1, 0.3)) +
  geom_errorbar(aes(x=samples, ymin=CLASS1, ymax=CLASS2, color='CLASS'),width = ifelse(results$CLASS1==results$CLASS2, 1, 0.3)) +
  xlab("Samples")+labs(title="4C/ID treatment effect resume results: all sample with school effect",y = "Variables changes",subtitle = "* vertical bold lines means 'no effect'")+
  scale_color_manual(name='Variables',
                     breaks=c('TEST','LAB', 'BEHAV', 'CLASS'),
                     values=c('TEST'='red', 'LAB'='blue', 'BEHAV'='green', 'CLASS'='orange'))+
  theme_bw()+
  theme(panel.grid.major.y = element_line(color = 3,linewidth = 0.25, linetype = 3),panel.background = element_rect(color = 4, linewidth = 1),plot.background = element_rect(fill = "lightyellow"))+
  geom_rect(aes(xmin=0,xmax=Inf,ymin = - Inf,ymax = 0),fill="red",alpha = 0.01) +
  geom_rect(aes(xmin=0,xmax=Inf,ymin = 0,ymax = Inf),fill="green",alpha = 0.01) +
  scale_x_discrete(limits=c("all sample", "all sample (school effect)" , "JUNIOR (school effect)", "JUNIOR (school effect)(S1 with 4CID)", "GRADE 7 (school effect)","GRADE 8 (school effect)", "GRADE 9 (school effect)", "RANK 1 (school effect)", "RANK 2 (school effect)","RANK 3 (school effect)","FEMALE (school effect)","MALE (school effect)"))+
  scale_y_continuous(breaks = seq(-100, 100, by = 10))+
  geom_hline(yintercept=c(-50,50),color="red",linetype=2)+
  annotate(geom="text", x=12, y=-10, label="Negative effect",color="red3",hjust=1)+
  annotate(geom="text", x=12, y=10, label="Positive effect*",color="green3",hjust=0)+
  coord_flip()

# Results: variable change
# prepare data frame for bar ploting
# IMPORTANT NOTE: the negative values are used to plot them in a different area of the graph for 
# negative effects. However, to use this data-frames to compute changes we need to have this values 
# exchanged in order to compute negative change values.
results$DTEST <- results$TEST2-results$TEST1
results$DLAB <- results$LAB2-results$LAB1
results$DBEHAV <- results$BEHAV2-results$BEHAV1
results$DCLASS <- results$CLASS2-results$CLASS1
results
resultsDTEST <- results %>% select(samples,DTEST)
resultsDTEST$GROUP <-"TEST"
names(resultsDTEST)[2]<-'VALUE'
resultsDTEST
resultsDLAB <- results %>% select(samples,DLAB)
resultsDLAB$GROUP <-"LAB"
names(resultsDLAB)[2]<-'VALUE'
resultsDLAB
resultsDBEHAV <- results %>% select(samples,DBEHAV)
resultsDBEHAV$GROUP <-"BEHAV"
names(resultsDBEHAV)[2]<-'VALUE'
resultsDBEHAV
resultsDCLASS <- results %>% select(samples,DCLASS)
resultsDCLASS$GROUP <-"CLASS"
names(resultsDCLASS)[2]<-'VALUE'
resultsDCLASS
resultsD<-bind_rows(resultsDTEST,resultsDLAB,resultsDBEHAV,resultsDCLASS)
resultsD

ggplot(resultsD,aes(y=samples,x=VALUE,fill=GROUP)) +
  geom_bar(stat = "identity",show.legend = TRUE,position = position_dodge(width = 1))+
  ylab("Samples")+labs(title="4C/ID treatment effect: all sample with school effect",x = "Variables changes")+
  theme(panel.grid.major.y = element_line(color = 3,linewidth = 0.25, linetype = 3),panel.background = element_rect(color = 4, linewidth = 1),plot.background = element_rect(fill = "lightyellow"))+
  annotate(geom="text", x=-6, y=11.5, label="Negative effect",color="red3",hjust=1)+
  annotate(geom="text", x=15, y=11.5, label="Positive effect",color="green3",hjust=0)
  #scale_y_discrete(limits=c("SCHOOL 1", "SCHOOL 1 - JUNIOR","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"))
  #scale_x_continuous(breaks = seq(-20, 20, by = 5))
  
########## Summary results for SCHOOL 1 sample only - 2003 to 2017
# for changes that have p-value that we can't reject the null I use the mean value
results <- data.frame(
  samples=c("SCHOOL 1", "SCHOOL 1 - JUNIOR","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"),
  TEST1=c(54,54,56,54,56,34,52,78,54,54),
  TEST2=c(57,58,56,58,58,33,52,78,64,54),
  LAB1=c(51,51,56,51,44,26,51,74,50,53),
  LAB2=c(65,66,69,62,66,35,60,81,70,63),
  BEHAV1=c(-76,-76,-73,-72,80,-53,-72,-88,82,-73),
  BEHAV2=c(-80,-81,-77,-88,85,-65,-80,-94,82,-80),
  CLASS1=c(58,58,60,58,55,39,57,76,58,58),
  CLASS2=c(62,63,64,61,64,39,57,80,66,60)
)

# This data.frame file ResultsD is also used for the all sample school effect. Please build it starting
# with the new data fle "results" continue to the line "Results change" to have the same file but only for the school 1 sample.

ggplot(resultsD,aes(y=samples,x=VALUE,fill=GROUP)) +
  geom_bar(stat = "identity",show.legend = TRUE,position = position_dodge(width = 1))+
  ylab("Samples")+labs(title="4C/ID treatment effect: school 1",x = "Variables changes")+
  theme(panel.grid.major.y = element_line(color = 3,linewidth = 0.25, linetype = 3),panel.background = element_rect(color = 4, linewidth = 1),plot.background = element_rect(fill = "lightyellow"))+
  annotate(geom="text", x=-6, y=9.5, label="Negative effect",color="red3",hjust=1)+
  annotate(geom="text", x=15, y=9.5, label="Positive effect",color="green3",hjust=0)

ggplot(results,aes(group=TEST1)) +
  geom_errorbar(aes(x=samples, ymin=TEST1, ymax=TEST2,color='TEST'), width = ifelse(results$TEST1==results$TEST2, 1, 0.3))+
  geom_errorbar(aes(x=samples, ymin=LAB1, ymax=LAB2,color='LAB'), width = ifelse(results$LAB1==results$LAB2, 1, 0.3)) +
  geom_errorbar(aes(x=samples, ymin=BEHAV1, ymax=BEHAV2, color='BEHAV'), width = ifelse(results$BEHAV1==results$BEHAV2, 1, 0.3)) +
  geom_errorbar(aes(x=samples, ymin=CLASS1, ymax=CLASS2, color='CLASS'),width = ifelse(results$CLASS1==results$CLASS2, 1, 0.3)) +
  xlab("Samples")+labs(title="4C/ID treatment effect resume results: school 1 sample",y = "Variables changes",subtitle = "* vertical bold lines means 'no effect'")+
  scale_color_manual(name='Variables',
                     breaks=c('TEST','LAB', 'BEHAV', 'CLASS'),
                     values=c('TEST'='red', 'LAB'='blue', 'BEHAV'='green', 'CLASS'='orange'))+
  theme_bw()+
  theme(panel.grid.major.y = element_line(color = 3,linewidth = 0.25, linetype = 3),panel.background = element_rect(color = 4, linewidth = 1),plot.background = element_rect(fill = "lightyellow"))+
  geom_rect(aes(xmin=0,xmax=Inf,ymin = - Inf,ymax = 0),fill="red",alpha = 0.01) +
  geom_rect(aes(xmin=0,xmax=Inf,ymin = 0,ymax = Inf),fill="green",alpha = 0.01) +
  scale_x_discrete(limits=c("SCHOOL 1", "SCHOOL 1 - JUNIOR","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"))+
  scale_y_continuous(breaks = seq(-100, 100, by = 10))+
  geom_hline(yintercept=c(-50,50),color="red",linetype=2)+
  annotate(geom="text", x=9.5, y=-10, label="Negative effect",color="red3",hjust=1)+
  annotate(geom="text", x=9.5, y=10, label="Positive effect*",color="green3",hjust=0)+
  coord_flip()

# Results: variable change
# prepare data frame for bar ploting
# IMPORTANT NOTE: the negative values are used to plot them in a different area of the graph for 
# negative effects. However, to use this data-frames to compute changes we need to have this values 
# exchanged in order to compute negative change values.
results$DTEST <- results$TEST2-results$TEST1
results$DLAB <- results$LAB2-results$LAB1
results$DBEHAV <- results$BEHAV2-results$BEHAV1
results$DCLASS <- results$CLASS2-results$CLASS1
results
resultsDTEST <- results %>% select(samples,DTEST)
resultsDTEST$GROUP <-"TEST"
names(resultsDTEST)[2]<-'VALUE'
resultsDTEST
resultsDLAB <- results %>% select(samples,DLAB)
resultsDLAB$GROUP <-"LAB"
names(resultsDLAB)[2]<-'VALUE'
resultsDLAB
resultsDBEHAV <- results %>% select(samples,DBEHAV)
resultsDBEHAV$GROUP <-"BEHAV"
names(resultsDBEHAV)[2]<-'VALUE'
resultsDBEHAV
resultsDCLASS <- results %>% select(samples,DCLASS)
resultsDCLASS$GROUP <-"CLASS"
names(resultsDCLASS)[2]<-'VALUE'
resultsDCLASS
resultsD<-bind_rows(resultsDTEST,resultsDLAB,resultsDBEHAV,resultsDCLASS)
resultsD

ggplot(resultsD,aes(y=samples,x=VALUE,fill=GROUP)) +
  geom_bar(stat = "identity",show.legend = TRUE,position = position_dodge(width = 1))+
  ylab("Samples")+labs(title="4C/ID treatment effect: all sample with school effect",x = "Variables changes")+
  theme(panel.grid.major.y = element_line(color = 3,linewidth = 0.25, linetype = 3),panel.background = element_rect(color = 4, linewidth = 1),plot.background = element_rect(fill = "lightyellow"))+
  annotate(geom="text", x=-6, y=11.5, label="Negative effect",color="red3",hjust=1)+
  annotate(geom="text", x=15, y=11.5, label="Positive effect",color="green3",hjust=0)
#scale_y_discrete(limits=c("SCHOOL 1", "SCHOOL 1 - JUNIOR","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"))
#scale_x_continuous(breaks = seq(-20, 20, by = 5))

########## Summary results all sample (with school effect) - 2003 to 2014
# for changes that have p-value that we can't reject the null I use the mean value
# Rank 1 small sample
results <- data.frame(
  samples=c("all sample", "all sample (school effect)" , "JUNIOR (school effect)", "JUNIOR (school effect)(S1 with 4CID)", "GRADE 7 (school effect)","GRADE 8 (school effect)", "GRADE 9 (school effect)", "RANK 2 (school effect)","RANK 3 (school effect)","FEMALE (school effect)","MALE (school effect)"),
  TEST1=c(62,62,67,68,66,66,70,64,83,67,67),
  TEST2=c(56,68,70,70,74,68,70,68,93,70,67),
  LAB1=c(60,60,63,62,66,60,61,57,77,63,62),
  LAB2=c(63,76,77,77,84,69,79,66,85,81,71),
  BEHAV1=c(79,79,77,77,77,79,76,72,85,78,77),
  BEHAV2=c(83,79,80,80,77,79,86,72,85,83,77),
  CLASS1=c(64,64,68,69,68,67,68,61,78,68,68),
  CLASS2=c(64,72,73,73,76,69,73,64,82,75,70)
)
results
ggplot(results,aes(group=TEST1)) +
  geom_errorbar(aes(x=samples, ymin=TEST1, ymax=TEST2,color='TEST'), width = ifelse(results$TEST1==results$TEST2, 1, 0.3))+
  geom_errorbar(aes(x=samples, ymin=LAB1, ymax=LAB2,color='LAB'), width = ifelse(results$LAB1==results$LAB2, 1, 0.3)) +
  geom_errorbar(aes(x=samples, ymin=BEHAV1, ymax=BEHAV2, color='BEHAV'), width = ifelse(results$BEHAV1==results$BEHAV2, 1, 0.3)) +
  geom_errorbar(aes(x=samples, ymin=CLASS1, ymax=CLASS2, color='CLASS'),width = ifelse(results$CLASS1==results$CLASS2, 1, 0.3)) +
  xlab("Samples")+labs(title="4C/ID treatment effect resume results: all sample with school effect: until 2014",y = "Variables changes",subtitle = "* vertical bold lines means 'no effect'")+
  scale_color_manual(name='Variables',
                     breaks=c('TEST','LAB', 'BEHAV', 'CLASS'),
                     values=c('TEST'='red', 'LAB'='blue', 'BEHAV'='green', 'CLASS'='orange'))+
  theme_bw()+
  theme(panel.grid.major.y = element_line(color = 3,linewidth = 0.25, linetype = 3),panel.background = element_rect(color = 4, linewidth = 1),plot.background = element_rect(fill = "lightyellow"))+
  geom_rect(aes(xmin=0,xmax=Inf,ymin = - Inf,ymax = 0),fill="red",alpha = 0.01) +
  geom_rect(aes(xmin=0,xmax=Inf,ymin = 0,ymax = Inf),fill="green",alpha = 0.01) +
  scale_x_discrete(limits=c("all sample", "all sample (school effect)" , "JUNIOR (school effect)", "JUNIOR (school effect)(S1 with 4CID)", "GRADE 7 (school effect)","GRADE 8 (school effect)", "GRADE 9 (school effect)", "RANK 1 (school effect)", "RANK 2 (school effect)","RANK 3 (school effect)","FEMALE (school effect)","MALE (school effect)"))+
  scale_y_continuous(breaks = seq(-100, 100, by = 10))+
  geom_hline(yintercept=c(-50,50),color="red",linetype=2)+
  annotate(geom="text", x=12, y=-10, label="Negative effect",color="red3",hjust=1)+
  annotate(geom="text", x=12, y=10, label="Positive effect*",color="green3",hjust=0)+
  coord_flip()

# Results: variable change
# prepare data frame for bar ploting
# IMPORTANT NOTE: the negative values are used to plot them in a different area of the graph for 
# negative effects. However, to use this data-frames to compute changes we need to have this values 
# exchanged in order to compute negative change values.
results$DTEST <- results$TEST2-results$TEST1
results$DLAB <- results$LAB2-results$LAB1
results$DBEHAV <- results$BEHAV2-results$BEHAV1
results$DCLASS <- results$CLASS2-results$CLASS1
results
resultsDTEST <- results %>% select(samples,DTEST)
resultsDTEST$GROUP <-"TEST"
names(resultsDTEST)[2]<-'VALUE'
resultsDTEST
resultsDLAB <- results %>% select(samples,DLAB)
resultsDLAB$GROUP <-"LAB"
names(resultsDLAB)[2]<-'VALUE'
resultsDLAB
resultsDBEHAV <- results %>% select(samples,DBEHAV)
resultsDBEHAV$GROUP <-"BEHAV"
names(resultsDBEHAV)[2]<-'VALUE'
resultsDBEHAV
resultsDCLASS <- results %>% select(samples,DCLASS)
resultsDCLASS$GROUP <-"CLASS"
names(resultsDCLASS)[2]<-'VALUE'
resultsDCLASS
resultsD<-bind_rows(resultsDTEST,resultsDLAB,resultsDBEHAV,resultsDCLASS)
resultsD

ggplot(resultsD,aes(y=samples,x=VALUE,fill=GROUP)) +
  geom_bar(stat = "identity",show.legend = TRUE,position = position_dodge(width = 1))+
  ylab("Samples")+labs(title="4C/ID treatment effect: all sample with school effect",x = "Variables changes")+
  theme(panel.grid.major.y = element_line(color = 3,linewidth = 0.25, linetype = 3),panel.background = element_rect(color = 4, linewidth = 1),plot.background = element_rect(fill = "lightyellow"))+
  annotate(geom="text", x=-6, y=11.5, label="Negative effect",color="red3",hjust=1)+
  annotate(geom="text", x=15, y=11.5, label="Positive effect",color="green3",hjust=0)
#scale_y_discrete(limits=c("SCHOOL 1", "SCHOOL 1 - JUNIOR","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"))
#scale_x_continuous(breaks = seq(-20, 20, by = 5))

########## Summary results for SCHOOL 1 sample only - 2003 to 2014
# for changes that have p-value that we can't reject the null I use the mean value
results <- data.frame(
  samples=c("SCHOOL 1", "SCHOOL 1 - JUNIOR","GRADE 7*","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"),
  TEST1=c(54,54,54,54,57,34,-51,79,54,54),
  TEST2=c(56,58,62,57,57,34,-53,79,58,54),
  LAB1=c(51,51,56,51,44,26,51,74,50,53),
  LAB2=c(63,64,71,56,66,35,58,80,68,59),
  BEHAV1=c(80,82,80,-83,80,65,-72,93,82,80),
  BEHAV2=c(83,82,80,-88,85,65,-80,93,86,80),
  CLASS1=c(58,58,60,58,55,39,57,76,58,58),
  CLASS2=c(63,64,67,60,64,39,57,80,66,61)
)

# * sample with 20 obs.

ggplot(results,aes(group=TEST1)) +
  geom_errorbar(aes(x=samples, ymin=TEST1, ymax=TEST2,color='TEST'), width = ifelse(results$TEST1==results$TEST2, 1, 0.3))+
  geom_errorbar(aes(x=samples, ymin=LAB1, ymax=LAB2,color='LAB'), width = ifelse(results$LAB1==results$LAB2, 1, 0.3)) +
  geom_errorbar(aes(x=samples, ymin=BEHAV1, ymax=BEHAV2, color='BEHAV'), width = ifelse(results$BEHAV1==results$BEHAV2, 1, 0.3)) +
  geom_errorbar(aes(x=samples, ymin=CLASS1, ymax=CLASS2, color='CLASS'),width = ifelse(results$CLASS1==results$CLASS2, 1, 0.3)) +
  xlab("Samples")+labs(title="4C/ID treatment effect resume results: school 1 sample",y = "Variables changes",subtitle = "* vertical bold lines means 'no effect'")+
  scale_color_manual(name='Variables',
                     breaks=c('TEST','LAB', 'BEHAV', 'CLASS'),
                     values=c('TEST'='red', 'LAB'='blue', 'BEHAV'='green', 'CLASS'='orange'))+
  theme_bw()+
  theme(panel.grid.major.y = element_line(color = 3,linewidth = 0.25, linetype = 3),panel.background = element_rect(color = 4, linewidth = 1),plot.background = element_rect(fill = "lightyellow"))+
  geom_rect(aes(xmin=0,xmax=Inf,ymin = - Inf,ymax = 0),fill="red",alpha = 0.01) +
  geom_rect(aes(xmin=0,xmax=Inf,ymin = 0,ymax = Inf),fill="green",alpha = 0.01) +
  scale_x_discrete(limits=c("SCHOOL 1", "SCHOOL 1 - JUNIOR","GRADE 7*","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"))+
  scale_y_continuous(breaks = seq(-100, 100, by = 10))+
  geom_hline(yintercept=c(-50,50),color="red",linetype=2)+
  annotate(geom="text", x=9.5, y=-10, label="Negative effect",color="red3",hjust=1)+
  annotate(geom="text", x=9.5, y=10, label="Positive effect*",color="green3",hjust=0)+
  coord_flip()

# Results: variable change
# prepare data frame for bar ploting
# IMPORTANT NOTE: the negative values are used to plot them in a different area of the graph for 
# negative effects. However, to use this data-frames to compute changes we need to have this values 
# exchanged in order to compute negative change values.
results$DTEST <- results$TEST2-results$TEST1
results$DLAB <- results$LAB2-results$LAB1
results$DBEHAV <- results$BEHAV2-results$BEHAV1
results$DCLASS <- results$CLASS2-results$CLASS1
results
resultsDTEST <- results %>% select(samples,DTEST)
resultsDTEST$GROUP <-"TEST"
names(resultsDTEST)[2]<-'VALUE'
resultsDTEST
resultsDLAB <- results %>% select(samples,DLAB)
resultsDLAB$GROUP <-"LAB"
names(resultsDLAB)[2]<-'VALUE'
resultsDLAB
resultsDBEHAV <- results %>% select(samples,DBEHAV)
resultsDBEHAV$GROUP <-"BEHAV"
names(resultsDBEHAV)[2]<-'VALUE'
resultsDBEHAV
resultsDCLASS <- results %>% select(samples,DCLASS)
resultsDCLASS$GROUP <-"CLASS"
names(resultsDCLASS)[2]<-'VALUE'
resultsDCLASS
resultsD<-bind_rows(resultsDTEST,resultsDLAB,resultsDBEHAV,resultsDCLASS)
resultsD

ggplot(resultsD,aes(y=samples,x=VALUE,fill=GROUP)) +
  geom_bar(stat = "identity",show.legend = TRUE,position = position_dodge(width = 1))+
  ylab("Samples")+labs(title="4C/ID treatment effect: 2009-2014 sample for school 1",x = "Variables changes")+
  theme(panel.grid.major.y = element_line(color = 3,linewidth = 0.25, linetype = 3),panel.background = element_rect(color = 4, linewidth = 1),plot.background = element_rect(fill = "lightyellow"))+
  annotate(geom="text", x=-6, y=11.5, label="Negative effect",color="red3",hjust=1)+
  annotate(geom="text", x=15, y=11.5, label="Positive effect",color="green3",hjust=0)
#scale_y_discrete(limits=c("SCHOOL 1", "SCHOOL 1 - JUNIOR","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"))
#scale_x_continuous(breaks = seq(-20, 20, by = 5))

