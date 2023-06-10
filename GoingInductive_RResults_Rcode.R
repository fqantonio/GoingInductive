#INDEX
# SUMMARY results and observations/notes 
# all Sample - 15 
# School 1 - 50

# libraries 
library(dplyr) #work with data frames
library(tidyverse) #include dplyr
library(ggplot2) # graphs
library(gridExtra) #arrange graphs in rows

# NOTE: https://r-charts.com/part-whole/diverging-bar-chart-ggplot2/
# for plotting changes

########## Summary results all sample (with school effect) - 2003 to 2017
# for changes that have p-value that we can't reject the null I use the mean value

results <- data.frame(
  samples=c("all sample", "all sample (school effect)" , "JUNIOR (school effect)", "JUNIOR (school effect)(S1 with 4CID)", "GRADE 7 (school effect)","GRADE 8 (school effect)", "GRADE 9 (school effect)", "RANK 1 (school effect)", "RANK 2 (school effect)","RANK 3 (school effect)","FEMALE (school effect)","MALE (school effect)"),
  TEST1=c(-62,67,67,68,68,66,66,-43,58,80,67,67),
  TEST2=c(-57,69,70,69,68,70,70,-37,58,81,72,67),
  LAB1=c(60,63,63,62,63,60,60,30,57,77,63,62),
  LAB2=c(65,78,79,78,78,75,75,45,66,88,83,76),
  BEHAV1=c(-79,-77,-77,-77,-76,-80,76,-56,-72,-85,79,-77),
  BEHAV2=c(-76,-73,-73,-73,-70,-69,82,-46,-64,-80,79,-70),
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
  
########## Summary results for SCHOOL 1 sample only - 2003 to 2017
# for changes that have p-value that we can't reject the null I use the mean value
results <- data.frame(
  samples=c("SCHOOL 1", "SCHOOL 1 - JUNIOR","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"),
  TEST1=c(54,54,56,54,56,34,52,78,54,54),
  TEST2=c(57,58,56,58,58,33,52,78,64,54),
  LAB1=c(51,51,56,51,44,26,51,74,50,53),
  LAB2=c(65,66,69,62,66,35,60,81,70,63),
  BEHAV1=c(-80,-81,-77,-88,80,-65,-80,-94,82,-80),
  BEHAV2=c(-76,-76,-73,-72,85,-53,-72,-88,82,-73),
  CLASS1=c(58,88,60,58,55,39,57,76,58,58),
  CLASS2=c(62,63,64,61,64,39,57,80,66,60)
)

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

