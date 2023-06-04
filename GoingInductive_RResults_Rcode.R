#INDEX
# SUMMARY results and observations/notes - 2458

# libraries 
library(dplyr) #work with data frames
#library(tidyverse) #include dplyr
library(ggplot2) # graphs
library(gridExtra) #arrange graphs in rows

########## Summary results all sample

#2003 to 2017
# for changes that have p-value that we can't reject the null I use the mean value
results <- data.frame(
  samples=c("all sample", "all sample (school effect)" , "JUNIOR (school effect)", "JUNIOR (school effect)(S1 with 4CID)", "GRADE 7 (school effect)","GRADE 8 (school effect)", "GRADE 9 (school effect)", "RANK 1 (school effect)", "RANK 2 (school effect)","RANK 3 (school effect)","FEMALE (school effect)","MALE (school effect)"),
  TEST1=c(-62,67,67,68,68),
  TEST2=c(-57,69,70,69,68),
  LAB1=c(60,63,63,62,63),
  LAB2=c(65,78,79,78,78),
  BEHAV1=c(-79,-77,-77,-77,-76),
  BEHAV2=c(-76,-73,-73,-73,-70),
  CLASS1=c(63,68,68,69,69),
  CLASS2=c(63,71,72,71,73)
)

?labs
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

########## Summary results SCHOOL 1

#2003 to 2017
# for changes that have p-value that we can't reject the null I use the mean value
results <- data.frame(
  samples=c("all sample", "all sample (school effect)" , "JUNIOR (school effect)", "school 1","JUNIOR SCHOOL 1","GRADE 7","GRADE 8", "GRADE 9", "RANK 1", "RANK 2","RANK 3","FEMALE","MALE"),
  TEST1=c(-62,67, 67, 54,54 ,56 ,54 ,57 ,33 ,53,80,54,55),
  TEST2=c(-57,69, 70, 57,58 ,56 ,58 ,57 ,33 ,53,80,61,55),
  LAB1=c(60,63 ,63, 51,51 ,56 ,51 ,44 ,26 ,51,73,50,53),
  LAB2=c(65,78, 79, 65,66 ,69 ,62 ,66 ,35 ,61,81,70,61),
  BEHAV1=c(-79,-77,-77, -80,-81,-77,-85,80 ,-65 ,-80,-94,82,-80),
  BEHAV2=c(-76,-72, -73, -76,-76,-71,-60,85 ,-54 ,-72,-91,82,-72),
  CLASS1=c( -64, 68,68,59 ,60 ,50 ,55 ,39 ,57,77,58,58),
  CLASS2=c( -62, 71,72,62 ,64 ,61 ,64 ,39 ,57,81,67,60)
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

