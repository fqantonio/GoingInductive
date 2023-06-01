# WELCOME

This is a **`Going Inductive`** paper DRAFT roadmap[^readme-1], with one major goal, to share the work done, worldwide. This **knowledge base source** contains the results, conclusions, procedures, exploratory data analysis (EDA), data (and raw data), statistical methods, educational methodologies, learning flow and bLearning practice. Hopefully, there will be, at least, one paper based on this.

[^readme-1]: This work, as well as the data, is also presented at the blog "<https://4cidchange.edublogs.org/>" and in the github repository <https://github.com/fqantonio/GoingInductive>.

# ABSTRACT

Should you, as a teacher of science and tech of young students, **move to an inductive strategy**, for the learning design flow with all the activities chronologically organized, even if the setting is bLearning? In overall, the **answer is yes**, that is, 4C/ID eLearning setting as some advantages to Direct Instruction but with some remarks. If you decide to do it, there will be specific impacts for the students that need to be **accommodated**, as well as implications for the stakeholders in the learning process: policy makers and school management. The sample covers 12 years of teacher data assessment, during the period 2003 to 2018, with conclusions about the impact in the Learning Transfer, Lab Practice, Social Skills and Academic Results.

> Education is the most powerful weapon which you can use to change the world. 
>
>                                                 Nelson Mandela

**Keywords** Inductive learning strategy; 4C/ID; Direct Instruction; Learning Flow; Statistical Non-parametric Inference; academic results; learning transfer; behavior; lab practice; Cognitive Load Theory; Multi Intelligence Theory; Brain Based Learning; bLearning.

# GOALS, RESEARCH QUESTIONS and HYPOTHESIS

The **main goal** is to understand the impact of the use of Inductive Methodology Four Component Instructional (4C/ID) in a bLearning setting, by following this mains **research question**: changing the learning methodologies from one mainly deductive, denominated by Direct Instruction (Merrill, 2007) to another one mostly inductive, Merriënboer's 4C/ID design theory (van Merriënboer, Kirschner, 2007), means what for students learning outcomes? Who benefits? What are the pros and cons about this methodological decision? What are the implications for the stakeholders?

The **hypothesis** is that this change will have a positive impact in all learning areas because, in overall, it is based on the 4C/ID methodology that, in itself, is anchored on Brain Based Learning (BBL), Cognitive Load Theory (CLT), Multi-Intelligence Theory (MIT) and Multimedia Learning (ML).

# CONTEXT

The work presented means a major change from a, **direct instruction** (Merril, 2007) classroom setting to an inductive strategy, **four components instructional design** (4C/ID) (Van Merrienboer, Clark, Coock, 2002), in a bLearning environment (Mayer, 2005).

The data used in this work was gathered over 12 years, in the period 2003 to 2017, from actual teacher day-to-day practice assessments about students **Learning Transfer**, **Lab Practice**, **Classroom Behavior**, that include Social Skills, and **Academic Results**. It includes junior and junior high students of two different schools, from different regions, for the chemistry and physics subjects of the Portuguese curriculum.

One of the main motivations is to assess the educational work developed: was it worth it? Did students benefit? What can be done better? What conclusions can be drawned? Most of the time the professional, specific, **day-to-day teacher work** data is rarely or never investigated, at least, in the Portuguese educational environment. For the teachers, there is little or no time to look back and work through the data results, the qualitative remarks, or through the statistics. And, as a consequence, no robust conclusions are drowned and worse, it's normal not to share it.

The concept map's software cmap tools[^readme-2] was used for the design of the learning flow maps and they are presented online[^readme-3] showing the four components and the structure based in the four component instructional design (4C/ID), described in the book *Ten Steps to Complex Learning* (van Merriënboer; Kirschner, 2007). This maps are used since 2012, included.

[^readme-2]: <https://cmap.ihmc.us/>

[^readme-3]: Flow charts (missing details to be updated) Data base sample (R data frame): <https://cmap.ihmc.us/> ;

# DATA

The main data **Variables** assessed in practice are TEST, LAB, BEHAV and CLASS, that are linked to **Learning Transfer**, **Lab Practice**, **Classroom Behavior** (including) Social Skills, and **Academic Results**, respectively.

Table 2 and 3 shows a short **descriptive statistics** summary of the samples for school 0 and 1. This is a longitudinal sample with 1414 rows (observations). The best sample is the one for just SCHOOL 1 and Junior grades: the other are not balanced and have a school effect identified in the work process. Table 3 shows that the samples are not normaly distributed in spite of the fact that the mean and median are similar, the samples have heavy tails because of the high values of the standart deviations. Values of the vairables cover almost all the possible outcomes.

| GRADE | Obs. | %  | Notes |
|:---:|:---:|:---:|:---:|
| 0 (7th) | 353 | 25% |
| 1 (8th) | 411 | 29% | 
| 2 (9th) | 396 | 28% |
| 3 (10th) | 27 | 2% |
| 4 (11th) | 18 | 1% |
| 5 (10technical) | 117 | 8% |
| 6 (11technical) | 93 | 7% |

Table 2: distribution by grade 

| Variable | Obs. | Mean | Median | Min.| Max. | St. Deviation | Notes |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|---|
| TEST | 1414 | 61 | 60 | 0 | 100 | 17 |  |
| LAB | 1414 | 60 | 60 | 0 | 100 | 19 |  | 
| BEHAV | 1414 | 76 | 77 | 27 | 100 | 15 | |
| CLASS | 1414 | 64 | 63 | 14 | 100 | 14 | |

Table 3: schools descriptive data sample

Looking to table 2, its clear that sample variables cover a wide range of the possible values and the means and medians are very close, this can be a clue about data normality. But, further inference analysis shows that it is not the case: samples are skewed and have heavy tails: big values of the standards deviations. Figure 1 shows the non-normal samples for each variable for the global sample. Shapiro tests show low p-values << 0.05, which means that there is a 95% sure to reject the null: they are not similar to the normal distribution. 

![Figure 1: all sample histograms for each variable series](Figures/figure1_histograms.png)

Looking at means and medians of Table 4 for samples of control group (M4CID = 0) and treatment group (M4CID=1)  shows that, in overall the impact is negative. However, because of differences in the schools, we will see that the impact is very positive for Lab Practice and negative for the classroom behavior and as no effect on learning transfer and academic results. 

| M4CID 0 | Obs. | Mean | Median | Min.| Max. | St. Deviation | Notes |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|---|
| TEST | 828 | 62 | 62 | 16 | 100 | 17  |  |
| LAB | 828 | 57 | 60 | 10 | 100 | 20  |  | 
| BEHAV | 828 | 77 | 79 | 28 | 100 | 14 | |
| CLASS | 828 | 64 | 64 | 14 | 97 | 14 | |

| M4CID 1 | Obs. | Mean | Median | Min.| Max. | St. Deviation | Notes |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|---|
| TEST | 586 | 58 | 57 | 0 | 100 | 17 |  |
| LAB | 586 | 64 | 65 | 15 | 100 | 16  |  | 
| BEHAV | 586 | 74 | 76 | 27 | 100 | 16 | |
| CLASS | 586 | 63 | 62 | 18 | 100 | 14 | |

Table 4: data from the treatment and control group

Figure 2 shows the variable longitudinal patterns for all sample using box plots time series, which includes junior and junior high students data for both schools. The red vertical line shows the school change and the blue one is the starting point for the implementation of 4C/ID strategic inductive methodology for the treatment group. Its clear that near each vertical line there is a change showed by the blue smooth line (polynomial local regression). So, something happened! 

![Figure 2: Boxplot time series](Figures/Figure2_BoxplotTimeSeries.png)

# METHODS

The **statistical analysis** methods uses R code software in the RStudio[^readme-4] IDE release and the principal methodology used is the non-parametric inferential treatment effect, but there is a discussion towards the non-parametric regression inference, regression discontinuity design and clustering. this last ones just to try to corroborate the upper first results. 

[^readme-4]: RStudio, <https://posit.co/products/open-source/rstudio/>

[^readme-5]: there was a need to make some analysis of this data sample group in order to conclude if there is an underlying school effect: in fact there is a difference confirmed with 95% confidence under the null. <https://github.com/fqantonio/GoingInductive/tree/main/DATA>

# RESULTS

Figure 3 is a resume of the **results**. In the y-axis it shows the name of the samples and the x-axis shows the median differences for each sample in four variables already described: TEST, LAB, BEHAV and CLASS. The variables change, represented by colors (see legend), between treatment group and control group are presented on the left of the graph if there is a negative impact and in the right if there is a positive 4C/ID impact. 

????There is a vertical line with x equal to zero, meaning that there is no 4C/ID impact.

Looking into the graph there are two groups of results that stands: blue and green. The first means that there is a positive treatment effect for the Lab Practice and the latter means that there is a overall negative impact for the social skills: in this case only the GRADE 9 students seems to have a positive impact and no effect for FEMALE students. The TEST variable shows a tendency towards no impact with a few positive and negative. For the CLASS, Academic Results there a tendecy towards no effect to a slight negative one.

Looking into the sample JUNIOR SCHOOL 1 (sample with grades 7, 8 and 9 for school 1) that seems to be more reliable for three reasons:most **robust and reliable sample** used in this work: in fact the sample is much larger. So, they show results about the JUNIOR sample for SCHOOL 1, which means:

  1. Not having the school effect differences;
  2. The sample has students with the same age;
  3. It doesn't have the problem of using the less numbered entrances of the junior high students sample.

# CONCLUSION

The conclusions presented here focus more on the more reliable and stable sample: junior sample of school 1. So, **Overall**, as a teacher of science and tech, you should (or at least think about it) move to implement the inductive strategy as your methodology for learning because it improves the **Learning Transfer**, **academic results** with a huge impact on **lab practice**, except for the **Social Skills** [^readme-8].

However, there are **risks and concerns** to be accommodated during the process, namely:
1. for the student less adapted to the school system;
2. the classroom management referring to students behavior (see discussion);
3. in the **learning transfer** process the gains are more modest;
4. males gain less with this change.

[^readme-8]: Is there an underlying behavioral underlying effect?

**Bottom line**, if you don't want to take the risks, change conditional to have a student group more adapted to school or if you want to increase the lab practices skills. 

Futhermore, this work shows is that there is a slight advantage of using 4C/ID eLearning setting compare to Direct Instruction. In fact, there is a huge advantage of 4C/ID in Lab Practice in spite of the negative impact in Social Skills. It also shows that the students that are less adapted to school don't improve much unless for the Lab Practice. So, compare with a instructional design with 200 years (???) of reliability is something to higlight.

# DISCUSSION

notes for discussion: - The conclusions meet the goals and answer th research question? - Did I implemented the methods correctly? - Is there an underlying effect for the behav variable? - Conclusions for the stakeholders in the learning process: teachers, Policy implications and school management. - blearning change: team work impressible - Reiterate results - world research corroboration ; Future research Tie loose hands: Clarify, robustness; after 2015 there is a crash in the student behavior skills.: need extra data; social skills: all, ranks, grades, and gender follow the same pattern? students behavior effect

  ## NOTES and OBSERVATIONS
  ## Policy implications
  ## Future  arch
  ## Tie loose hands together
  # DOUBTS AND DISCUSSIONS
  ########## doubts and discussions
# 802!?!?!? - scale change
#Kendall correlation test, sometimes because of the random sample it gives results of depency...
#what to ? a for cycle and calcula probability of getting independency?
#rank 1, kendall ties!?

# NOTES
