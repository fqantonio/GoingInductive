# WELCOME

This is a **`Going Inductive`** paper DRAFT roadmap[^readme-1], with one major goal, to share, worldwide, the work done. This **knowledge base source** contains the results, conclusions, procedures, exploratory data analysis (EDA), data (and raw data), statistical methods, educational methodologies, learning flow and bLearning practice. Hopefully, there will be, at least, one paper based on this.

[^readme-1]: This work, as well as the data, is also presented at the blog "<https://4cidchange.edublogs.org/>" and in the github repository <https://github.com/fqantonio/GoingInductive>.

>
> Education is the most powerful weapon which you can use to change the world. 
>
>                                                 Nelson Mandela


# ABSTRACT

Should you, as a teacher of science and tech of young students, **move to an inductive strategy**, for the learning design flow with all the activities chronologically organized, even if the setting is bLearning? In overall, the **answer is yes**, that is, 4C/ID eLearning setting has some advantages over Direct Instruction but with some remarks. 

If you decide to do it, there will be specific impacts that need to be **accommodated**, as well as implications for the stakeholders in the learning process: policy makers and school management. 

The sample covers 12 years of teacher data assessment, during the period 2003 to 2017, with conclusions about the impact in the Learning Transfer, Lab Practice, Social Skills and Academic Results. 

**Keywords** Inductive learning strategy; 4C/ID; Direct Instruction; Learning Flow; Statistical Non-parametric Inference; academic results; learning transfer; behavior; lab practice; Cognitive Load Theory; Multi Intelligence Theory; Brain Based Learning; bLearning.

# GOALS, RESEARCH QUESTIONS and HYPOTHESIS

The **main goal** is to understand the impact of changing from Merriënboer's Inductive Methodology Four Component Instructional (4C/ID)y (van Merriënboer, Kirschner, 2007) in a bLearning setting to Direct Instruction (Merrill, 2007) with this **research questions** as guidelines for the data analysis in four aspects, Learning Transfer, Lab Practice, Social Skills and Academic Results: what impact for this students learning outcomes? Who benefits more? What are the pros and cons about this methodological decision? What are the implications for the stakeholders?

The **hypothesis** is that this change will have a positive impact in all of the the learning outcomes mentioned, because, it is based on the 4C/ID methodology that, in itself, is anchored on Brain Based Learning (BBL), Cognitive Load Theory (CLT), Multi-Intelligence Theory (MIT) and Multimedia Learning (ML). For the stakeholders it will give some guidelines for better support of the teaching work, not only in efficiency but also to improve the learning strategies and team work.

# CONTEXT and DATA

This work is based on real day-to-day teacher assessment data, for junior and junior high students of two different schools, from different regions and time periods, for the chemistry and physics subjects of the Portuguese curriculum. This data was gathered over 12 years, in the period 2003 to 2017, and includes students **Learning Transfer**, **Lab Practice**, **Classroom Behavior** (including Social Skills), and **Academic Results** in four different variables: TEST, LAB, BEHAV and CLASS.

The motivation lies on the need to understand the impact of the a professional strategic decision within the educational work developed (flow charts, [^readme-2]): was it worth it? Did students benefit? What can be done better? What conclusions can be drawned? 

Most of the time the professional, specific, **day-to-day teacher work** data (see figure 2) is rarely or never investigated, at least, in the Portuguese educational environment. For the teachers, there is little or no time to look back and work through the data results, the qualitative remarks, or through the statistics. And, as a consequence, no robust conclusions are devised and worse, it's normal not to share it.

[^readme-2]: <https://cmap.ihmc.us/>

Samples are independent and, in general, non-normal: some few sample for the CLASS and TEST variables where normal distributed.

Figure 1 shows the variable longitudinal patterns for all sample using box plots time series, which includes junior and junior high students data for both schools. The red vertical line shows the school change and the blue one is the starting point for the implementation of 4C/ID strategic inductive methodology for the treatment group. Its clear that near each vertical line there is a change showed by the blue smooth line (polynomial local regression). So, something happened! 

![Figure 1: Boxplot time series](Images/BoxplotTimeSeries.png)

# METHODS

The **statistical analysis** uses R code software in the RStudio[^readme-3] IDE release and the principal method is the non-parametric inferential treatment group effect with sign-rank Wilcoxon procedure. There is also a discussion towards the non-parametric regression inference, regression discontinuity design and clustering, just to have same feedback from other types of analysis that could show some incoherence on the inference analysis. The methods used take in account the samples independency and non-normality distribution.

[^readme-3]: RStudio, <https://posit.co/products/open-source/rstudio/>

# RESULTS

Figure 2 is a resume of the **results** for all sample including the school effect identified. In the y-axis it shows the name of the samples and the x-axis the median differences for each sample in four variables: TEST, LAB, BEHAV and CLASS. The variables change, represented by colors (see legend), between treatment group and control group are presented on the left of the graph if there is a negative impact and in the right if there is a positive 4C/ID impact. Color vertical lines of each variable means that there is no change, that is, no 4C/ID impact.

![Figure 2: Resume results](Images/ResumeResults_allSample.png)

There are same patterns on figure 2 that show that sommething happen around 2012, the first year of 4C/ID implementation.

# CONCLUSION

The conclusions presented here focus more on the less reliable and stable sample: all sample from both schools with school effect included. So, **Overall**, as a teacher of science and tech, you should (or at least think about it) move to implement the inductive strategy as your methodology for learning because it improves the **Learning Transfer**, **academic results** with a huge impact on **lab practice**, except for the **Social Skills** [^readme-4] and RANK 1 students.

However, there are **risks and concerns** to be accommodated during the process, namely:
1. for the student less adapted (RANK 1) to the school system;
2. the classroom management referring to students behavior (see discussion);
3. in the **learning transfer** process the gains are more modest;
4. males gain less with this change.

[^readme-4]: Is there an underlying behavioral underlying effect?

**Bottom line**, if you don't want to take the risks, change conditionaly to have a student group more adapted to school or if you want to increase the lab practices skills in some subject or school context. 

Furthermore, we've to considered that the comparison is to an well developed and researched  instructional design over more then 200 years. Any slight improve is strong. Another thing that has to be acknowledge by the **stakeholders** is the fact that this change is overwhelming for a teacher alone: there is a crucial need to a commited team work. Also, one methodology is not a panacea for all of the learning process, there is a need of stimulating research and knowledge spread of the specific impacts of each strategy. In this work only two were considered.

Further research should include the why of the behavior declining over 2016 and develop the same research in other educational context national and international in order to confirm this findings. 

# DISCUSSION

What strikes in a teacher practice are the doubts: did I implement the strategies correctly? Are this conclusions valuable? Are this statistical results robust, reliable and powerfull?

# NOTES
