# Welcome
Should you, as a teacher of science and tech, **move to an inductive strategy**, for the design of the learning activities that you prepare and organize, even if you include bLearning?

This is the **`Going Inductive`** paper DRAFT roadmap! This repository [^1] is used for documenting, managing and making available the outcame **knowledge base source** about the work done so far in order to answer the former research question. Here is shared, not only the results and conclusions, but also procedures, exploratory data analysis (EDA), data (and raw data), statistical methods, educational methodologies, learning flux and blearning. Hopefully, there will be, at least, one paper based on this. There is also a **wiki** where the comments and R code is presented.

# Abstract
Should you, as a teacher of science and tech, **move to an inductive strategy**, for the design of the learning activities that you prepare and organize, even if you include bLearning? In overall, the **answer is yes**, but with some crucial remarks. If you decide to do it, there will be consequences for the students in different categories that need to be **accommodated**. The question addressed here has also consequences for the stakeholders in the learning process: students, teachers, policy and school management implications. After the exploratory data analysis (EDA), this work used inference treatment effect, regression analysis and a regression discontinuity design for the results and subsquently conclusions.

**Keywords**
Inductive strategy; 4C/ID; Direct Instruction; Learning Flux; concept maps; Statistical inference; Quantitative and Qualitative analysis.

#INTRODUCTION
Should you, as a teacher of science and tech, **move to an inductive strategy**, for the design of the learning activities that you prepare and organize, even if you include bLearning? In my opinion, and based on the results, you should, but with some specific remarks that are explained at the conclusion, below and in the document **CONCLUSION**.

The data was gathered along 16 years of teacher practice and reflection. The conclusions are very specific because of the longitudinal sample used: Junior and junior high for the physics and chemistry content of the Portuguese curriculum. 

The **analysis work flow** uses R Code (RStudio) and starts with a exploratory data analysis (EDA), a descriptive summary of the quantitative and qualitative data. It goes into a discussion of the data structure, normality and sample independence and ends with an explanation about the methodology used, results and conclusions. The statistical methodology used is non-parametric inferencial and clustering. 

There is also a discussion about the **educational methodologies** used, from Direct Instruction, bLearning to 4C/ID, four component instructional design. It is presented the concept map's that helped to elaborate the learning flux for the learning environments. The inductive methodology that supported the educational worlflow was designed with cmaptools [^2] and used merrionboer´s 4C/ID [^3] structure. 

The **conclusions** described in this work are drawn from 16 years of data (2003-2019), both qualitative and quantitative, from actual teacher day-to-day practice learning strategies: deductive and inductive. This transition, over this period of time, is highlighted and the learning flux elaborated for the integration of bLearning environments is also presented. 

The **analysis** was made based in 3 learning outcomes: transfer tests, behavior and social skills registered in specific forms. The results were based on the usual inference statistical and qualitative analysis. This work, as well as the data, is also presented at the blog “https://4cidchange.edublogs.org/”.

# GOAL (Research questions)

The **main goal** is to understand the impact of the use of Inductive Methodology 4C/ID by answering the questions: 
  1 - changing the learning methodologies from one mainly deductive (Direct Instruction[^1]) to another one mostly inductive (4C/ID [^2]), means what for your students? Who benefits? What are the pros and cons about this methodological decision?
  2 - Should you, as a teacher of science and tech, move to implement the inductive strategy as your methodology for learning? 
  3 - And if you do, what are the outcomes? What are the risks? What were the consequences for the student's academic results? 
  4 - Is there a different impact between female and male? 
  5 - And within the different grades?
  6 - Student more adapted to school have benefit more with the use of 4CID methodology?

# MOTIVATION

Most of the time the professional, specific, day-to-day teacher work gathered data is rarely or never investigated. At least in the portuguese educational environment. For the teachers, there is not much time to look back and work through the data results, the qualitative remarks, or through the statistics. And, as a consequence, no robust conclusions are possible. Worse, even if someone did it, it's normal to not share it. Research and discussion with peers are crucial to better adjust our decisions regarding the way students learn today. The way learning happens and motivation changes, are so fast! So, teaching, at least in Portugal, is a very solitary job with all of its directly related drawbacks.

My personal goal was to know if the strategic decisions I did in 2008 were positive for my students. Well, was it? The questions are at least two: 

1. Did my students benefit from the educational strategic change? It was a global benefit or some groups benefited more.
2. To share this conclusion in order to exchange knowledge with others around the world, and see if they corroborate with this findings.

# CONTEXT
On the other end, the organizational environment of schools and teachers in Portugal doesn't help either. Focused on managing the schools as enterprises with a lot of controlling bureaucracy, putting numbers and paperwork ahead of the crucial and significant decisions around teaching, real assessment is barely used, and when it is, its just at the university level. This means:
  1 - The crucial planning and individual teacher-student interaction is less important for this organization;
  2 - Time spent on research and discussions is useless: numbers constrain our job, of course, it's normal, but with the same money we can do better if we can get together and discuss the way we could be organized.

Luckily, the knowledge and the tools available online today, permit that we can go further with peer contribution and go beyond a simple Average Treatment Effect (some of us, at least do some mean computations): statistical knowledge is not something that we are prepared for and the statistical tools are very expensive. Now this tools are available in opensource licences: so, time is crucial: teacher need it to study, get the necessary knowledge and interpret the results. And publish it.

# MAJOR RESULT
The next two tables present the results for the most reliable sample used in this work. This is because it only uses JUNIOR sample for SCHOOL 1, which means not having the school effect nor the problem of using the less numbered entrances of the junior high sample. 

| SAMPLE | TEST | LAB | BEHAV | CLASS | OBSERVATIONS/NOTES |
|:------------|:------------:|:------------:|:------------:|:------------:|:------------:|
| *Junior for school 1*             | \+   | \+  | \-    | \+    | AFTER this road sample is junior grade for SCHOOL 1                                                                        |
| GRADE 7                          | \~   | \+  | \-    | \~    |                                                                        |
| GRADE 8                         | \+   | \+  | \-    | \~    | M4CID=="1",#71 entrances, less then 100 entrances                      |
| GRADE 9                         | \~   | \+  | \+    | \+    |                                                                        |
| RANK 1                          | \-   | \+  | \-    | \-    |                                                                        |
| RANK 2                          | \~   | \+  | \-    | \~    |                                                                        |
| RANK 3                          | \~   | \+  | \-    | \-    |                                                                        |
| FEMALE                          | \+   | \+  | \~    | \+    |                                                                        |
| MALE                            | \~   | \+  | \-    | \~    |              |

| Variable | + | ~ | -| SAMPLE + | no effect sample | sample - | Notes | 
| TEST (Learning Transfer) | 22% | 56% | 0% | All Junior sample, 8th grade and FEMALE |GRADE 7, 9; |RANK1||
| LAB (Lab practice) |100%|0%|0%||all samples||||
|BEHAV (Following social and behavioral classroom rules)|11%|11%|78%|GRADE 9|FEMALE|Junior; GRADE 7 and 8; all RANKs; MALE ||
|CLASS (weight average learning outcome) |33%|44%|22%|junior; 9th grade; FEMALE|grades 7 and 8; RANK 2 e MALE|RANK 1 and 3 ||

# Conclusion (need more editing and discussion)
  
The conclusions presented here are using what I considered the most reliable sample (table above). For more, please read the conclusion file.

**Overall**, as a teacher of science and tech, you should move to implement the inductive strategy as your methodology for learning but... 
  1. there is a need accommodate the probability of a large negative effect in the behavior context, that is, following social and behavioral classroom rules. 
  2. along with the fact that the **9th grade** seems to benefit more from this treatment effect, students **more adapted** to the school and **females** benefit more;
  3. the **lab practice** is what students benefit more with this strategic change;
  4. The **risks** of this change are in the loosing control of the students group inside the classroom but in this case, in spite of decreasing, they were all positive;
  5. The **academic results** decrease in the case of the less adapted students and females benefit more;
  6. 9th grade benefit more.

Reiterate results
Policy implications
Future research
Tie loose hands???
world research corroboration???

Clarify
robustness

[^1] https://github.com/fqantonio/GoingInductive
[^2] https://cmap.ihmc.us/
[^3] https://www.4cid.org/
