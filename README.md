# WELCOME

This is a **`Going Inductive`** paper DRAFT roadmap, not only to address the research question but also to share the work done: a **knowledge base source**, with the results and conclusions, procedures, exploratory data analysis (EDA), data (and raw data), statistical methods, educational methodologies, learning flux and blearning practice. Hopefully, there will be, at least, one paper based on this. There is also a **wiki** where the comments and R code is presented.

# ABSTRACT

Should you, as a teacher of science and tech, **move to an inductive strategy**, for the design of the learning activities, that you prepare and organize, even if you include bLearning? In overall, the **answer is yes**, but with some crucial remarks. If you decide to do it, there will be consequences for the students, different for different categories, that need to be **accommodated**. The question addressed here has also consequences all of the stakeholders in the learning process: students, teachers, policy makers and school management. The sequence starts in the exploratory data analysis (EDA), includes the inference treatment effect, regression analysis and a regression discontinuity design, and ends with the results and conclusions and some remarks about the findings and future research.

**Keywords**
Inductive learning strategy; 4C/ID; Direct Instruction; Learning Flux; Concept Maps; Statistical Non-parametric Inference; Quantitative and Qualitative analysis.

# INTRODUCTION

Should you, as a teacher of science and tech, **move to an inductive strategy**, for the design of the learning activities that you prepare and organize, even if you include bLearning? In my opinion, and based on the results, you should, but with some specific remarks that are explained at the conclusion, below and in the document 

The **conclusions** of this work are very specific because of the longitudinal sample used: Junior and junior high for the physics and chemistry content of the Portuguese curriculum. They are drawn from 16 years of data (2003-2019), both qualitative and quantitative, from actual teacher day-to-day practice learning strategies: deductive and inductive. The transition, over this period of time, is highlighted and the learning flux elaborated for the integration of bLearning environments is also presented.

The **statistical analysis** uses R Code (RStudio) and starts with a exploratory data analysis (EDA): a descriptive summary of the quantitative and qualitative data. It goes into a discussion of the data structure, normality and sample independence, and it ends with an explanation about the statistical methodology, results and conclusions. The statistical methodology used is the usual treatment effect inferencial non-parametric, regression non-parametric inference, regression discontinuity design and clustering. For this, it was used 3 learning outcomes: transfer learning, laboratory practice and social skills, represented respectively by 3 variables, TEST, LAB and BEHAV. They were assessed by paper/online tests and in classroom observational paper forms.

There is also a discussion about the **educational methodologies** used, from Direct Instruction, bLearning to 4C/ID (four component instructional design). The concept map's that helped to elaborate the learning flux for this learning environmental process are presented along with the workflow. The respective inductive methodology that supported the educational worlflow was designed with cmaptools [^2] and used merrionboer´s 4C/ID [^3] map structure. 

This work, as well as the data, is also presented at the blog “https://4cidchange.edublogs.org/”.

# GOALS and RESEARCH QUESTIONS

The **main goal** is to understand the impact of the use of Inductive Methodology 4C/ID, by answering the following the **research questions**: 
  1 - changing the learning methodologies from one mainly deductive (Direct Instruction[^1]) to another one mostly inductive (4C/ID [^2]), means what for your students? Who benefits? What are the pros and cons about this methodological decision?
  2 - Should you, as a teacher of science and tech, move to implement the inductive strategy as your methodology for learning? 
  3 - And if you do, what are the outcomes? What are the risks? What were the consequences for the student's academic results? 
  4 - Is there a different impact between female and male? 
  5 - And within the different grades?
  6 - Student more adapted to school have benefit more with the use of 4CID methodology?

# MOTIVATION

Most of the time the professional, specific, **day-to-day teacher work** gathered data is rarely or never investigated, at least, in the Portuguese educational environment. For the teachers, there is not much time to look back and work through the data results, the qualitative remarks, or through the statistics. And, as a consequence, no robust conclusions are possible. Worse, even if someone did it, it's normal to not share it. Research and discussion with peers are crucial to better adjust our decisions regarding the way students learn today. The way learning happens and motivation changes, is so fast! So, teaching, at least in Portugal, is a very solitary job with all of its directly related drawbacks.

My **personal goal** was to know if the strategic decisions I did in 2008 were positive for my students: did they benefit from the educational strategic change? It was a global benefit or some groups benefited more? Does anyone have the same kind of research? Can anyone corroborate this results or criticize them? Sharing is the way to go in order to exchange results an find knowledge.

# CONTEXT

This work includes junior and junior high students of two different schools, from different regions, at the chemistry and physics subjects of the Portuguese curriculum, in the time scale 2003 to 2019.

The organizational environment of schools and teachers in Portugal doesn't help this type of research work. Focused on managing the schools as enterprises with a lot of controlling bureaucracy, putting numbers and paperwork ahead of the crucial and significant decisions around teaching, real assessment is barely used, research also, and when it happens, its just shared at the university level. Meaning:
  1 - The crucial planning and individual teacher-student interaction is less important for this organization;
  2 - Time spent on research and discussions is useless: numbers constrain our job, of course, it's normal, but with the same money we can do better if we can get together and discuss the way we could be organized.

Luckily, the knowledge and the tools available online today, allows us to go further with peer contribution, beyond a simple Average Treatment Effect (some of us, at least, do some mean computations): statistical knowledge is not something that we are prepared for and the statistical tools are very expensive. Today, this tools are available with opensource licences: so, time is crucial: teacher need it to study, get the necessary knowledge and interpret the results. And publish it!

# RESULT

The next two tables present the **results**, only for the most **reliable sample** used in this work, in spite of the fact that the sample is much large. So, they showed results about the JUNIOR sample for SCHOOL 1, which means not having the school effect nor the problem of using the less numbered entrances of the junior high students sample. CLASS is an weight average of the **variables**, TEST, LAB and BEHAV with formula: CLASS = 0.5 x TEST + 0.3 x LAB + 0.2 x BEHAV. The RANK variable is categorized using the variable CLASS and is a measured of the level of adaptation of students to the school system: RANK 1, for CLASS less or equal to 45%; RANK 2, for CLASS between 45% and 70% and RANK 3, for CLASS greater or equal to 70%. MALE and FEMALE are variables related to the gender of students: categorized by F if students are female gender and M if male.

| SAMPLE | TEST | LAB | BEHAV | CLASS | OBSERVATIONS/NOTES |
|:------------|:------------:|:------------:|:------------:|:------------:|:------------:|
| *Junior for school 1*             | \+   | \+  | \-    | \+    |                  |
| GRADE 7                          | \~   | \+  | \-    | \~    |                   |
| GRADE 8                         | \+   | \+  | \-    | \~    | M4CID=="1", 71 entrances, less then 100 entrances|
| GRADE 9                         | \~   | \+  | \+    | \+    |          |
| RANK 1                          | \-   | \+  | \-    | \-    |           |
| RANK 2                          | \~   | \+  | \-    | \~    |            |
| RANK 3                          | \~   | \+  | \-    | \-    |            |
| FEMALE                          | \+   | \+  | \~    | \+    |          |
| MALE                            | \~   | \+  | \-    | \~    |          |

| Variable | + | ~ | -| SAMPLE + | no effect sample | SAMPLE - | Notes | 
|---|:---:|:---:|:---:|:---:|:---:|:---:|---|
| TEST (Learning Transfer) | 22% | 56% | 0% | All Junior sample, 8th grade and FEMALE | GRADE 7, 9; | RANK1 ||
| LAB (Lab practice) |100%|0%|0%||all samples|||
|BEHAV (Following social and behavioral classroom rules)|11%|11%|78%|GRADE 9|FEMALE|Junior; GRADE 7 and 8; all RANKs; MALE ||
|CLASS (weight average learning outcome) |33%|44%|22%|junior; 9th grade; FEMALE|grades 7 and 8; RANK 2 e MALE|RANK 1 and 3 ||

# CONCLUSION

(need more editing and discussion)
  
The conclusions presented here are using what I considered the most reliable sample (table above). For more, please read the conclusion file.

**Overall**, as a teacher of science and tech, you should move to implement the inductive strategy as your methodology for learning but... 
  1. there is a need accommodate the probability of a large negative effect in the behavior context, that is, following social and behavioral classroom rules. 
  2. along with the fact that the **9th grade** seems to benefit more from this treatment effect, students **more adapted** to the school and **females** benefit more;
  3. the **lab practice** is what students benefit more with this strategic change;
  4. The **risks** of this change are in the loosing control of the students group inside the classroom but in this case, in spite of decreasing, they were all positive;
  5. The **academic results** decrease in the case of the less adapted students and females benefit more;
  6. 9th grade benefit more.

# DISCUSSION 

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
