# WELCOME

This is a **`Going Inductive`** paper DRAFT roadmap, not only to address the research question but also to share the work done: a **knowledge base source**, with the results and conclusions, procedures, exploratory data analysis (EDA), data (and raw data), statistical methods, educational methodologies, learning flux and blearning practice. Hopefully, there will be, at least, one paper based on this.

# ABSTRACT

Should you, as a teacher of science and tech, **move to an inductive strategy**, for the design of the learning activities, that you prepare and organize, even if you include bLearning? In overall, the **answer is yes**, but with some crucial remarks. If you decide to do it, there will be consequences for the students, different for different categories, that need to be **accommodated**. The question addressed here has direct consequences usefull for the stakeholders in the learning process: students, teachers, policy makers and school management. 

The work sequence starts with the exploratory data analysis (EDA); next, there is an presentation of the non-parametric inference treatment effect results, regression analysis, a regression discontinuity design, cluster analysis and it ends with the results, conclusions, some remarks about the findings and future research.

**Keywords**
Inductive learning strategy; 4C/ID; Direct Instruction; Learning Flux; Statistical Non-parametric Inference; academic results; learning transfer; behavior; lab practice;

# INTRODUCTION

Should you, as a teacher of science and tech, **move to an inductive strategy**, for the design of learning activities that you prepare and organize, even if you include bLearning? In my opinion, and based on the results, you should, but with some specific remarks that are discussed and explained at the conclusion.

The **conclusions** of this work are very specific because of the longitudinal sample used: Junior and junior high for the physics and chemistry content of the Portuguese school curriculum. They are drawn from 13 years of data, in the period from 2003 to 2019, both qualitative and quantitative, from actual teacher day-to-day practice learning strategies: deductive and inductive. The transition made over this period of time, is highlighted and the learning flux elaborated for the integration also of bLearning environments.

The **statistical analysis** uses R Code (RStudio) and starts with a exploratory data analysis (EDA): a descriptive summary of the quantitative data. It goes into a discussion of the data structure, normality and sample independence, and it ends with an explanation about the statistical methodology, results and conclusions. 

The statistical methodology used is the usual treatment effect inferencial non-parametric, regression non-parametric inference, regression discontinuity design and clustering. For this, it was used 3 learning outcomes: transfer learning, laboratory practice and social skills, addressed respectively by 3 variables, TEST, LAB and BEHAV. They were assessed by paper/online tests and in classroom observational paper register forms.

There is also a discussion about the **educational methodologies** used, from Direct Instruction, bLearning to 4C/ID (four component instructional design). The concept map's that helped to elaborate the learning flux for this learning environmental process use cmaptools [^???] are presented along with the workflow. The respective inductive methodology that supported the educational worlflow was designed with merrionboer´s 4C/ID [^3] map structure. 

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

The next two tables present the **results**, only for the most **robust and reliable sample** used in this work, in spite of the fact that the sample is much large. So, they showed results about the JUNIOR sample for SCHOOL 1, which means not having the school effect, nor the problem of using the less numbered entrances of the junior high students sample. 

CLASS is an weight average of the **variables**, TEST, LAB and BEHAV with formula: CLASS = 0.5 x TEST + 0.3 x LAB + 0.2 x BEHAV. The RANK variable is categorized using the variable CLASS and is a measured of the level of adaptation of students to the school system: RANK 1, for CLASS less or equal to 45%; RANK 2, for CLASS between 45% and 70% and RANK 3, for CLASS greater or equal to 70%. MALE and FEMALE are variables related to the gender of students: categorized by F if students are female gender and M if male. 

The values showned in the table are median changes in the variable group.

| SAMPLE | TEST | LAB | BEHAV | CLASS | OBSERVATIONS/NOTES |
|:------------|:------------:|:------------:|:------------:|:------------:|:------------:|
| *Junior for school 1*             | \+ (54 to 58)  | \+ (53 to 66)  | \- (81 to 74)    | \+ (59 to 62)    |   Median values presented in %               |
| GRADE 7                          | \~   | \+ (56 to 64)  | \- (77 to 71)   | \~    |                   |
| GRADE 8                         | \+ (54 to 58)   | \+ (55 to 68)  | \- (85 to 60)   | \~    | M4CID=="1", 71 entrances, less then 100 entrances|
| GRADE 9                         | \~   | \+ (44 to 66)  | \+ (80 to 85)   | \+ (55 to 64)   |          |
| RANK 1                          | \- (34 to 29)  | \+ (26 to 35) | \- (65 to 50)    | \- (39 to 37)    |           |
| RANK 2                          | \~   | \+ (51 to 62) | \- (80 to 67)    | \~    |            |
| RANK 3                          | \~   | \+  (73 to 80) | \- (94 to 90)   | \+ (77 to 81)    |            |
| FEMALE                          | \+ (54 to 56)   | \+ (52 to 70)  | \~    | \+ (59 to 67)    |          |
| MALE                            | \~   | \+ (53 to 62) | \- (81 to 66)    | \~    |          |

The next table resumes the former table in another order: this next one shows the percentage of "positive", "negative" and "no effects", for each sample.

2/9

| Variable | + | ~ | -| SAMPLE + | no effect sample | SAMPLE - | Notes | 
|---|:---:|:---:|:---:|:---:|:---:|:---:|---|
| TEST (Learning Transfer) | 33% | 56% | 11% | All Junior sample, 8th grade and FEMALE | GRADE 7, 9; | RANK1 ||
| LAB (Lab practice) |100%|0%|0%||all samples|||
|BEHAV (behavior)|11%|11%|78%|GRADE 9|FEMALE|Junior; GRADE 7 and 8; all RANK's; MALE ||
|CLASS (academic results) |22%|45%|33%|junior; 9th grade; FEMALE|grades 7 and 8; RANK 2 e MALE|RANK 1 and 3 ||

# CONCLUSION

(need more editing ;) )
  
The conclusions presented here focus on the junior sample of school 1 (table above). 

**Overall**, as a teacher of science and tech, you should move to implement the inductive strategy as your methodology for learning because it improves the **academic results** with a huge impact on **lab practice**.

However there are risks and concerns to be accommodated during the process, namely: 

  1. the less adapted to the school system;
  2. in the **behavior** aspect of the classroom managment and in the **learning transfer**;
  3. in less degree of concern, Grade 7 and males seems to benefit less from this change.

**Bottom of line**, if you don't want to have the less risks as possible, change if you have a student group more adapted to school and if you want to increase the lab practices skills.

RISKS:
  1. In the **learning transfer** and **academic results**, there is the risk that the less adapted to the school environment suffer more with this strategic change;
  2. For **behavior**, specifically in aspect of following social and behavioral classroom rules, there is a major negative [^???] impact in all samples: the risk of loosing control of the students group inside the classroom is real, but in this case, the assessment were all positive: median change from 81 to 74%, which means that the behavior was above 50% and didn't represent a thread to the classroom learning environment;

CONCERNS:
  1. There seems to be a positive effect only in GRADE 8 and FEMALE groups for the **learning transfer**, the other groups the change don't have any effect. 
  2. On the **behavior field** only grade 9 seems to get some positive effect;
  3. In general, Female gender group tend to benefit more them male;
  4. In general, RANK 3 tends to benefit more then the other rank groups;
  5. grade 9th benefit more then the other grades;
  
BENEFITS:
  1. In overall, the change has a positive impact on junior grade students for the **academic results**; 
  2. **Lab practice** has a large positive effect in all samples;

# DISCUSSION 

Reiterate results
Policy implications
Future research
Tie loose hands???
world research corroboration???
Clarify
robustness

# KEYWORDS

Inductive learning strategy - the learning flux starts by facts, experiences and real problems and move towards knowledge and a conceptual mental model; 
4C/ID - instructional design approach for complex learning. It provides guidelines for the analysis of real-life tasks and the transition into a blueprint for an educational program.
Direct Instruction - Principles of Instruction that prescribe a cycle of instruction consisting of activation, demonstration, application, and integration ([^???]Merril, 2007);
Learning Flux - longitudinal (temporal) map with sequenced activities organized in time, by complexity and logical learning coherence; 
Statistical Non-parametric Inference -  statistical techniques that use data to test if two sample came from the same non-normality distribution; 
Learning transfer - the ability of pass information to the students measured by tests (online or paper supported), directly related to the TEST variable
Lab Practice - Laboratory activities directly related to the variable LAB
Behavior - Ability to follow social and behavioral classroom rules directly related to the BEHAV variable.
Academic results - weight average variable directly related to the other variables: TEST, LAB and BEHAV.

# REFERENCES

[^1] https://github.com/fqantonio/GoingInductive
[^2] https://cmap.ihmc.us/
[^3] https://www.4cid.org/
[^???] there is a need to make some anlysis of this data because it seems that there is an underlying effect
[^tt] - Journal of Research on Technology in Education, 2007, 40(1), A Task-Centered Instructional Strategy, M. David Merrill
[^???] https://www.4cid.org/publications/
