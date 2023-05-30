# WELCOME

This is a **`Going Inductive`** paper DRAFT roadmap[^readme-1], with one major goal, to share the work done, worldwide. This **knowledge base source** contains the results, conclusions, procedures, exploratory data analysis (EDA), data (and raw data), statistical methods, educational methodologies, learning flow and bLearning practice. Hopefully, there will be, at least, one paper based on this.

[^readme-1]: This work, as well as the data, is also presented at the blog "<https://4cidchange.edublogs.org/>" and in the github repository <https://github.com/fqantonio/GoingInductive>.

# ABSTRACT

Should you, as a teacher of science and tech of young students, **move to an inductive strategy**, for the learning design flow with all the activities chronologically organized, even if the setting is a bLearning one? In overall, the **answer is yes**, that is, 4C/ID eLearning setting as some advantages to Direct Instruction but with some remarks. If you decide to do it, there will be specific impacts for the students learning that need to be **accommodated**, as well as implications for the stakeholders in the learning process: policy makers and school management. The sample covers 13 years of teacher data assessment, during the period 2003 to 2019, with conclusions about the impact in the Learning Transfer, Lab Practice, Social Skills and Academic Results.

> Education is the most powerful weapon which you can use to change the world. 
>
>                                                 Nelson Mandela

**Keywords** Inductive learning strategy; 4C/ID; Direct Instruction; Learning Flow; Statistical Non-parametric Inference; academic results; learning transfer; behavior; lab practice; Cognitive Load Theory; Multi Intelligence Theory; Brain Based Learning; bLearning.

# GOALS, RESEARCH QUESTIONS and HYPHOTESIS

The **main goal** is to understand the impact of the use of Inductive Methodology four component instructional (4C/ID) in a bLearning setting, by answering the following **research questions**:

1 - changing the learning methodologies from one mainly deductive, denominated by Direct Instruction (Merrill, 2007) to another one mostly inductive, Merriënboer's 4C/ID design theory (van Merriënboer, Kirschner, 2007), means what for students learning outcomes? Who benefits? What are the pros and cons about this methodological decision? What are the implications for the stakeholders?

2 - Should you, as a teacher of science and tech, move to implement the inductive strategy as your methodology for learning?

3 - And if you do, what are the outcomes? What are the risks? What were the consequences for the student's academic results?

4 - Is there a different impact on gender?

5 - And within the different grades?

6 - Student more adapted to school benefit more?

The **hypothesis** is that this change will have a positive impact in all learning areas because, in overall, it is based on the 4C/ID methodology that, in itself, is anchored on Brain Based Learning (BBL), Cognitive Load Theory (CLT), Multi-Intelligence Theory (MIT) and Multimedia Learning (ML).

# INTRODUCTION

The work presented here deals with the impact of changing from a mostly deductive type of methodology to a mostly inductive one: more precisely, this means a major change from a, **direct instruction** (Merril, 2007) classroom setting to an inductive strategy, **four components instructional design** (4C/ID) (Van Merrienboer, Clark, Coock, 2002), in a bLearning environment (Mayer, 2005). This leads directly to a major and crucial teacher concern that transform into the main **research question**: what are the accommodations, risks and concerns about the impact on students learning that you have to deal, if you make this strategic change?

Since this is a longitudinal data sample, Junior and junior high for the physics and chemistry content of the Portuguese school curriculum, the **conclusions** extrapolation should be used carefully.

The data used in this work was gathered over 13 years, in the period 2003 to 2019, from actual teacher day-to-day practice assessments about students **Learning Transfer**, **Lab Practice**, **Classroom Behavior**, that include Social Skills, and **Academic Results**. 

After starting to be interested about the different ways students learn I came across with the work of Felder´s learning styles (Felder; Silverman; 1988) and Wiley's Learning Objects (Wiley, 2002). These paper's showed me a complete list of different methodologies with different points of views: the inductive ones seems very promising and more adapted to the way brain learns. 

Furthermore, the readings about alternative **educational methodologies** went on, into the Cognitive Load Theory (Sweller, 1998), Brain Based Learning[\^2] (Jensen, 2005), Gardner Multi-Intelligence Theory (Gardner, 2011) and four component instructional design (4C/ID) (Van Merrienboer, Clark, Croock, 2002). After four years of readings and tests, in 2013 the 4C/ID methodology was applied and a bLearning environment online set with MOODLE Learning Management System (LMS) (Rice, 2006). Lessons were organized and were mostly accomplished in the classroom: fewer logs into the system were from outside.

# CONTEXT

This work includes junior and junior high students of two different schools data, from different regions, for the chemistry and physics subjects of the Portuguese curriculum, in the time scale 2003 to 2019. Raw data was gathered during 13 years, from actual day-to-day assessment registering in three different areas: transfer tests, lab practice and classroom behavior.

Figure 1 show the variable longitudinal patterns for all sample time series, which includes junior and junior high students data for the two schools. The red vertical line shows the school change and the blue one is the starting point for the implementation of 4C/ID strategic inductive methodology for the treatment group. Its clear that near each vertical line there is a change showed by the blue smooth line (polynomial local regression). So, something happened! 

Furthermore, it seems that there no positive effect of the methodological change and its clear that there is a school effect. Meaning that, the schools have different learning outcomes.

![Figure 1: all sample variables time series](Figures/figure1_boxplot_allSample.png)

The concept map's software cmap tools[^readme-2] was used for the design of the learning flow maps and they are presented online[^readme-3] showing the four components and the structure based in the four component instructional design (4C/ID), described in the book *Ten Steps to Complex Learning* (van Merriënboer; Kirschner, 2007).

[^readme-2]: <https://cmap.ihmc.us/>

[^readme-3]: Flow charts (missing details to be updated) Data base sample (R data frame): <https://cmap.ihmc.us/> ;

The **statistical analysis** uses R code software in the RStudio[^readme-4] IDE release and starts with a exploratory data analysis (EDA): a descriptive summary of the quantitative data and several plots showing patterns and relationships between variables. Afterwards there is a data normality and sample independence investigation. 

The principal methodology used is the non-parametric inferential treatment effect, but there is a discussion towards the non-parametric regression inference, regression discontinuity design and clustering. this last ones just to try to corroborate the upper first results. For this, it was used a data base[^readme-5] and 3 learning outcomes variables: transfer learning, laboratory practice and social skills, addressed respectively by 3 variables, TEST, LAB and BEHAV. The CLASS variable is directly related to the academic results and is a weigth average of TEST, LAB and BEHAV. TEST, LAB and BEHAV. This variables were assessed by paper and online transfer testing and in classroom observational paper registering forms (Koretz, 2008).

[^readme-4]: RStudio, <https://posit.co/products/open-source/rstudio/>

[^readme-5]: there was a need to make some analysis of this data sample group in order to conclude if there is an underlying school effect: in fact there is a difference confirmed with 95% confidence under the null. <https://github.com/fqantonio/GoingInductive/tree/main/DATA>

The **Variables** are described in this next table.

|     VARIABLE     | Description                                                                                                                                                                              |
|:----------------:|------------------------------------------------------|
|        ID        | Identification entry row data                                                                                                                                                            |
|       DATE       | First Year of the school year period                                                                                                                                                     |
|      SCHOOL      | Categorical variable, 0 and 1, represent two schools, identified by 0 (till 2008) and 1 (after 2009, included)                                                                           |
|      GENDER      | Categorical variable, F and M                                                                                                                                                            |
| TEST, LAB, BEHAV | Score variables in the tests and observational forms at TEST (Learning transfer), LAB (Laboratory practical skills assessment) and BEHAV (Social skills and school rules accomplishment) |
|      GRADE       | categories 0 to 6, representing, respectively, 7,8,9,10,11, 10 p (Technical) and 11p (Technical) grades                                                                                  |
|      M4CID       | Categorical 0 and 1 variable, respectively, without 4C/ID and with 4CID                                                                                                                  |
|      CLASS       | Continuous variable, 0 to 100, height average [^readme-6]                                                                                                                                |
|       RANK       | Categorical variable, 1,2,3, that measures the adaptability of students to school: 1 less adapted, 3, more adapted.                                                                      |
[^readme-6]: CLASS = 0.5 x TEST + 0.3 x LAB + 0.2 x BEHAV

# MOTIVATION

Most of the time the professional, specific, **day-to-day teacher work** data is rarely or never investigated, at least, in the Portuguese educational environment. For the teachers, there is no time to look back and work through the data results, the qualitative remarks, or through the statistics. And, as a consequence, no robust conclusions are drowned and worse, it's normal not to share it.

My **personal motivation** is to know if the strategic decisions I did in 2008 were positive for my students: did they benefit from the educational strategic change? It was a global benefit or some groups benefited more? Does anyone have the same kind of research for comparision? Can anyone corroborate this results or criticize them? Sharing is the way to go to find knowledge.

# RESULTS

Figure 2 and the next table present a resume of the **results**. The values showed represent changes in the variables median.

![Figure 2: Resume results for the 4C/ID treatment effect](Figures/Figure2_resumeResults.png)

Figure 2 shows in the y-axis the name of the samples and the x-axis show the median differences for each sample in four variables already described: TEST, LAB, BEHAV and CLASS. The variables change, represented by colors (see legend), between treatment group and control group are presented on the left of the graph if there is a negative impact and in the right if there is a positive 4C/ID impact. There is a vertical line with x equal to zero, meaning that there is no 4C/ID impact.

Looking into the graph there are two groups of results that stands: blue and green. The first means that there is a positive treatment effect for the Lab Practice and the latter means that there is a overall negative impact for the social skills: in this case only the GRADE 9 students seems to have a positive impact and no effect for FEMALE students. The TEST variable shows a tendency towards no impact with a few positive and negative. For the CLASS, Academic Results there a tendecy towards no effect to a slight negative one.

Looking into the sample JUNIOR SCHOOL 1 (sample with grades 7, 8 and 9 for school 1) that seems to be more reliable for three reasons:most **robust and reliable sample** used in this work: in fact the sample is much larger. So, they show results about the JUNIOR sample for SCHOOL 1, which means:

  1. Not having the school effect differences;
  2. The sample has students with the same age;
  3. It doesn't have the problem of using the less numbered entrances of the junior high students sample.

The next table resumes the former graph in another order: it shows the percentage of "positive", "negative" and "no effect" results, detected for each sample.

| Variable                 |  \+  | \~  | \-  |                SAMPLE +                 |       no effect sample        |                SAMPLE -                 | Notes |
|---------|:-------:|:-------:|:-------:|:-------:|:-------:|:-------:|---------|
| TEST (Learning Transfer) | 33%  | 56% | 11% | All Junior sample, 8th grade and FEMALE |          GRADE 7, 9;          |                  RANK1                  |       |
| LAB (Lab practice)       | 100% | 0%  | 0%  |               all samples               |                               |                                         |       |
| BEHAV (behavior)         | 11%  | 11% | 78% |                 GRADE 9                 |            FEMALE             | Junior; GRADE 7 and 8; all RANK's; MALE |       |
| CLASS (academic results) | 22%  | 45% | 33% |        junior; 9th grade; FEMALE        | grades 7 and 8; RANK 2 e MALE |              RANK 1 and 3               |       |

## Qualitative results 

The qualitative results show students more motivated, teacher with more individual time but a crucial need for team work among teachers.

## Qualitative

| Qualitative Results | Description   | Notes |
|---------------------------|---------------------------|------------------|
| Students inquire/comments about  | almost all positive |     |
| teacher available to interact with the students while they are following the online lessons activities |               |       |
| students feel happy to pass to the next level, high motivation                                         |               |       |
| They claim that the way they like to learn                                                             |               |       |
| massive hours of teacher work, more than 55 hours a week                                               | Health issues |       |
| Students help on lessons faults: missing information, fault links                                      |               |       |

# CONCLUSION

(need more editing ;) )

The conclusions presented here focus more on the junior sample of school 1. So, **Overall**, as a teacher of science and tech, you should (or at least think about it) move to implement the inductive strategy as your methodology for learning because it improves the **Learning Transfer**, **academic results** with a huge impact on **lab practice**, except for the **Social Skills** [^readme-8].

However, there are **risks and concerns** to be accommodated during the process, namely:
1. for the student less adapted to the school system;
2. the classroom management referring to students behavior (see discussion);
3. in the **learning transfer** process the gains are more modest;
4. males gain less with this change.

[^readme-8]: Is there an underlying behavioral underlying effect?

**Bottom line**, if you don't want to take the risks, change conditional to have a student group more adapted to school or if you want to increase the lab practices skills. Futhermore, this work shows is that there is a slight advantage of using 4C/ID eLearning setting compare to Direct Instruction. In fact, there is a huge advantage of 4C/ID in Lab Practice in spite of the negative impact in Social Skills. It also shows that the students that are less adapted to school don't improve much unless for the Lab Practice. So, compare with a instructional design with 200 years (???) of reliability is something to higlight.

RISKS:

A major risk is for the less adapted students to school: there is a need to accommodate this. 

For the **Social Skills**, specifically in the aspect of following social and behavioral classroom rules, there is a major negative [\^6] impact in all samples: the risk of loosing control of the students group inside the classroom is real; however, note that the assessment were all positive: median change from 81 to 74%, which means that the behavior was above 50% and didn't represent a thread to the classroom learning environment.

CONCERNS:

In general, GRADE 7 and RANK 2 tends to benefit less then the other grades and rank 3;

# DISCUSSION

notes for discussion: - The conlsuions meet the goals and answer th research question? - Did I implemented the methods correctly? - Is there an underlying effect for the behav variable? - Conclusions for the stakeholders in the learning process: teachers, Policy implications and school management. - blearning change: team work impressible - Reiterate results - world research corroboration ; Future research Tie loose hands: Clarify, robustness; after 2015 there is a crash in the student behavior skills.: need extra data; social skills: all, ranks, grades, and gender follow the same pattern? students behavior effect

# KEYWORDS

**`Inductive learning strategy`** - the learning flow starts by facts, experiences and real problems and move towards knowledge and a conceptual mental model;

**`4C/ID`** - instructional design approach for complex learning. It provides guidelines for the analysis of real-life tasks and the transition into a blueprint for an educational program.

**`Direct Instruction`** - Principles of Instruction that prescribe a cycle of instruction consisting of activation, demonstration, application, and integration (Merril, 2007);

**`Learning flow`** - longitudinal (temporal) map with sequenced activities organized in time, by complexity and loical learning coherence;

**`Statistical Non-parametric Inference`** - statistical techniques that use data to test if two sample came from the same non-normality distribution;

**`Learning transfer`** - the ability of pass information to the students measured by tests (online or paper supported), directly related to the TEST variable

**`Lab Practice`** - Laboratory activities directly related to the variable LAB

**`Behavior`** - Ability to follow social and behavioral classroom rules directly related to the BEHAV variable.

**`Academic results`** - weight average variable directly related to the variables: TEST, LAB and BEHAV: $$ CLASS = 0.5 \times TEST + 0.3 \times LAB + 0.2 \times BEHAV $$.

**`Cognitive Load Theory`** - Cognitive load theory provides empirically-based guidelines that help instructional designers decrease extraneous cognitive load during learning and thus refocus the learner's attention toward germane materials, thereby increasing germane (schema related) cognitive load. This theory differentiates between three types of cognitive load: intrinsic cognitive load, germane cognitive load, and extraneous cognitive load;

**`Multi Intelligences Theory`** - Multiple intelligences refers to a theory describing the different ways students learn and acquire information. These multiple intelligences range from the use of words, numbers, pictures and music, to the importance of social interactions, introspection, physical movement and being in tune with nature;

**Multimedia Learning** - Set of principles based on scientific evidence for learning from words (spoken or printed) and pictures (video, graphs, illustrations, map or photos)

**`Brain Based Learning`** - refers to teaching methods, lesson designs, and school programs that are based on the latest neurologic scientific research about how the brain learns;

**`bLearning`** - Means Blended learning for education combines classroom-based learning with distance learning (eLearning).

# REFERENCES

Felder, Richard M. ; Silverman, Linda K.; (1988), Engr. Education, 78(7), 674--681;

Gardner, Howard (2011) Frames Of Mind: The Theory Of Multiple Intelligences, ISBN-13: 978-0465024339;

Jensen, Eric (2005), Teaching with the Brain in Mind, ASCD, ISBN-10: 146600302; ISBN-13:978-1416600305;

Koretz, Daniel (2008), What Educational Testing Really Tell Us, Havard University Press, ISBN: 978-0-674-03521-8;

Mayer, Richard (editor)(2005); The Cambridge Handbook of Multimedia Learning, Cambridge University Press; ISBN: 0-521-83873-8;

Merrill, M. David (2007) A Task-Centered Instructional Strategy, Journal of Research on Technology in Education, 40:1, 5-22, DOI: 10.1080/15391523.2007.10782493

Van Merrienboer, Jeroen J. G. & Clark, Richard & Croock, Marcel. (2002). Blueprints for complex learning: The 4C/ID-model. Educational Technology Research and Development. 50. 39-61. 10.1007/BF02504993. <https://github.com/fqantonio/GoingInductive>

van Merriënboer, Jeroen J. G.; Kirschner, Paul A.; (2007) Ten Steps to Complex Learning, Routledge; 1st edition, SBN-10:0805857931; ISBN-13:978-0805857931;

Rice, William (2006); Moodle, E-Learning Course Development, Packt Publishing, ISBN: 1-904811-29-9;

Sweller, John et al. (1998) "Cognitive Architecture and Instructional Design." Educational Psychology Review 10 (1998): 251-296;

Wiley, David A. (Editor) (2002) The Instructional Use of Learning Objects, Agency for Instructional Technology Association for Educational Communications & Technology; First Edition; ISBN: 0-7842-0892-1;

# NOTES
