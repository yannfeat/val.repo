# Read me
# This file contains the help files, in Roxygen [ library(roxygen2) ] for the AMCP package, which contains the data to accompany Maxwell, Delaney, & Kelley (2018).

# Tables from the chapters
#######################################################################################################################################
#' The data used in Chapter 1, Table 1
#'
#' Assume that a developmental psychologist is interested in whether brief training can improve performance of 2-year-old children on a test of mental abilities. The test selected is the Mental Scale of the Bayley Scales of Infant Development, which yields a mental age in months. To increase the sensitivity of the experiment, the psychologist decides to rectruit sets of twins and randomly assigns one member of each pair to the treatment condition. The treatment consists of simply watching a videotape of another child attempting to perform tasks similar to those making up the Bayley Mental Scale. The other member of each pair plays in a waiting area as a time-filling activity while the first is viewing the videotape. Then both children are individually given the Bayley by a tester who is blind to their assigned conditions. A different set of twins takes part in the experiment each day, Monday through Friday, and the experiment extends over a 2-week period. Table 1.1 shows the data for the study in the middle columns.
#'
#'@section Variables:
#'\describe{
#'  \item{treat}{scores for the treatment group}
#'  \item{control}{scores for the control group}
#'  \item{week}{identifies the week}}
#'
#' @docType data
#' @name chapter_1_table_1
#' @aliases C1T1 chapter_1_table_1 Chapter_1_Table_1 c1t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_1_table_1)
#' @section Synonym:
#' C1T1
#' @examples
#' # Load the data
#' data(chapter_1_table_1)
#'
#' # Or, alternatively load the data as
#' data(C1T1)
#'
#' # View the structure
#' str(chapter_1_table_1)
#'
#' chapter_1_table_1$Difference <- chapter_1_table_1$treat - chapter_1_table_1$control
#'
#' # Summaries by week.
#'summary(chapter_1_table_1[chapter_1_table_1$week==1,])
#'summary(chapter_1_table_1[chapter_1_table_1$week==2,])
"chapter_1_table_1"

#' The data used in Chapter 3, Table 1
#'
#' Hyperactive children's IQ scores from the WISC-R
#'
#' Assume that you work in the research office of a large school system. For the last several years, the mean score on the WISC-R, which is administered to all elementary school children in your district, has been holding fairly steady at about 98. A parent of a hyperactive child in one of your special edication programs maintains that the hyperactive children in the district are actually brighter than this average. To investigate this assertion, you randomly select the files of six hyperactive children and examine their WISC-R scores. The data set analyzed to replicate Chapter 3 Table 1 consists of IQ (WISC-R) measurements on six hyperactive children. The question of interest is: "are hyperactive children in the school district brighter than the average student?" The mean IQ among the students is known to be 98. Thus, the null hypothesis in this situation is that the population mean for the hyperactive students is also 98. To answer such a question we perform a one sample \emph{t}-test specifying the value of the null hypothesis as 98. Because a t-value squared with df degrees of freedom is equivalent to an \emph{F}-value with one numerator and \emph{df} denominator degrees of freedom. Recall that the observed \emph{F}-value (with 1 and 5 degrees of freedom) in the book is 9, whereas our \emph{t}-value (with 5 degrees of freedom) is 3.
#'
#' @section Variables:
#'\describe{
#'\item{iq}{IQ score from the WISC-R}}
#'
#' @docType data
#' @name chapter_3_table_1
#' @aliases C3T1 chapter_3_table_1 Chapter_3_Table_1 c3t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_3_table_1)
#' @section Synonym:
#' C3T1
#' @examples
#' # Load the data
#' data(chapter_3_table_1)
#'
#' # Or, alternatively load the data as
#' data(C3T1)
#'
#' # View the structure
#' str(chapter_3_table_1)
#'
"chapter_3_table_1"

#' The data used in Chapter 3, Table 3
#'
#' The data used in Chapter 3, Table 3.
#'
#' Although different mood states have, of course, always been of interest to clinicians, recent years have seen a profusion of studies attempting to manipulate mood states in controlled laboratory studies. In such induced-mood research, participants typically are randomly assigned to one of three groups: a depressed-mood induction, a neutral-mood induction, or an elated-mood induction. One study (Pruitt, 1988) used selected videoslips from several movies and public television programs as the mood-induction treatments. After viewing the video for her assigned condition, each participant was asked to indicate her mood on various scales. In addition, each subject was herself videotaped, and her facial expressions of emotion were rated on a scale of 1 to 7 (1 indicating sad; 4, neutral; and 7, happy) by an assistant who viewed the videotapes but was kept "blind" regarding the subjects' assigned conditions.
#'
#' @section Variables:
#'\describe{
#'  \item{cond}{assigned Condition: a numeric vector (1=Pleasant/elated, 2=Neutral, 3=Unpleasant/depressed)}
#'  \item{mood}{a numeric vector between 1 and 7}}
#'
#' @docType data
#' @name chapter_3_table_3
#' @aliases C3T3 chapter_3_table_3 Chapter_3_Table_3 c3t3
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_3_table_3)
#' @section Synonym:
#' C3T3
#' @examples
#' # Load the data
#' data(chapter_3_table_3)
#'
#' # Or, alternatively load the data as
#' data(C3T3)
#'
#' # View the structure
#' str(chapter_3_table_3)
#'
"chapter_3_table_3"


#' The data used for Chapter 3, Table 7 (raw data to produce the summary measures)
#'
#' Raw data on the number of drinks per day (and log of the number of drinks)
#'
#' Average number of standard drinks per week at intake for a sample of homeless alcoholics at nine-month follow-up (Smith, Meyers, & Delaney, 1988). Note that the groups, 1-5, are, respectively, "T1 CRA-D", "T1 CRA+D", "T1Std", "T2 CRA-D", and "T2 Std," where CRA is "Community Reinforcement Approach (with or without Disulfiram) and where "Std" is standard therepy. Note that this is the same data as \code{data(chapter_3_table_9_raw)}.
#'
#' @section Variables:
#'\describe{
#'\item{Group}{randomly assigned group membership (see details)}
#'\item{Drinks}{number of standard drinks, on average, per week}
#'\item{LgDrinks}{log of the number of standard drinks, on average, per week}}
#'
#' @docType data
#' @name chapter_3_table_7_raw
#' @aliases C3T7R chapter_3_table_7_raw Chapter_3_Table_7_Raw c3t7r
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @references Smith, J. E., Meyers, R. J. & Delaney, H. D. (1998). The community reinforcement approach with homeless alcohol-dependent individuals. \emph{Journal of Consulting and Clinical Psychology, 66}, 541--548.
#' @keywords datasets
#' @usage data(chapter_3_table_7_raw)
#' @section Synonym:
#' C3T7R
#' @examples
#' # Load the data
#' data(chapter_3_table_7_raw)
#'
#' # Or, alternatively load the data as
#' data(C3T7R)
#'
#' # View the structure
#' str(chapter_3_table_7_raw)
#'
"chapter_3_table_7_raw"


#' The data used for Chapter 3, Table 9 (raw data to produce the summary measures)
#'
#' Raw data on the number of drinks per day (and log of the number of drinks)
#'
#' Average number of standard drinks per week at intake for a sample of homeless alcoholics at nine-month follow-up (Smith, Meyers, & Delaney, 1988). Note that the groups, 1-5, are, respectively, "T1 CRA-D", "T1 CRA+D", "T1Std", "T2 CRA-D", and "T2 Std," where CRA is "Community Reinforcement Approach (with or without Disulfiram) and where "Std" is standard therepy. Note that this is the same data as \code{data(chapter_3_table_9_raw)}.
#'
#' @section Variables:
#'\describe{
#'\item{Group}{randomly assigned group membership (see details)}
#'\item{Drinks}{number of standard drinks, on average, per week}
#'\item{LgDrinks}{log of the number of standard drinks, on average, per week}}
#'
#' @docType data
#' @name chapter_3_table_9_raw
#' @aliases C3T9R chapter_3_table_9_raw Chapter_3_Table_9_Raw c3t9r
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @references Smith, J. E., Meyers, R. J. & Delaney, H. D. (1998). The community reinforcement approach with homeless alcohol-dependent individuals. \emph{Journal of Consulting and Clinical Psychology, 66}, 541--548.
#' @keywords datasets
#' @usage data(chapter_3_table_9_raw)
#' @section Synonym:
#' C3T9R
#' @examples
#' # Load the data
#' data(chapter_3_table_9_raw)
#'
#' # Or, alternatively load the data as
#' data(C3T9R)
#'
#' # View the structure
#' str(chapter_3_table_9_raw)
#'
"chapter_3_table_9_raw"


#' The data used in Chapter 4, Table 1
#'
#' The data used in Chapter 4, Table 1
#'
#'This is hypothetical data for four groups of participants, corresponding to treatments for hypertension. The context is 24 mild hypertensives that have been independently and randomly assigned to one of the four treatments. The scores are the systolic blood pressure values two-weeks after the termination of treatment.
#'
#' @section Variables:
#'\describe{
#'\item{\code{bloodpr}}{systolic blood pressure (hypothetical data)}
#'\item{\code{cond}}{identifies group membership (1=drug therapy; 2=biofeedbacbk; 3=diet; 4=combination)}}
#'
#' @docType data
#' @name chapter_4_table_1
#' @aliases C4T1 chapter_4_table_1 Chapter_4_Table_1 c4t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_4_table_1)
#' @section Synonym:
#' C4T1
#' @examples
#' # Load the data
#' data(chapter_4_table_1)
#'
#' # Or, alternatively load the data as
#' data(C4T1)
#'
#' # View the structure
#' str(chapter_4_table_1)
#'
"chapter_4_table_1"


#' The data used in Chapter 4, Table 7
#'
#' The data used in Chapter 4, Table 7
#'
#' Data used to demonstrate another property of orthogonal contrasts. The demonstrated principle is that the sums of squares of nonorthogonal contrasts are not additive, yet the sums of squares of orthogonal contrasts can be added to determine the magnitude of the sum of squares they jointly account for.
#'
#' @section Variables:
#'\describe{
#'\item{dv}{dependent variable}
#'\item{group}{Group number: 1, 2, or 3}}
#'
#' @docType data
#' @name chapter_4_table_7
#' @aliases C4T7 chapter_4_table_7 Chapter_4_Table_7 c4t7
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_4_table_7)
#' @section Synonym:
#' C4T7
#' @examples
#' # Load the data
#' data(chapter_4_table_7)
#'
#' # Or, alternatively load the data as
#' data(C4T7)
#'
#' # View the structure
#' str(chapter_4_table_7)
#'
"chapter_4_table_7"

#' The data used in Chapter 5, Table 4
#'
#' The data used in Chapter 5, Table 4
#'
#' The following data consists of blood pressure measurements for six individuals randomly assigned to one of four groups. Our purpose here is to perform four planed contrasts in order to discern if group differences exist for the selected contrasts of interests.
#'
#' @section Variables:
#'\describe{
#'\item{group}{a numeric vector between 1 and 4; group number}
#'\item{sbp}{systolic blood pressure of a patient within one of the four groups}}
#'
#' @docType data
#' @name chapter_5_table_4
#' @aliases C5T4 chapter_5_table_4 Chapter_5_Table_4 c5t4
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_5_table_4)
#' @section Synonym:
#' C5T4
#' @examples
#' # Load the data
#' data(chapter_5_table_4)
#'
#' # Or, alternatively load the data as
#' data(C5T4)
#'
#' # View the structure
#' str(chapter_5_table_4)
#'
"chapter_5_table_4"


#' The data used in Chapter 6, Table 1
#'
#' The data used in Chapter 6, Table 1
#'
#' Recall scores for 24 children who have been randomly assigned to one of four experimental conditions where there are 6 children in each condition. The experimental conditions of interest are 1, 2, 3, and 4 minutes where the number of minutes is the amount of time the child is allotted to study a list of words before attempting to recall the words. The dependent variable (i.e., the recall scores) are the number of words the child is able to recall after a brief interference task. The first hypothesis of interest is whether the number of words recalled is linearly related to the number of minutes spent studying.
#'
#' @section Variables:
#'\describe{
#'\item{recall}{the number of words recalled by the child after the study time expires}
#'\item{minutes}{the amount of time, in minutes, the child was permitted to study}}
#'
#' @docType data
#' @name chapter_6_table_1
#' @aliases C6T1 chapter_6_table_1 Chapter_6_Table_1 c6t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_6_table_1)
#' @section Synonym:
#' C6T1
#' @examples
#' # Load the data
#' data(chapter_6_table_1)
#'
#' # Or, alternatively load the data as
#' data(C6T1)
#'
#' # View the structure
#' str(chapter_6_table_1)
#'
"chapter_6_table_1"


#' The data used in Chapter 7, Table 1
#'
#' The data used in Chapter 7, Table 1
#'
#' This data is the hypothetical data from a psychologist's evaluation of the effectiveness of biofeedback and drug therapy for treating hypertension (lowering blood pressure). There are four groups: both biofeedback training and drug therapy, biofeedback but not drug therapy, drug therapy but no biofeedback, and neither biofeedback nor drug therapy... As usual, in this data set, the number of subjects is kept small to minimize the computational burden. We assume that the scores in the table represent systolic blood pressure readings taken at the end of the treatment period.
#'
#' The following data consists specifically of blood pressure measurements taken after the end of treatment for five individuals that were randomly assigned to one of four groups. The initial question of interest is whether there is a significant difference between any of the group means, that is, are all of the population group means equal or is there a difference somewhere.
#'
#' As before, we can perform a one-way ANOVA via the One-Way ANOVA procedure to replicate the results given in Table 7.2.
#'
#' @section Variables:
#'\describe{
#'\item{group}{a numeric vector between 1 and 4 equal to the drug therapy group}
#'\item{score}{the blood pressure of one of the individuals in the study}}
#'
#' @docType data
#' @name chapter_7_table_1
#' @aliases C7T1 chapter_7_table_1 Chapter_7_Table_1 c7t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_7_table_1)
#' @section Synonym:
#' C7T1
#' @examples
#' # Load the data
#' data(chapter_7_table_1)
#'
#' # Or, alternatively load the data as
#' data(C7T1)
#'
#' # View the structure
#' str(chapter_7_table_1)
#'
"chapter_7_table_1"


#' The data used in Chapter 7, Table 5
#'
#' The data used in Chapter 7, Table 5
#'
#' This table represents hypothetical data from a study investigating the effects of biofeedback and drug therapy on hypertension. We (arbitrarily) refer to the presence or absence of biofeedback as factor A and to the type of drug as factor B.
#'
#' The following data is a generalization of the blood pressure data given in Table 7.1 (as there are now three, rather than two, levels of the drug factor). In addition to assessing the likelihood of there being a biofeedback or a drug main effect, the interaction is explicitly taken into consideration.
#'
#' @section Variables:
#'\describe{
#'\item{score}{blood pressure}
#'\item{feedback}{the likelihood of there being a biofeedback or drug main effect}
#'\item{drug}{the level of the drug factor}}
#'
#' @docType data
#' @name chapter_7_table_5
#' @aliases C7T5 chapter_7_table_5 Chapter_7_Table_5 c7t5
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_7_table_5)
#' @section Synonym:
#' C7T5
#' @examples
#' # Load the data
#' data(chapter_7_table_5)
#'
#' # Or, alternatively load the data as
#' data(C7T5)
#'
#' # View the structure
#' str(chapter_7_table_5)
#'
"chapter_7_table_5"


#' The data used in Chapter 7, Table 9
#'
#' The data used in Chapter 7, Table 9
#'
#' The following data is a generalization of the blood pressure data given in Table 7.5 (which itself was a generalization of the data given in Table 7.1). After the interaction is found to be significant, a common recommendation is to examine simple main effects. Recall that a simple main effect is the main effect of one factor given a fixed level of another factor. In this case interest is in determining if there are any differences in drugs (a) given biofeedback and (b) given no biofeedback.
#'
#' @section Variables:
#'\describe{
#'\item{score}{blood pressure}
#'\item{feedback}{the likelihood of there being a biofeedback or drug main effect}
#'\item{drug}{the level of the drug factor}}
#'
#' @docType data
#' @name chapter_7_table_9
#' @aliases C7T9 chapter_7_table_9 Chapter_7_Table_9 c7t9
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_7_table_9)
#' @section Synonym:
#' C7T9
#' @examples
#' # Load the data
#' data(chapter_7_table_9)
#'
#' # Or, alternatively load the data as
#' data(C7T9)
#'
#' # View the structure
#' str(chapter_7_table_9)
#'
"chapter_7_table_9"

#' The data used in Chapter 7, Table 11
#'
#' The data used in Chapter 7, Table 11
#'
#' Table 7.11 presents this hypothetical data for 15 amnesiacs, 15 Huntington individuals, and 15 controls. The data represents a two way factorial design where diagnosis and task are fully crossed, each with three levels. Of interest for the results displayed in Table 7.12 is whether the interaction contrast specified in Figure 7.3 and 7.4 is statistically significant. Namely the question pertains to whether the relationship of the mean of grammar and classification versus recognition differs for those in the amnesic and Huntington's group. The procedure SPSS syntax procedure MANOVA is very general and can handle many types of analyses. Interaction contrasts are easily performed in this procedure.
#'
#' Consider an example of a cognitive neuroscience study of patient groups. Specifically, suppose that a certain theory implies that amnesic patients will have a deficit in explicit memory but not in implicit memory. According to this theory, Huntington patients, on the other hand, will be just the opposite: They will have no deficit in explicit memory, but will have a deficit in implicit memory. Further suppose that a study is designed yielding a 3x3 factorial design to test this theory. The rows of this study will represent three types of individuals: amnesic patients, Huntington patients, and a control group of individuals with no known neurological disorder. Each research participant will be randomly assigned to one of three tasks: (1) artificial grammar task, which consists of classifying letter sequences as either following or not following grammatical rules; (2) classification learning task, which consists of classifying hypothetical patients as either having or not having a certain disease based on symptoms probabilistically related to the disease; and (3) recognition memory task, which consists of recognizing particular stimuli as stimuli that have previously been presented during the task.
#'
#' @section Variables:
#'\describe{
#'\item{score}{blood pressure}
#'\item{feedback}{the likelihood of there being a biofeedback or drug main effect}
#'\item{drug}{the level of the drug factor}}
#'
#' @docType data
#' @name chapter_7_table_11
#' @aliases C7T11 chapter_7_table_11 Chapter_7_Table_11 c7t11
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_7_table_11)
#' @section Synonym:
#' C7T11
#' @examples
#' # Load the data
#' data(chapter_7_table_11)
#'
#' # Or, alternatively load the data as
#' data(C7T11)
#'
#' # View the structure
#' str(chapter_7_table_11)
#'
"chapter_7_table_11"


#' The data used in Chapter 7, Table 15
#'
#' The data used in Chapter 7, Table 15
#'
#' The following hypothetical salary data represents a nonorthogonal two-by-two factorial design. The first factor (sex) is crossed with college (degree or no degree). The primary question of interest is whether or not there is sex discrimination in terms of salary.
#'
#' The data in Table 7.15 presents hypothetical data (in thousands) for 12 females and 10 males who have just been hired by the organization. The mean salary for the 12 females is $22,333, whereas the mean for the 10 males is $22,100. The data in Table 7.15 also contains information about an additional characteristic of employees, namely whether they received a college degree. It is clear from the data that a majority of the new female employees are college graduates, whereas a majority of the males are not.
#'
#'
#' @section Variables:
#'\describe{
#'\item{sex}{gender (male vs female)}
#'\item{educ}{education level (degree vs no degree)}
#'\item{salary}{salary (in thousands)}
#'}
#'
#' @docType data
#' @name chapter_7_table_15
#' @aliases C7T15 chapter_7_table_15 Chapter_7_Table_15 c7t15
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_7_table_15)
#' @section Synonym: 
#' C7T15
#' @examples
#' # Load the data
#' data(chapter_7_table_15)
#'
#' # Or, alternatively load the data as
#' data(C7T15)
#'
#' # View the structure
#' str(chapter_7_table_15)
#'
"chapter_7_table_15"


#' The data used in Chapter 7, Table 15
#'
#' The data used in Chapter 7, Table 15
#'
#' Table 7.23 shows hypothetical MMPI scores for 45 participants, each of whom is placed in one cell of a 3x3 design. One factor (A, the row factor) is type of therapy. The other factor (B, the column factor) is degree of severity.
#'
#' Suppose that a clinical psychologist is interested in comparing the relative effectiveness of three forms of psychotherapy for alleviating depression. Fifteen individuals are randomly assigned to one of each of three treatment groups: cognitive-behavioral, Rogerian, and assertiveness training. The Depression Scale of the MMPI serves as the dependent variable. After the fact, these individuals where placed into one of three categories based on the severity of their depression. Thus, this data set represents a 3 by 3 nonorthogonal factorial design with post hoc blocking.
#'
#' The data represents the relative effectiveness of three forms of psychotherapy for alleviating depression. Fifteen individuals were randomly assigned to one of three groups. After the fact, these individuals where placed into one of three categories based on the severity of their depression. Thus, this data set represents a 3 by 3 nonorthogonal factorial design with post hoc blocking.
#'
#' @section Variables:
#'\describe{
#'\item{therapy}{the type of therapy}
#'\item{severity}{the severity of the therapy}
#'\item{score}{the score of the individual}
#'}
#'
#' @docType data
#' @name chapter_7_table_15
#' @aliases C7T15 chapter_7_table_15 Chapter_7_Table_15 c7t15
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_7_table_15)
#' @section Synonym:
#' C7T15
#' @examples
#' # Load the data
#' data(chapter_7_table_15)
#'
#' # Or, alternatively load the data as
#' data(C7T15)
#'
#' # View the structure
#' str(chapter_7_table_15)
#'
"chapter_7_table_15"


#' The data used in Chapter 7, Table 23
#'
#' The data used in Chapter 7, Table 23
#'
#' Suppose that a clinical psychologist is interested in comparing the relative effectiveness of three forms of psychotherapy for alleviating depression. Fifteen individuals are randomly assigned to one of each of three treatment groups: cognitive-behavioral, Rogerian, and assertiveness training. The Depression Scale of the MMPI serves as the dependent variable. After the fact, these individuals where placed into one of three categories based on the severity of their depression. Thus, this data set represents a 3 by 3 nonorthogonal factorial design with post hoc blocking. Table 7.23 shows hypothetical MMPI scores for 45 participants, each of whom is placed in one cell of a 3x3 design. One factor (A, the row factor) is type of therapy. The other factor (B, the column factor) is degree of severity.
#'
#' The data represents the relative effectiveness of three forms of psychotherapy for alleviating depression. Fifteen individuals were randomly assigned to one of three groups. After the fact, these individuals where placed into one of three categories based on the severity of their depression. Thus, this data set represents a 3 by 3 nonorthogonal factorial design with post hoc blocking.
#'
#' @section Variables:
#'\describe{
#'\item{therapy}{the type of therapy}
#'\item{severity}{the severity of the therapy}
#'\item{score}{the score of the individual}
#'}
#'
#' @docType data
#' @name chapter_7_table_23
#' @aliases C7T23 chapter_7_table_23 Chapter_7_Table_23 c7t23
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_7_table_23)
#' @section Synonym:
#' C7T23
#' @examples
#' # Load the data
#' data(chapter_7_table_23)
#'
#' # Or, alternatively load the data as
#' data(C7T23)
#'
#' # View the structure
#' str(chapter_7_table_23)
#'
"chapter_7_table_23"


#' The data used in Chapter 8, Table 12
#'
#' The data used in Chapter 8, Table 12
#'
#' This example builds from the hypertension example used in chapter 7 for the two-way design. The data in Table 8.12 consist of blood pressure scores for 72 participants. Three categorical independent variables: the presence and absence of biofeedback (biofeed), drug X, Y, or Z (drug), and diet absent or present (diet) have been factorially combined to form a 2 x 3 x 2 design where each person contributes one blood pressure score to one of the 12 different experimental conditions. For this example, there are 6 participants in each group.#'
#'
#' @section Variables:
#'\describe{
#'\item{bp}{patient blood pressure}
#'\item{drug}{drug given (X,Y,or Z)}
#'\item{biofeed}{presence or absence of biofeedback}
#'\item{diet}{presence of absence of a diet}
#'}
#'
#' @docType data
#' @name chapter_8_table_12
#' @aliases C8T12 chapter_8_table_12 Chapter_8_Table_12 c8t12
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_8_table_12)
#' @section Synonym:
#' C8T12
#' @examples
#' # Load the data
#' data(chapter_8_table_12)
#'
#' # Or, alternatively load the data as
#' data(C8T12)
#'
#' # View the structure
#' str(chapter_8_table_12)
#'
"chapter_8_table_12"


#' The data used in Chapter 9, Table 1
#'
#' The data used in Chapter 9, Table 1
#'
#' The data in Table 9.1 are the numerical values for the data that is presented in Figure 9.1, which presents a comparison of errors in ANOVA and ANCOVA restricted models.
#'
#' The data represents a pre-post design, where a training program designed to assist people in losing weight is evaluated. An initial measure of weight is collected to use as a baseline measure (specifically as a covariate in the present analysis) and then participants are randomly assigned to one of two groups. At the end of the training program another measure of weight is obtained. The question of interest is: "did the participants who received the treatment lose more weight than those that were assigned to the wait-list control group?"
#'
#' @section Variables:
#'\describe{
#'\item{group}{the group (treatment group vs wait-list control group)}
#'\item{x}{the weight lost by the control group}
#'\item{y}{the weight lost by the treatment group}}
#'
#' @docType data
#' @name chapter_9_table_1
#' @aliases C9T1 chapter_9_table_1 Chapter_9_Table_1 c9t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_9_table_1)
#' @section Synonym:
#' C9T1
#' @examples
#' # Load the data
#' data(chapter_9_table_1)
#'
#' # Or, alternatively load the data as
#' data(C9T1)
#'
#' # View the structure
#' str(chapter_9_table_1)
#'
"chapter_9_table_1"


#' The data used in Chapter 9, Table 7
#'
#' The data used in Chapter 9, Table 7
#'
#' The data shown in Table 9.7 represents a hypothetical three-group study assessing different interventions for depression. 30 depressive individuals have been randomly assigned to one of three conditions: (1) selective serotonin reuptake inhibitor (SSRI) antidepressant medication, (2) placebo, or (3) wait list control. The Beck Depression Inventory (BDI) has been administered to each individual prior to the study, and then later is administered a second time at the end of the study. The data represents a three group pre-post design, where the 30 depressives were randomly assigned to one of three conditions. The primary question of interest is: "do individuals in some groups change more on their measures of depression than do individuals in other groups?"#'
#'
#' @section Variables:
#'\describe{
#'\item{cond}{the treatment condition (SSRI, Placebo, Wait List Control)}
#'\item{pre}{the measure of depression before the experiment}
#'\item{post}{the measure of depression after the experiment}
#'}
#'
#' @docType data
#' @name chapter_9_table_7
#' @aliases C9T7 chapter_9_table_7 Chapter_9_Table_7 c9t7
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_9_table_7)
#' @section Synonym:
#' C9T7
#' @examples
#' # Load the data
#' data(chapter_9_table_7)
#'
#' # Or, alternatively load the data as
#' data(C9T7)
#'
#' # View the structure
#' str(chapter_9_table_7)
#'
"chapter_9_table_7"


#' The data used in Chapter 9, Table 11
#'
#' The data used in Chapter 9, Table 11
#'
#' The question of interest in the present situation assumes that there are three blocks of elderly participants, six per block. The elderly participants are sorted into the three blocks as a function of their age. The purpose of the study was to assess the effect of age on motor control, measured by the number of errors on a certain task (where there were three tasks). The goal here is to replicate the results from the top portion of Table 9.12 (At this point performing the ANOVA and the ANCOVA also included in Table 9.12 should be straightforward.#'
#'
#' @section Variables:
#'\describe{
#'\item{block}{a numeric vector between 1 and 3, equal to the block of elderly participants (6 per block)}
#'\item{task}{the task given}
#'\item{x}{age}
#'\item{y}{error scores}
#'}
#'
#' @docType data
#' @name chapter_9_table_11
#' @aliases C9T11 chapter_9_table_11 Chapter_9_Table_11 c9t11
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_9_table_11)
#' @section Synonym:
#' C9T11
#' @examples
#' # Load the data
#' data(chapter_9_table_11)
#'
#' # Or, alternatively load the data as
#' data(C9T11)
#'
#' # View the structure
#' str(chapter_9_table_11)
#'
"chapter_9_table_11"


#' The data used in Chapter 9, Extension Table 1
#'
#' The data used in Chapter 9, Extension Table 1
#'
#' Table 9E.1 shows the data from Table 9.1 after some minor modifications to reflect heterogeneity of regression. The data were altered in such a way that the means in both groups are the same as in the original example, as is the pooled within-group slope.
#'
#' @section Variables:
#'\describe{
#'\item{group}{the group (treatment group vs wait-list control group)}
#'\item{x}{the weight lost by the control group}
#'\item{y}{the weight lost by the treatment group}
#'}
#'
#' @docType data
#' @name chapter_9_extension_table_1
#' @aliases C9ExtT1 chapter_9_extension_table_1 Chapter_9_Extension_Table_1 c9extt1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_9_extension_table_1)
#' @section Synonym:
#' C9ExtT1
#' @examples
#' # Load the data
#' data(chapter_9_extension_table_1)
#'
#' # Or, alternatively load the data as
#' data(C9ExtT1)
#'
#' # View the structure
#' str(chapter_9_extension_table_1)
#'
"chapter_9_extension_table_1"



#' The data used in Chapter 10, Table 5
#'
#' The data used in Chapter 10, Table 5
#'
#' Assume that an educational products firm markets study programs to help high school students prepare for college entrance exams such as the ACT, and wants to compare a new computer-based training program with their standard packet of printed materials. The firm would like to be able to generalize to all American high schools but only has the resources to conduct a study in a few schools. Thus, assume four high schools are selected at random from a listing of all public schools in the country. Volunteers from the junior class at these schools are solicited to take part in an eight-session after-school study program. Ten students from each school are permitted to take part, and equal numbers from each school are assigned randomly to the two study programs. Designating the type of study program as factor A (a1 designates the computer-based program and a2 designates the standard paper-and-pencil program) and the particular school as factor B, assume the data in Table 10.5 are obtained.
#'
#' The data consists of simulated ACT scores from 40 participants where 10 participants were selected from each of four schools. It is assumed that the schools are randomly selected from a population of schools in America in order to generalize the results found. Two schools (and thus, 20 participants) are randomly assigned to the computer-based ACT training program, while the other two schools are randomly assigned to the standard paper-and-pencil program in order to assess the effectiveness of these different types of programs.
#'
#' The primary hypothesis of interest is whether the standard paper-and-pencil and computer-based ACT training programs differ in effectiveness.
#'
#' @section Variables:
#'\describe{
#'\item{a}{type of study program}
#'\item{b}{the particular school}
#'\item{act}{the individual's ACT score}
#'}
#'
#' @docType data
#' @name chapter_10_table_5
#' @aliases C10T5 chapter_10_table_5 Chapter_10_Table_5 c10t5
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_10_table_5)
#' @section Synonym:
#' C10T5
#' @examples
#' # Load the data
#' data(chapter_10_table_5)
#'
#' # Or, alternatively load the data as
#' data(C10T5)
#'
#' # View the structure
#' str(chapter_10_table_5)
#'
"chapter_10_table_5"


#' The data used in Chapter 10, Table 9
#'
#' The data used in Chapter 10, Table 9
#'
#' The data in Table 10.9 is based upon the information from the student therapist example of the random-effects section. Assume that the director of the clinic decides to test for a difference across genders in the general severity ratings that graduate students assign to clients. If three male and three female clinical students are randomly selected to participate, and each is randomly assigned four clients with whom to do an intake interview, then we might obtain data like that shown in Table 10.9. Three of the trainees are males while the other three trainees are females. The trainees are nested within their particular gender.
#'
#' @section Variables:
#'\describe{
#'\item{a}{type of study program}
#'\item{b}{the particular school}
#'\item{act}{the individual's ACT score}
#'}
#'
#' @docType data
#' @name chapter_10_table_9
#' @aliases C10T9 chapter_10_table_9 Chapter_10_Table_9 c10t9
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_10_table_9)
#' @section Synonym:
#' C10T9
#' @examples
#' # Load the data
#' data(chapter_10_table_9)
#'
#' # Or, alternatively load the data as
#' data(C10T9)
#'
#' # View the structure
#' str(chapter_10_table_9)
#'
"chapter_10_table_9"


#' The data used in Chapter 11, Table 1
#'
#' The data used in Chapter 11, Table 1
#'
#' Table 11.1 displays the data from the observation of six subjects under two treatment conditions, yielding 12 scores in all on the dependent variable.
#'
#' For the data set, six individuals were observed under two different conditions. The question of interest is: "does the mean of the scores in Condition 1 differ from the mean of the scores in Condition 2?"
#'
#' @section Variables:
#'\describe{
#'\item{ycond1}{data from the six subjects under treatment condition 1}
#'\item{ycond2}{data from the six subjects under treatment condition 2}
#'}
#'
#' @docType data
#' @name chapter_11_table_1
#' @aliases C11T1 chapter_11_table_1 Chapter_11_Table_1 c11t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_11_table_1)
#' @section Synonym:
#' C11T1
#' @examples
#' # Load the data
#' data(chapter_11_table_1)
#'
#' # Or, alternatively load the data as
#' data(C11T1)
#'
#' # View the structure
#' str(chapter_11_table_1)
#'
"chapter_11_table_1"



#' The data used in Chapter 11, Table 4
#'
#' The data used in Chapter 11, Table 4
#'
#' No analyses are conducted for this data set. The traditional view of a repreated-measures design is to regard it as a two-factor design. Specifically, one factor represents the repeated condition (e.g., time, drug, subtest), whereas the second factor represents subjects. The rationale for this conceptualization can be understood by considering the data in Table 11.4. When the data are displayed this way, the design looks very much like other factorial designs we've already encountered.
#'
#' @section Variables:
#'\describe{
#'\item{ycond1}{a numeric vector}
#'\item{ycond2}{a numeric vector}
#'\item{ycond3}{a numeric vector}
#'\item{ycond4}{a numeric vector}
#'}
#'
#' @docType data
#' @name chapter_11_table_4
#' @aliases C11T4 chapter_11_table_4 Chapter_11_Table_4 c11t4
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_11_table_4)
#' @section Synonym:
#' C11T4
#' @examples
#' # Load the data
#' data(chapter_11_table_4)
#'
#' # Or, alternatively load the data as
#' data(C11T4)
#'
#' # View the structure
#' str(chapter_11_table_4)
#'
"chapter_11_table_4"



#' The data used in Chapter 11, Table 5
#'
#' The data used in Chapter 11, Table 5
#'
#' The data show that 12 participants have been observed in each of 4 conditions. To make the example easier to discuss, let's suppose that the 12 subjects are children who have been observed at 30, 36, 42, and 48 months of age. Essentially, for the present data set, 12 children were each observed four times over an 18 month period. The dependent variable is the age-normed general cognitive score on the McCarthy Scales of Children's Abilities. Interest is to determine if the children were sampled from a population where growth in cognitive ability is more rapid or less rapid than average.
#'
#' @section Variables:
#'\describe{
#'\item{months30}{age-normed general cognitive score for 30-month-old}
#'\item{months36}{age-normed general cognitive score for 36-month-old}
#'\item{months42}{age-normed general cognitive score for 42-month-old}
#'\item{months48}{age-normed general cognitive score for 48-month-old}
#'}
#'
#' @docType data
#' @name chapter_11_table_5
#' @aliases C11T5 chapter_11_table_5 Chapter_11_Table_5 c11t5
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_11_table_5)
#' @section Synonym:
#' C11T5
#' @examples
#' # Load the data
#' data(chapter_11_table_5)
#'
#' # Or, alternatively load the data as
#' data(C11T5)
#'
#' # View the structure
#' str(chapter_11_table_5)
#'
"chapter_11_table_5"


#' The data used in Chapter 11, Table 19
#'
#' The data used in Chapter 11, Table 19
#'
#' Table 11.19 duplicates a table from Shrout and Fleiss showing hypothetical data obtained from four judges, each of whom has rated six targets (i.e., subjects). For the present (hypothetical) data set (taken from Shrout and Fleiss, 1979) consists of six participants who are ranked by four judges.
#'
#' As is pointed out in the book, the structure of Table 11.19 is analogous to that of Table 11.5 (repeated measures). However, notice that in the data file that the data are entered differently. For the repeated measures design (e.g., Table 11.5), each row corresponded to a different participant, while each column corresponded with another measurement. The main reason for the difference in how the data was entered is mainly because of the procedures used to analyze the data. SPSS and SAS allow repeated measures data to be entered in a "participants by occasions" fashion, whereas in order to get the appropriate mean squares to carryout the intraclass correlations for the data given in Table 11.19, a mixed-effects (one fixed factor and one random factor) ANOVA needs to be performed.
#'
#' @section Variables:
#'\describe{
#'\item{subject}{a numeric vector}
#'\item{judge}{judge number; of 3}
#'\item{rating}{a numeric vector}
#'}
#'
#' @docType data
#' @name chapter_11_table_19
#' @aliases C11T19 chapter_11_table_19 Chapter_11_Table_19 c11t19
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_11_table_19)
#' @section Synonym:
#' C11T19
#' @examples
#' # Load the data
#' data(chapter_11_table_19)
#'
#' # Or, alternatively load the data as
#' data(C11T19)
#'
#' # View the structure
#' str(chapter_11_table_19)
#'
"chapter_11_table_19"


#' The data used in Chapter 11, Table 20
#'
#' The data used in Chapter 11, Table 20
#'
#' Table 11.20 shows hypothetical data obtained from three judges, each of whom has rated five targets (i.e., subjects). This data will be important in determining if our reliability measure should reflect consistency or agreement. Notice that the rank order of targets is identical for each of the three judges (in fact, not only are the ranks identical, but the scores are also perfectly linearly related to one another in this example). However, in an absolute sense, the ratings provided by Judge 2 are clearly very different from the ratings of the other two judges... Consistency is relatively low in these data, because the columns of scores do not closely resemble one another. However, agreement is high in these data because the relative position of any target in the distribution of scores is identical for each and every judge.
#'
#' The analysis of the data contained in Table 11.20 is carried out in exactly the same manner as was the data contained in Table 11.19. Thus, a mixed effects ANOVA model is performed in order to obtain the mean squares which are then used in the formulas give towards the end of Chapter 11.
#'
#' @section Variables:
#'\describe{
#'\item{months30}{age-normed general cognitive score for 30-month-old}
#'\item{months36}{age-normed general cognitive score for 36-month-old}
#'\item{months42}{age-normed general cognitive score for 42-month-old}
#'\item{months48}{age-normed general cognitive score for 48-month-old}
#'}
#'
#' @docType data
#' @name chapter_11_table_20
#' @aliases C11T20 chapter_11_table_20 Chapter_11_Table_20 c11t20
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_11_table_20)
#' @section Synonym:
#' C11T20
#' @examples
#' # Load the data
#' data(chapter_11_table_20)
#'
#' # Or, alternatively load the data as
#' data(C11T20)
#'
#' # View the structure
#' str(chapter_11_table_20)
#'
"chapter_11_table_20"


#' The data used in Chapter 12, Table 1
#'
#' The data used in Chapter 12, Table 1
#'
#' Suppose that a psychologist studying the visual system was interested in determining the extent to which interfering visual stimuli slow the abilityto recognize letters. Subjects are brought into a laboratory and seated in front of a tachistoscope. Subjects are told that they will see either the letter T or the letter I displayed on the screen. In some trials, the letter appears by itself, but in other trials, the target letter is embedded in a group of other letters. This variation in the display consitutes the first factor, which is referred to as noise. The noise factor has two levels -- absent and present. The other factor varied by the experimenter is where in the display the target letter appears. This factor, which is called angle, has three levels. The target letter is either shown at the center of the screen (0 degrees off-center, where the subject has been instructed to fixate), 4 degrees off-center or 8 degrees off-center (in each case, the deviation from the center varies randomly between left and right).
#' The data in Table 12.1 consist of reaction time scores for 10 participants where each participant contributes 6 scores to the analysis. In particular, each participant is exposed to each of 6 experimental conditions, which are obtained by factorially combining angle (0, 4, and 8) with noise (absent and present). The tests of interest are the omnibus tests within the two-factor within-subjects ANOVA. The dependent measure is reaction time (latency), measured in milliseconds (ms), required by the subject to identify the correct target letter. Each subject has six scores.
#'
#' @section Variables:
#'\describe{
#'    \item{abs0}{reaction time w/ noise absent, angle 0}
#'    \item{abs4}{reaction time w/ noise absent, angle 4}
#'    \item{abs8}{reaction time w/ noise absent, angle 8}
#'    \item{pres0}{reaction time w/ noise present, angle 0}
#'    \item{pres4}{reaction time w/ noise present, angle 4}
#'    \item{pres8}{reaction time w/ noise present, angle 8}
#'}
#'
#' @docType data
#' @name chapter_12_table_1
#' @aliases C12T1 chapter_12_table_1 Chapter_12_Table_1 c12t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_12_table_1)
#' @section Synonym:
#' C12T1
#' @examples
#' # Load the data
#' data(chapter_12_table_1)
#'
#' # Or, alternatively load the data as
#' data(C12T1)
#'
#' # View the structure
#' str(chapter_12_table_1)
#'
"chapter_12_table_1"



#' The data used in Chapter 12, Table 7
#'
#' The data used in Chapter 12, Table 7
#'
#' Table 12.7 presents scores for the individual subjects for the A effect for the data in Table 12.1. Notice that each score for a given subject is simply that subject's mean response time for that angle, where the mean is the average of the noise-absent and the noise-present scores.
#'
#' @section Variables:
#'\describe{
#'\item{angle0}{mean reaction time for subject at angle factor 0, averaging over noise}
#'\item{angle4}{mean reaction time for subject at angle factor 4, averaging over noise}
#'\item{angle8}{mean reaction time for subject at angle factor 8, averaging over noise}
#'}
#'
#' @docType data
#' @name chapter_12_table_7
#' @aliases C12T7 chapter_12_table_7 Chapter_12_Table_7 c12t7
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_12_table_7)
#' @section Synonym:
#' C12T7
#' @examples
#' # Load the data
#' data(chapter_12_table_7)
#'
#' # Or, alternatively load the data as
#' data(C12T7)
#'
#' # View the structure
#' str(chapter_12_table_7)
#'
"chapter_12_table_7"


#' The data used in Chapter 12, Table 9
#'
#' The data used in Chapter 12, Table 9
#'
#' A different covariance matrix is relevant for the B main effect because the B effect averages over levels of A, whereas the A effect averages over levels of B. Table 12.9 presents each subject's mean score for noise absent and noise present, where the mean is the average of the three angle scores at that particular level of noise.
#'
#' @section Variables:
#'\describe{
#'    \item{noiseabs}{mean reaction time for subject without noise, averaging over angle}
#'    \item{noiseprs}{mean reaction time for subject with noise, averaging over angle}
#'}
#'
#' @docType data
#' @name chapter_12_table_9
#' @aliases C12T9 chapter_12_table_9 Chapter_12_Table_9 c12t9
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_12_table_9)
#' @section Synonym:
#' C12T9
#' @examples
#' # Load the data
#' data(chapter_12_table_9)
#'
#' # Or, alternatively load the data as
#' data(C12T9)
#'
#' # View the structure
#' str(chapter_12_table_9)
#'
"chapter_12_table_9"


#' The data used in Chapter 12, Table 11
#'
#' The data used in Chapter 12, Table 11
#'
#' A third covariance matrix is relevant for the AB interaction because this effect does not average over either A or B. Instead, the interaction assesses whether the B difference is the same at each level of A. Table 12.11 presents scores that address this question. For each subject, a given score represents the noise effect (i.e., reaction time when noise is present minus reaction time when noise is absent) at a particular level of the angle factor.
#'
#' #' @section Variables:
#'\describe{
#'\item{angle0}{reaction time when noise is present minus reaction time when noise is absent at angle 0}
#'\item{angle4}{reaction time when noise is present minus reaction time when noise is absent at angle 4}
#'\item{angle8}{reaction time when noise is present minus reaction time when noise is absent at angle 8}
#'}
#'
#' @docType data
#' @name chapter_12_table_11
#' @aliases C12T11 chapter_12_table_11 Chapter_12_Table_11 c12t11
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_12_table_11)
#' @section Synonym:
#' C12T11
#' @examples
#' # Load the data
#' data(chapter_12_table_11)
#'
#' # Or, alternatively load the data as
#' data(C12T11)
#'
#' # View the structure
#' str(chapter_12_table_11)
#'
"chapter_12_table_11"


#' The data used in Chapter 12, Table 15
#'
#' The data used in Chapter 12, Table 15
#'
#' The data in Table 12.15 consist of reaction time scores for 10 young participants where each participant contributes 3 scores to the analysis. In particular, each participant is exposed to each of 3 experimental conditions, angle (0, 4, and 8). For the current analyses Table 12.15 is appended to Table 12.7, which contains reaction time scores for 10 old participants for angles of 0, 4, and 8. Thus, it is necessary to perform some data management before analyzing the data.
#'
#' @section Variables:
#'\describe{
#'\item{angle0}{reaction time when noise is present minus reaction time when noise is absent at angle 0}
#'\item{angle4}{reaction time when noise is present minus reaction time when noise is absent at angle 4}
#'\item{angle8}{reaction time when noise is present minus reaction time when noise is absent at angle 8}
#'}
#'
#' @docType data
#' @name chapter_12_table_15
#' @aliases C12T15 chapter_12_table_15 Chapter_12_Table_15 c12t15
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_12_table_15)
#' @section Synonym:
#' C12T15
#' @examples
#' # Load the data
#' data(chapter_12_table_15)
#'
#' # Or, alternatively load the data as
#' data(C12T15)
#'
#' # View the structure
#' str(chapter_12_table_15)
#'
"chapter_12_table_15"


#' The data used in Chapter 12, Table 21
#'
#' The data used in Chapter 12, Table 21
#'
#' Suppose that we are interested in comparing the effects of three drugs (A, B, and C) on aggressiveness on monkeys. To control for possible order effects, we use a Latin square design. Specifically, we suppose that six subjects are available (as we discussed in Chapter 11, a subject is actually a pair of monkeys in this design). Following the design principles outlined at the end of Chapter 11, we use a replicated Latin square design with two randomly consituted squares. Subjects are then randomly assigned to rows of the squares. The dependent measure can be thought of as the number of aggressive behvaiors engaged in during a fixed time period. Notice that each sore is a function of three possible influences: subject, time period, and treatment condition (where here is drug, with three levels, either A, B, or C).
#'
#' To summarize, the data in Table 12.21 consists of hypothetical aggressiveness scores for 6 monkeys who have been exposed to three types of drugs (A, B, and C). In order to control for potential order effects, a Latin square design is utilized. In particular, two randomly constituted squares are formed with three monkeys randomly assigned to each square and also randomly assigned to the particular row of the square that assigns the order that they are exposed to treatment.
#'
#' @section Variables:
#'\describe{
#'\item{dv}{dependent variable; the number of aggressive behaviors in a time period}
#'\item{subject}{one pair of monkeys (6 total)}
#'\item{time}{time period}
#'\item{cond}{treatment condition}
#'\item{square}{a numeric vector}
#'}
#'
#' @docType data
#' @name chapter_12_table_21
#' @aliases C12T21 chapter_12_table_21 Chapter_12_Table_21 c12t21
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_12_table_21)
#' @section Synonym:
#' C12T21
#' @examples
#' # Load the data
#' data(chapter_12_table_21)
#'
#' # Or, alternatively load the data as
#' data(C12T21)
#'
#' # View the structure
#' str(chapter_12_table_21)
#'
"chapter_12_table_21"



#' The data used in Chapter 13, Table 1
#'
#' The data used in Chapter 13, Table 1
#'
#' For the hypothetical data contained in Table 13.1, five participants were measured at two occasions. The question of interest is: "is there a difference between Time 1 and Time 2 scores?"
#'
#' Table 13.1 presents hypothetical data. The null hypothesis to be tested is that population means of Time 1 and Time 2 are equal to one another. This will be tested by forming a difference score. The right-most column of Table 13.1 shows such a difference score, Time 2 score minus Time 1 score, for each subject.
#'
#' @section Variables:
#'\describe{
#'\item{time1}{participant score at time 1}
#'\item{time2}{participant score at time 2}
#'}
#'
#' @docType data
#' @name chapter_13_table_1
#' @aliases C13T1 chapter_13_table_1 Chapter_13_Table_1 c13t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_13_table_1)
#' @section Synonym:
#' C13T1
#' @examples
#' # Load the data
#' data(chapter_13_table_1)
#'
#' # Or, alternatively load the data as
#' data(C13T1)
#'
#' # View the structure
#' str(chapter_13_table_1)
#'
"chapter_13_table_1"



#' The data used in Chapter 13, Table 2
#'
#' The data used in Chapter 13, Table 2
#'
#' For the hypothetical data contained in Table 13.2, eight participants were measured at three occasions. The question of interest is: "Is there a population mean difference between across Time 1, Time 2, and Time 3 measurement occasions?" Table 13.2 presents hypothetical data for a three-level design. The null hypothesis to be tested is that the population means of scores at all three time points are equal to each.
#'
#' @section Variables:
#'\describe{
#'\item{time1}{participant score at time 1}
#'\item{time2}{participant score at time 2}
#'\item{time3}{participant score at time 3}
#'}
#'
#' @docType data
#' @name chapter_13_table_2
#' @aliases C13T2 chapter_13_table_2 Chapter_13_Table_2 c13t2
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_13_table_2)
#' @section Synonym:
#' C13T2
#' @examples
#' # Load the data
#' data(chapter_13_table_2)
#'
#' # Or, alternatively load the data as
#' data(C13T2)
#'
#' # View the structure
#' str(chapter_13_table_2)
#'
"chapter_13_table_2"



#' The data used in Chapter 13, Table 6
#'
#' The data used in Chapter 13, Table 6
#'
#' Table 13.6 presents the hypothetical McCarthy IQ scores for 12 subjects (ages 30, 36, 42, or 48 months). The McCarthy data contained in Table 13.6, which was previously analyzed in Table 11.5, is now analyzed using the multivariate approach to repeated measures.
#'
#' @section Variables:
#'\describe{
#'\item{months30}{hypothetical McCarthy IQ for 30-month-old individuals}
#'\item{months36}{hypothetical McCarthy IQ for 36-month-old individuals}
#'\item{months42}{hypothetical McCarthy IQ for 42-month-old individuals}
#'\item{months48}{hypothetical McCarthy IQ for 48-month-old individuals}
#'}
#'
#' @docType data
#' @name chapter_13_table_6
#' @aliases C13T6 chapter_13_table_6 Chapter_13_Table_6 c13t6
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_13_table_6)
#' @section Synonym:
#' C13T6
#' @examples
#' # Load the data
#' data(chapter_13_table_6)
#'
#' # Or, alternatively load the data as
#' data(C13T6)
#'
#' # View the structure
#' str(chapter_13_table_6)
#'
"chapter_13_table_6"



#' The data used in Chapter 13, Table 12
#'
#' The data used in Chapter 13, Table 12
#'
#' For the hypothetical data contained in Table 13.2, the linear and quadratic D variables were formed by making use of the appropriate coefficients from Appendix Table A.10. Because the eight participants were measured at three occasions, both a linear and a quadratic effect can be tested. The question of interest in this instance is: "is there a linear and/or quadratic trend exhibited by the group over time?" Recall that in the book (pages 646-647) it was shown that the D variables for linear and quadratic effects led to an omnibus F test of 19.148, which was a value previously obtained for the omnibus effect. Because the particular values chosen for the D variables do not matter (unless it leads to a linear combination of columns), we will focus only on the tests of the individual contrasts when analyzing the data given in Table 12. Because columns one and two already represent the linear and quadratic effect respectively, all that needs to be done is to test mean of the column in order to determine if it differs from zero.
#'
#' @section Variables:
#'\describe{
#'\item{months30}{hypothetical McCarthy IQ for 30-month-old individuals}
#'\item{months36}{hypothetical McCarthy IQ for 36-month-old individuals}
#'\item{months42}{hypothetical McCarthy IQ for 42-month-old individuals}
#'\item{months48}{hypothetical McCarthy IQ for 48-month-old individuals}
#'}
#'
#' @docType data
#' @name chapter_13_table_12
#' @aliases C13T12 chapter_13_table_12 Chapter_13_Table_12 c13t12
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_13_table_12)
#' @section Synonym:
#' C13T12
#' @examples
#' # Load the data
#' data(chapter_13_table_12)
#'
#' # Or, alternatively load the data as
#' data(C13T12)
#'
#' # View the structure
#' str(chapter_13_table_12)
#'
"chapter_13_table_12"


#' The data used in Chapter 13, Table 12
#'
#' The data used in Chapter 13, Table 12
#'
#' Table 13.14 shows the slope of the least-squares regression line for each of the eight subjects, as well as the score on the linear D variable, reproduced from Table 13.12. There is a strikin relationship between the numbers in the two columns of Table 13.14. Every subject's score on D is 24 times his or her slope.
#'
#' @section Variables:
#'\describe{
#'\item{slope}{slope of the least-squares regression line for data in Table 13.2}
#'\item{linear}{Linear D variable for data in Table 13.2}
#'}
#'
#' @docType data
#' @name chapter_13_table_12
#' @aliases C13T12 chapter_13_table_12 Chapter_13_Table_12 c13t12
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_13_table_12)
#' @section Synonym:
#' C13T12
#' @examples
#' # Load the data
#' data(chapter_13_table_12)
#'
#' # Or, alternatively load the data as
#' data(C13T12)
#'
#' # View the structure
#' str(chapter_13_table_12)
#'
"chapter_13_table_12"


#' The data used in Chapter 13, Table 14
#'
#' The data used in Chapter 13, Table 14
#'
#' Table 13.14 shows the slope of the least-squares regression line for each of the eight subjects, as well as the score on the linear D variable, reproduced from Table 13.14. There is a strikin relationship between the numbers in the two columns of Table 13.14. Every subject's score on D is 24 times his or her slope.
#'
#' @section Variables:
#'\describe{
#'\item{slope}{slope of the least-squares regression line for data in Table 13.2}
#'\item{linear}{Linear D variable for data in Table 13.2}
#'}
#'
#' @docType data
#' @name chapter_13_table_14
#' @aliases C13T14 chapter_13_table_14 Chapter_13_Table_14 c13t14
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_13_table_14)
#' @section Synonym:
#' C13T14
#' @examples
#' # Load the data
#' data(chapter_13_table_14)
#'
#' # Or, alternatively load the data as
#' data(C13T14)
#'
#' # View the structure
#' str(chapter_13_table_14)
#'
"chapter_13_table_14"


#' The data used in Chapter 14, Table 1
#'
#' The data used in Chapter 14, Table 1
#'
#' Suppose that a perceptual psychologist studying the visual system was interested in determining the extent to which interfering visual stimuli slow the ability to recognize letters. Participants are brought into a laboratory and seated in front of a tachistoscope. They are told that they will see either the letter T or the letter I displayed on the screen. In some trials, the letter appears by itself, but in other trials the target letter is embedded in a group of other letters. This variation in the display consitutes the first factor, which is referred to as noise. The noise factor has two levels - absent and present. The other factor varied by the experimenter is where in the display the target letter appears. This factor, which is called angle, also has two levels. The target letter is either shown at the center of the screen (where the participant has been told to fixate), or 8 degrees off center (with the deviation from the center randomly varying between left and right). Table 14.1 presents hypothetical data for 10 participants. As usual, the same size is kept small to minimize the computational burden. The dependent measure is reaction time (or latency) measured in milliseconds. Each participant has  four scores, one for each combination of the 2x2 design. In an actual perceptual experiment, each of these four scores would itself be the mean score for that individual across a number of trials in the particular condition.
#'
#' @section Variables:
#'\describe{
#'\item{abs0}{reaction time for participant without noise and at angle 0}
#'\item{abs8}{reaction time for participant without noise and at angle 8}
#'\item{pres0}{reaction time for participant with noise and at angle 0}
#'\item{pres8}{reaction time for participant with noise and at angle 8}
#'}
#'
#' @docType data
#' @name chapter_14_table_1
#' @aliases C14T1 chapter_14_table_1 Chapter_14_Table_1 c14t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_14_table_1)
#' @section Synonym:
#' C14T1
#' @examples
#' # Load the data
#' data(chapter_14_table_1)
#'
#' # Or, alternatively load the data as
#' data(C14T1)
#'
#' # View the structure
#' str(chapter_14_table_1)
#'
"chapter_14_table_1"


#' The data used in Chapter 14, Table 3
#'
#' The data used in Chapter 14, Table 3
#'
#' Using the data in Table 14.1, we could average scores for each participant individually because the noise factor we need to average over is a within-subjects factor. For example, participant 1's average 0 degree score is 450, whereas his or her 8 degree score is 630. This participant's reaction time averages 180 msec longer (630 vs 450) for the 8 degree condition than the 0 degree condition. If the other 9 participants' data show a similar pattern, we would infer that there is indeed a main effect due to angle.
#'
#' For the hypothetical data contained in Table 14.1, Table 14.3 gives the set of D variables. The D variables are subsequently used to analyze the data given in Table 14.1. Recall that we analyzed the data contained in Table 14.1 directly using SPSS without (explicitly) forming D variables. Although obtaining the results of the main effects is easily accomplished using the data directly, forming and then analyzing D variables directly also has its benefits (which are delineated in the chapter). Below we analyze the D variables contained in Table 14.3. As expected, our results will match those previously obtained when we analyzed the raw data (i.e., skipping the step of explicitly forming D variables). However, the method to be outlined here provides a different way to accomplish the same goal. We will soon see that analyzing the data by explicitly forming D variables has its advantages.The first column of Table 14.3 (D1) shows these scores for all 10 participants. Indeed, all 10 participants have an average 8 degree reaction time that is slower than their average 0 degree reaction time. Such consistency strongly supports the existence of an angle main effect.
#'
#' @section Variables:
#'\describe{
#'\item{d1}{participant D1 difference score averaged over noise}
#'\item{d2}{participant D2 difference score averaged over noise}
#'\item{d3}{participant D3 difference score averaged over noise}
#'}
#'
#' @docType data
#' @name chapter_14_table_3
#' @aliases C14T3 chapter_14_table_3 Chapter_14_Table_3 c14t3
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_14_table_3)
#' @section Synonym:
#' C14T3
#' @examples
#' # Load the data
#' data(chapter_14_table_3)
#'
#' # Or, alternatively load the data as
#' data(C14T3)
#'
#' # View the structure
#' str(chapter_14_table_3)
#'
"chapter_14_table_3"


#' The data used in Chapter 14, Table 4
#'
#' The data used in Chapter 14, Table 4
#'
#' Suppose a perceptual psychologist studying the visual system was interested in determining the extent to which interfering visual stimuli slow the ability to recognize letters. Participants are brought into a laboratory where they are seated in front of a tachistoscope. Variations in the presentations of letters is examined with interest being on the reaction time for target letters presented either in the center of the screen or off centered with and without "noise" accompanying the target letters.
#'
#' @section Variables:
#'\describe{
#'    \item{abs0}{participant reaction time without noise and with angle 0}
#'    \item{abs4}{participant reaction time without noise and with angle 4}
#'    \item{abs8}{participant reaction time without noise and with angle 8}
#'    \item{pres0}{participant reaction time with noise and with angle 0}
#'    \item{pres4}{participant reaction time with noise and with angle 4}
#'    \item{pres8}{participant reaction time with noise and with angle 8}
#'}
#'
#' @docType data
#' @name chapter_14_table_4
#' @aliases C14T4 chapter_14_table_4 Chapter_14_Table_4 c14t4
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_14_table_4)
#' @section Synonym:
#' C14T4
#' @examples
#' # Load the data
#' data(chapter_14_table_4)
#'
#' # Or, alternatively load the data as
#' data(C14T4)
#'
#' # View the structure
#' str(chapter_14_table_4)
#'
"chapter_14_table_4"


#' The data used in Chapter 14, Table 5
#'
#' The data used in Chapter 14, Table 5
#'
#' In terms of symbols, let D(1i) represent the linear trend for a given angle. For the hypothetical data contained in Table 14.4, Table 14.5 gives an appropriate and substantively interesting set of D variables. The D variables (rather than the raw data itself) is used because of the benefits and flexibility gained from analyzing the D variables directly (rather than indirectly as we did with the Table 14.4 data).
#'
#' @section Variables:
#'\describe{
#'\item{d1}{participant D1 variable}
#'\item{d2}{participant D2 variable}
#'\item{d3}{participant D3 variable}
#'\item{d4}{participant D4 variable}
#'\item{d5}{participant D5 variable}
#'}
#'
#' @docType data
#' @name chapter_14_table_5
#' @aliases C14T5 chapter_14_table_5 Chapter_14_Table_5 c14t5
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_14_table_5)
#' @section Synonym:
#' C14T5
#' @examples
#' # Load the data
#' data(chapter_14_table_5)
#'
#' # Or, alternatively load the data as
#' data(C14T5)
#'
#' # View the structure
#' str(chapter_14_table_5)
#'
"chapter_14_table_5"


#' The data used in Chapter 14, Table 8
#'
#' The data used in Chapter 14, Table 8
#'
#' For the hypothetical data contained in Table 14.8, a perceptual psychologist is interested in age differences ("young" and "old") in reaction time on a perceptual task. In addition, the psychologist is also interested in the effect of angle (zero degrees off center and eight degrees off center). The question of interest is to see if there are is a main effect of age, a main effect of angle, and an interaction between the two. Table 14.8 presents the same data that we analyzed in chapter 12 for 10 young participants and 10 old participants, except that for the moment we are only analyzing data from the 0 degree and 8 degree conditions of the angle factor.
#'
#' In any two factor design, the effects to be tested are typically the two main effects and the two-way interaction. In our example, then, we test the main effect of age (designated A), the main effect of angle (designated B), and the interaction of age and angle.
#'
#' @section Variables:
#'\describe{
#'\item{angle0}{participant reaction time at angle 0}
#'\item{angle8}{participant reaction time at angle 8}
#'\item{group}{participant age (young or old)}
#'}
#'
#' @docType data
#' @name chapter_14_table_8
#' @aliases C14T8 chapter_14_table_8 Chapter_14_Table_8 c14t8
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_14_table_8)
#' @section Synonym:
#' C14T8
#' @examples
#' # Load the data
#' data(chapter_14_table_8)
#'
#' # Or, alternatively load the data as
#' data(C14T8)
#'
#' # View the structure
#' str(chapter_14_table_8)
#'
"chapter_14_table_8"


#' The data used in Chapter 14, Table 10
#'
#' The data used in Chapter 14, Table 10
#'
#' These data are identical to those analyzed in chapter 12 (see Tables 12.7 and 12.15) to facilitate comparisons of the multivariate approach and the mixed-model approach.The hypothetical data contained in Table 14.10 contains an additional level of angle (four degrees) that was not considered in Table 14.8.
#'
#' @section Variables:
#'\describe{
#'\item{angle0}{participant reaction time at angle 0}
#'\item{angle4}{participant reaction time at angle 4}
#'\item{angle8}{participant reaction time at angle 8}
#'\item{group}{participant group number}
#'}
#'
#' @docType data
#' @name chapter_14_table_10
#' @aliases C14T10 chapter_14_table_10 Chapter_14_Table_10 c14t10
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_14_table_10)
#' @section Synonym:
#' C14T10
#' @examples
#' # Load the data
#' data(chapter_14_table_10)
#'
#' # Or, alternatively load the data as
#' data(C14T10)
#'
#' # View the structure
#' str(chapter_14_table_10)
#'
"chapter_14_table_10"



#' The data used in Chapter 15, Table 1
#'
#' The data used in Chapter 15, Table 1
#'
#' The first table in Chapter 15 presents the Hypothetical McCarthy data again (originally shown in Table 11.5). This data set is used throughout the chapter as the to illustrate the discussion given on the mixed model. At the time of this writing the mixed model procedure is SPSS is not as fully developed the SAS analog PROC MIXED. For this reason no analyses using SPSS are provided to replicate the analyses given in Chapter 15.
#'
#'@section Variables:
#' \describe{
#' \item{months30}{McCarthy IQ score for 30-month-old}
#' \item{months36}{McCarthy IQ score for 36-month-old}
#' \item{months42}{McCarthy IQ score for 42-month-old}
#' \item{months48}{McCarthy IQ score for 48-month-old}}
#'
#'
#' @docType data
#' @name chapter_15_table_1
#' @aliases C15T1 chapter_15_table_1 Chapter_15_Table_1 c15t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_15_table_1)
#' @section Synonym:
#' C15T1
#' @examples
#' # Load the data
#' data(chapter_15_table_1)
#'
#' # Or, alternatively load the data as
#' data(C15T1)
#'
#' # View the structure
#' str(chapter_15_table_1)
#'
"chapter_15_table_1"


#' The data used in Chapter 16, Table 1
#'
#' The data used in Chapter 16, Table 1
#'
#' The first table in Chapter 16 presents the Severity Ratings by Clinical Trainees, which was originally given in Table 10.9. The data set is analyzed again using the multilevel model approach and the results are compared with those obtained in Chapter 10. As a brief background, the goal of the study here is to examine the extent to which female and male clinical psychology graduate student trainees may assign different severity ratings to clients at initial intake.Three female and three male graduate students are randomly selected to participate and each is randomly assigned four clients with whom to do an intake interview, after which each clinical trainee assigns a severity rating to each client, producing the data shown in Table 16.1.
#'
#'@section Variables:
#' \describe{
#' \item{severity}{severity rating assigned to client by trainee}
#' \item{trainee}{trainee}
#' \item{gender}{gender of trainee}}
#'
#' @docType data
#' @name chapter_16_table_1
#' @aliases C16T1 chapter_16_table_1 Chapter_16_Table_1 c16t1
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_16_table_1)
#' @section Synonym:
#' C16T1
#' @examples
#' # Load the data
#' data(chapter_16_table_1)
#'
#' # Or, alternatively load the data as
#' data(C16T1)
#'
#' # View the structure
#' str(chapter_16_table_1)
#'
"chapter_16_table_1"


#' The data used in Chapter 16, Table 4
#'
#' The data used in Chapter 16, Table 4
#'
#' The hypothetical data contained in Table 16.4 is supposed to represent the data from 29 children who participated in a study to evaluate the effectiveness of an intervention designed to increase inductive reasoning skills. The data consists of children who are nested within one of six classrooms, where each classroom contained students from both the control and the experimental condition. The question of interest is whether or not the children who participated in the experimental group actually improved their cognitive reasoning ability.
#'
#' The children with condition values of 0 received the control, whereas the 14 children with condition values of 1 received the treatment. 4 of the children in the control condition were students in control Classroom 1, 6 of them were students in control Classroom 2, and 5 were students in control Classroom 3. Similarly, 3 of the students in the treatment condition were students in treatment Classroom 1, 5 were students in treatment Classroom 2, and 6 were students in treatment Classroom 3. It is also important to note that scores on the dependent variable appear in the rightmost column under the variable label "induct". The variable labeled "cog" in Table 16.4 represents cognitive ability scores that have been obtained for each student sometime prior to assigning classrooms to treatment conditions. The variable labeled "skill" represents a global measure of each teacher's teaching skill, once again assigned prior to assigning classrooms to treatment conditions.
#'
#'@section Variables:
#' \describe{
#' \item{obs}{observation/participant number}
#' \item{room}{participant classroom placement}
#' \item{cond}{participant condition (0=control, 1=treatment)}
#' \item{cog}{participant cognitive ability score}
#' \item{skill}{participant's teacher's teaching skill}
#' \item{induct}{induction; scores on the dependent variable}}
#'
#' @docType data
#' @name chapter_16_table_4
#' @aliases C16T4 chapter_16_table_4 Chapter_16_Table_4 c16t4
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_16_table_4)
#' @section Synonym:
#' C16T4
#' @examples
#' # Load the data
#' data(chapter_16_table_4)
#'
#' # Or, alternatively load the data as
#' data(C16T4)
#'
#' # View the structure
#' str(chapter_16_table_4)
#'
"chapter_16_table_4"




# Exercises
#######################################################################################################################################

#' The data used in Chapter 1, Exercise 18
#'
#' Data from Chapter 1 Exercise 18 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Promoted.
#' \item Minority.
#' \item Freq. Frequency}
#'
#' @docType data
#' @name chapter_1_exercise_18
#' @aliases C1E18 chapter_1_exercise_18 Chapter_1_Exercise_18 c1e18
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @keywords datasets
#' @usage data(chapter_1_exercise_18)
#' @section Synonym:
#' C1E18
#' @examples
#' # Load the data
#' data(chapter_1_exercise_18)
#'
#' # Or, alternatively load the data as
#' data(C1E18)
#'
#' # View the structure
#' str(chapter_1_exercise_18)
#'
#' # Brief summary of the data.
#' summary(chapter_1_exercise_18)
#'
"chapter_1_exercise_18"


#' The data used in Chapter 1, Exercise 19
#'
#' Data from Chapter 1 Exercise 19 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Convicted.
#' \item Monozygotic.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_1_exercise_19
#' @aliases chapter_1_exercise_19 Chapter_1_Exercise_19 C1E19 c1e19
#' @keywords datasets
#' @usage data(chapter_1_exercise_19)
#' @section Synonym:
#' C1E19
#' @examples
#' # Load the data
#' data(chapter_1_exercise_19)
#'
#' # Or, alternatively load the data as
#' data(C1E19)
#'
#' # View the structure
#' str(chapter_1_exercise_19)
#'
#' # Brief summary of the data.
#' summary(chapter_1_exercise_19)
#'
"chapter_1_exercise_19"


#' The data used in Chapter 1, Exercise 21
#'
#' Data from Chapter 1 Exercise 21 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Experimental.
#' \item Control.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_1_exercise_21
#' @aliases chapter_1_exercise_21 Chapter_1_Exercise_21 C1E21 c1e21
#' @keywords datasets
#' @usage data(chapter_1_exercise_21)
#' @section Synonym:
#' C1E21
#' @examples
#' # Load the data
#' data(chapter_1_exercise_21)
#'
#' # Or, alternatively load the data as
#' data(C1E21)
#'
#' # View the structure
#' str(chapter_1_exercise_21)
#'
#' # Brief summary of the data.
#' summary(chapter_1_exercise_21)
#'
"chapter_1_exercise_21"


#' The data used in Chapter 1, Exercise 22
#'
#' Data from Chapter 1 Exercise 22 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Pot.
#' \item Crossed.
#' \item SelfFertilized.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_1_exercise_22
#' @aliases chapter_1_exercise_22 Chapter_1_Exercise_22 C1E22 c1e22
#' @keywords datasets
#' @usage data(chapter_1_exercise_22)
#' @section Synonym:
#' C1E22
#' @examples
#' # Load the data
#' data(chapter_1_exercise_22)
#'
#' # Or, alternatively load the data as
#' data(C1E22)
#'
#' # View the structure
#' str(chapter_1_exercise_22)
#'
#' # Brief summary of the data.
#' summary(chapter_1_exercise_22)
#'
"chapter_1_exercise_22"


#' The data used in Chapter 1, Exercise 23
#'
#' Data from Chapter 1 Exercise 23 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Group.
#' \item Cholesterol.
#' \item Cholesterol_Category_MedianSplit.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_1_exercise_23
#' @aliases chapter_1_exercise_23 Chapter_1_Exercise_23 C1E23 c1e23
#' @keywords datasets
#' @usage data(chapter_1_exercise_23)
#' @section Synonym:
#' C1E23
#' @examples
#' # Load the data
#' data(chapter_1_exercise_23)
#'
#' # Or, alternatively load the data as
#' data(C1E23)
#'
#' # View the structure
#' str(chapter_1_exercise_23)
#'
#' # Brief summary of the data.
#' summary(chapter_1_exercise_23)
#'
"chapter_1_exercise_23"


#' The data used in Chapter 3, Exercise 9
#'
#' Data from Chapter 3 Exercise 9 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Group.
#' \item Scores.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_3_exercise_9
#' @aliases chapter_3_exercise_9 Chapter_3_Exercise_9 C3E9 c3e9
#' @keywords datasets
#' @usage data(chapter_3_exercise_9)
#' @section Synonym:
#' C3E9
#' @examples
#' # Load the data
#' data(chapter_3_exercise_9)
#'
#' # Or, alternatively load the data as
#' data(C3E9)
#'
#' # View the structure
#' str(chapter_3_exercise_9)
#'
#' # Brief summary of the data.
#' summary(chapter_3_exercise_9)
#'
"chapter_3_exercise_9"


#' The data used in Chapter 3, Exercise 10
#'
#' Data from Chapter 3 Exercise 10 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Experiment.
#' \item Experimental.
#' \item Control.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_3_exercise_10
#' @aliases chapter_3_exercise_10 Chapter_3_Exercise_10 C3E10 c3e10
#' @keywords datasets
#' @usage data(chapter_3_exercise_10)
#' @section Synonym:
#' C3E10
#' @examples
#' # Load the data
#' data(chapter_3_exercise_10)
#'
#' # Or, alternatively load the data as
#' data(C3E10)
#'
#' # View the structure
#' str(chapter_3_exercise_10)
#'
#' # Brief summary of the data.
#' summary(chapter_3_exercise_10)
#'
"chapter_3_exercise_10"


#' The data used in Chapter 3, Exercise 11
#'
#' Data from Chapter 3 Exercise 11 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Condition.
#' \item Scores.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_3_exercise_11
#' @aliases chapter_3_exercise_11 Chapter_3_Exercise_11 C3E11 c3e11
#' @keywords datasets
#' @usage data(chapter_3_exercise_11)
#' @section Synonym:
#' C3E11
#' @examples
#' # Load the data
#' data(chapter_3_exercise_11)
#'
#' # Or, alternatively load the data as
#' data(C3E11)
#'
#' # View the structure
#' str(chapter_3_exercise_11)
#'
#' # Brief summary of the data.
#' summary(chapter_3_exercise_11)
#'
"chapter_3_exercise_11"


#' The data used in Chapter 3, Exercise 19
#'
#' Data from Chapter 3 Exercise 19 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item Condition.
#' \item Anger.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_3_exercise_19
#' @aliases chapter_3_exercise_19 Chapter_3_Exercise_19 C3E19 c3e19
#' @keywords datasets
#' @usage data(chapter_3_exercise_19)
#' @section Synonym:
#' C3E19
#' @examples
#' # Load the data
#' data(chapter_3_exercise_19)
#'
#' # Or, alternatively load the data as
#' data(C3E19)
#'
#' # View the structure
#' str(chapter_3_exercise_19)
#'
#' # Brief summary of the data.
#' summary(chapter_3_exercise_19)
#'
"chapter_3_exercise_19"


#' The data used in Chapter 3, Exercise 20
#'
#' Data from Chapter 3 Exercise 20 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item Condition.
#' \item Anger.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_3_exercise_20
#' @aliases chapter_3_exercise_20 Chapter_3_Exercise_20 C3E20 c3e20
#' @keywords datasets
#' @usage data(chapter_3_exercise_20)
#' @section Synonym:
#' C3E20
#' @examples
#' # Load the data
#' data(chapter_3_exercise_20)
#'
#' # Or, alternatively load the data as
#' data(C3E20)
#'
#' # View the structure
#' str(chapter_3_exercise_20)
#'
#' # Brief summary of the data.
#' summary(chapter_3_exercise_20)
#'
"chapter_3_exercise_20"

#' The data used in Chapter 3, Exercise 21
#'
#' Data from Chapter 3 Exercise 21 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Condition.
#' \item Exercise.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_3_exercise_21
#' @aliases chapter_3_exercise_21 Chapter_3_Exercise_21 C3E21 c3e21
#' @keywords datasets
#' @usage data(chapter_3_exercise_21)
#' @section Synonym:
#' C3E21
#' @examples
#' # Load the data
#' data(chapter_3_exercise_21)
#'
#' # Or, alternatively load the data as
#' data(C3E21)
#'
#' # View the structure
#' str(chapter_3_exercise_21)
#'
#' # Brief summary of the data.
#' summary(chapter_3_exercise_21)
#'
"chapter_3_exercise_21"

#' The data used in Chapter 3, Exercise 22
#'
#' Data from Chapter 3 Exercise 22 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Grade.
#' \item Treatment.
#' \item IQPre.
#' \item IQ4.
#' \item IQ8.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_3_exercise_22
#' @aliases chapter_3_exercise_22 Chapter_3_Exercise_22 C3E22 c3e22
#' @keywords datasets
#' @usage data(chapter_3_exercise_22)
#' @section Synonym:
#' C3E22
#' @examples
#' # Load the data
#' data(chapter_3_exercise_22)
#'
#' # Or, alternatively load the data as
#' data(C3E22)
#'
#' # View the structure
#' str(chapter_3_exercise_22)
#'
#' # Brief summary of the data.
#' summary(chapter_3_exercise_22)
#'
"chapter_3_exercise_22"

#' The data used in Chapter 4, Exercise 11
#'
#' Data from Chapter 4 Exercise 11 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item dv.
#' \item cond.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_4_exercise_11
#' @aliases chapter_4_exercise_11 Chapter_4_Exercise_11 C4E11 c4e11
#' @keywords datasets
#' @usage data(chapter_4_exercise_11)
#' @section Synonym:
#' C4E11
#' @examples
#' # Load the data
#' data(chapter_4_exercise_11)
#'
#' # Or, alternatively load the data as
#' data(C4E11)
#'
#' # View the structure
#' str(chapter_4_exercise_11)
#'
#' # Brief summary of the data.
#' summary(chapter_4_exercise_11)
#'
"chapter_4_exercise_11"

#' The data used in Chapter 4, Exercise 12
#'
#' Data from Chapter 4 Exercise 12 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item group.
#' \item y.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_4_exercise_12
#' @aliases chapter_4_exercise_12 Chapter_4_Exercise_12 C4E12 c4e12
#' @keywords datasets
#' @usage data(chapter_4_exercise_12)
#' @section Synonym:
#' C4E12
#' @examples
#' # Load the data
#' data(chapter_4_exercise_12)
#'
#' # Or, alternatively load the data as
#' data(C4E12)
#'
#' # View the structure
#' str(chapter_4_exercise_12)
#'
#' # Brief summary of the data.
#' summary(chapter_4_exercise_12)
#'
"chapter_4_exercise_12"

#' The data used in Chapter 4, Exercise 13
#'
#' Data from Chapter 4 Exercise 13 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item dv.
#' \item cond.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_4_exercise_13
#' @aliases chapter_4_exercise_13 Chapter_4_Exercise_13 C4E13 c4e13
#' @keywords datasets
#' @usage data(chapter_4_exercise_12)
#' @section Synonym:
#' C4E13
#' @examples
#' # Load the data
#' data(chapter_4_exercise_13)
#'
#' # Or, alternatively load the data as
#' data(C4E13)
#'
#' # View the structure
#' str(chapter_4_exercise_13)
#'
#' # Brief summary of the data.
#' summary(chapter_4_exercise_13)
#'
"chapter_4_exercise_13"

#' The data used in Chapter 5, Exercise 5
#'
#' Data from Chapter 5 Exercise 5 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item dv.
#' \item cond.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_5_exercise_5
#' @aliases chapter_5_exercise_5 Chapter_5_Exercise_5 C5E5 c5e5
#' @keywords datasets
#' @usage data(chapter_5_exercise_5)
#' @section Synonym:
#' C5E5
#' @examples
#' # Load the data
#' data(chapter_5_exercise_5)
#'
#' # Or, alternatively load the data as
#' data(C5E5)
#'
#' # View the structure
#' str(chapter_5_exercise_5)
#'
#' # Brief summary of the data.
#' summary(chapter_5_exercise_5)
#'
"chapter_5_exercise_5"

#' The data used in Chapter 5, Exercise 10
#'
#' Data from Chapter 5 Exercise 10 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item cond.
#' \item score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_5_exercise_10
#' @aliases chapter_5_exercise_10 Chapter_5_Exercise_10 C5E10 c5e10
#' @keywords datasets
#' @usage data(chapter_5_exercise_10)
#' @section Synonym:
#' C5E10
#' @examples
#' # Load the data
#' data(chapter_5_exercise_10)
#'
#' # Or, alternatively load the data as
#' data(C5E10)
#'
#' # View the structure
#' str(chapter_5_exercise_10)
#'
#' # Brief summary of the data.
#' summary(chapter_5_exercise_10)
#'
"chapter_5_exercise_10"

#' The data used in Chapter 5, Exercise 16
#'
#' Data from Chapter 5 Exercise 16 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item cond.
#' \item scores.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_5_exercise_16
#' @aliases chapter_5_exercise_16 Chapter_5_Exercise_16 C5E16 c5e16
#' @keywords datasets
#' @usage data(chapter_5_exercise_16)
#' @section Synonym:
#' C5E16
#' @examples
#' # Load the data
#' data(chapter_5_exercise_16)
#'
#' # Or, alternatively load the data as
#' data(C5E16)
#'
#' # View the structure
#' str(chapter_5_exercise_16)
#'
#' # Brief summary of the data.
#' summary(chapter_5_exercise_16)
#'
"chapter_5_exercise_16"

#' The data used in Chapter 6, Exercise 10
#'
#' Data from Chapter 6 Exercise 10 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item cond.
#' \item score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_6_exercise_10
#' @aliases chapter_6_exercise_10 Chapter_6_Exercise_10 C6E10 c6e10
#' @keywords datasets
#' @usage data(chapter_6_exercise_10)
#' @section Synonym:
#' C6E10
#' @examples
#' # Load the data
#' data(chapter_6_exercise_10)
#'
#' # Or, alternatively load the data as
#' data(C6E10)
#'
#' # View the structure
#' str(chapter_6_exercise_10)
#'
#' # Brief summary of the data.
#' summary(chapter_6_exercise_10)
#'
"chapter_6_exercise_10"

#' The data used in Chapter 6, Exercise 14
#'
#' Data from Chapter 6 Exercise 14 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Proportion.
#' \item Months.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_6_exercise_14
#' @aliases chapter_6_exercise_14 Chapter_6_Exercise_14 C6E14 c6e14
#' @keywords datasets
#' @usage data(chapter_6_exercise_14)
#' @section Synonym:
#' C6E14
#' @examples
#' # Load the data
#' data(chapter_6_exercise_14)
#'
#' # Or, alternatively load the data as
#' data(C6E14)
#'
#' # View the structure
#' str(chapter_6_exercise_14)
#'
#' # Brief summary of the data.
#' summary(chapter_6_exercise_14)
#'
"chapter_6_exercise_14"

#' The data used in Chapter 6, Exercise 16
#'
#' Data from Chapter 6 Exercise 16 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item id.
#' \item group.
#' \item y.
#' \item latency.
#' \item latency_2.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_6_exercise_16
#' @aliases chapter_6_exercise_16 Chapter_6_Exercise_16 C6E16 c6e16
#' @keywords datasets
#' @usage data(chapter_6_exercise_16)
#' @section Synonym:
#' C6E16
#' @examples
#' # Load the data
#' data(chapter_6_exercise_16)
#'
#' # Or, alternatively load the data as
#' data(C6E16)
#'
#' # View the structure
#' str(chapter_6_exercise_16)
#'
#' # Brief summary of the data.
#' summary(chapter_6_exercise_16)
#'
"chapter_6_exercise_16"

#' The data used in Chapter 7, Exercise 6
#'
#' Data from Chapter 7 Exercise 6 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Treatment.
#' \item Level.
#' \item Score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_6
#' @aliases chapter_7_exercise_6 Chapter_7_Exercise_6 C7E6 c7e6
#' @keywords datasets
#' @usage data(chapter_7_exercise_6)
#' @section Synonym:
#' C7E6
#' @examples
#' # Load the data
#' data(chapter_7_exercise_6)
#'
#' # Or, alternatively load the data as
#' data(C7E6)
#'
#' # View the structure
#' str(chapter_7_exercise_6)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_6)
#'
"chapter_7_exercise_6"

#' The data used in Chapter 7, Exercise 9
#'
#' Data from Chapter 7 Exercise 9 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Treatment.
#' \item Level.
#' \item Score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_9
#' @aliases chapter_7_exercise_9 Chapter_7_Exercise_9 C7E9 c7e9
#' @keywords datasets
#' @usage data(chapter_7_exercise_9)
#' @section Synonym:
#' C7E9
#' @examples
#' # Load the data
#' data(chapter_7_exercise_9)
#'
#' # Or, alternatively load the data as
#' data(C7E9)
#'
#' # View the structure
#' str(chapter_7_exercise_9)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_9)
#'
"chapter_7_exercise_9"

#' The data used in Chapter 7, Exercise 12
#'
#' Data from Chapter 7 Exercise 12 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ALevel.
#' \item BLevel.
#' \item Score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_12
#' @aliases chapter_7_exercise_12 Chapter_7_Exercise_12 C7E12 c7e12
#' @keywords datasets
#' @usage data(chapter_7_exercise_12)
#' @section Synonym:
#' C7E12
#' @examples
#' # Load the data
#' data(chapter_7_exercise_12)
#'
#' # Or, alternatively load the data as
#' data(C7E12)
#'
#' # View the structure
#' str(chapter_7_exercise_12)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_12)
#'
"chapter_7_exercise_12"

#' The data used in Chapter 7, Exercise 13
#'
#' Data from Chapter 7 Exercise 13 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Age.
#' \item Gender.
#' \item Score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_13
#' @aliases chapter_7_exercise_13 Chapter_7_Exercise_13 C7E13 c7e13
#' @keywords datasets
#' @usage data(chapter_7_exercise_13)
#' @section Synonym:
#' C7E13
#' @examples
#' # Load the data
#' data(chapter_7_exercise_13)
#'
#' # Or, alternatively load the data as
#' data(C7E13)
#'
#' # View the structure
#' str(chapter_7_exercise_13)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_13)
#'
"chapter_7_exercise_13"

#' The data used in Chapter 7, Exercise 14
#'
#' Data from Chapter 7 Exercise 14 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item cond.
#' \item status.
#' \item score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_14
#' @aliases chapter_7_exercise_14 Chapter_7_Exercise_14 C7E14 c7e14
#' @keywords datasets
#' @usage data(chapter_7_exercise_14)
#' @section Synonym:
#' C7E14
#' @examples
#' # Load the data
#' data(chapter_7_exercise_14)
#'
#' # Or, alternatively load the data as
#' data(C7E14)
#'
#' # View the structure
#' str(chapter_7_exercise_14)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_14)
#'
"chapter_7_exercise_14"

#' The data used in Chapter 7, Exercise 15
#'
#' Data from Chapter 7 Exercise 15 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Gender.
#' \item Cond.
#' \item Score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_15
#' @aliases chapter_7_exercise_15 Chapter_7_Exercise_15 C7E15 c7e15
#' @keywords datasets
#' @usage data(chapter_7_exercise_15)
#' @section Synonym:
#' C7E15
#' @examples
#' # Load the data
#' data(chapter_7_exercise_15)
#'
#' # Or, alternatively load the data as
#' data(C7E15)
#'
#' # View the structure
#' str(chapter_7_exercise_15)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_15)
#'
"chapter_7_exercise_15"

#' The data used in Chapter 7, Exercise 18
#'
#' Data from Chapter 7 Exercise 18 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item level.
#' \item gender.
#' \item salary.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_18
#' @aliases chapter_7_exercise_18 Chapter_7_Exercise_18 C7E18 c7e18
#' @keywords datasets
#' @usage data(chapter_7_exercise_18)
#' @section Synonym:
#' C7E18
#' @examples
#' # Load the data
#' data(chapter_7_exercise_18)
#'
#' # Or, alternatively load the data as
#' data(C7E18)
#'
#' # View the structure
#' str(chapter_7_exercise_18)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_18)
#'
"chapter_7_exercise_18"

#' The data used in Chapter 7, Exercise 19
#'
#' Data from Chapter 7 Exercise 19 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item race.
#' \item courses.
#' \item scores.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_19
#' @aliases chapter_7_exercise_19 Chapter_7_Exercise_19 C7E19 c7e19
#' @keywords datasets
#' @usage data(chapter_7_exercise_19)
#' @section Synonym:
#' C7E19
#' @examples
#' # Load the data
#' data(chapter_7_exercise_19)
#'
#' # Or, alternatively load the data as
#' data(C7E19)
#'
#' # View the structure
#' str(chapter_7_exercise_19)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_19)
#'
"chapter_7_exercise_19"

#' The data used in Chapter 7, Exercise 22
#'
#' Data from Chapter 7 Exercise 22 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item id.
#' \item bpd.
#' \item drug.
#' \item trust.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_22
#' @aliases chapter_7_exercise_22 Chapter_7_Exercise_22 C7E22 c7e22
#' @keywords datasets
#' @usage data(chapter_7_exercise_22)
#' @section Synonym:
#' C7E22
#' @examples
#' # Load the data
#' data(chapter_7_exercise_22)
#'
#' # Or, alternatively load the data as
#' data(C7E22)
#'
#' # View the structure
#' str(chapter_7_exercise_22)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_22)
#'
"chapter_7_exercise_22"

#' The data used in Chapter 7, Exercise 23
#'
#' Data from Chapter 7 Exercise 23 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item id.
#' \item esteem.
#' \item cond.
#' \item mood.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_23
#' @aliases chapter_7_exercise_23 Chapter_7_Exercise_23 C7E23 c7e23
#' @keywords datasets
#' @usage data(chapter_7_exercise_23)
#' @section Synonym:
#' C7E23
#' @examples
#' # Load the data
#' data(chapter_7_exercise_23)
#'
#' # Or, alternatively load the data as
#' data(C7E23)
#'
#' # View the structure
#' str(chapter_7_exercise_23)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_23)
#'
"chapter_7_exercise_23"

#' The data used in Chapter 7, Exercise 24
#'
#' Data from Chapter 7 Exercise 24 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item id.
#' \item thought.
#' \item complexity.
#' \item attitude.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_24
#' @aliases chapter_7_exercise_24 Chapter_7_Exercise_24 C7E24 c7e24
#' @keywords datasets
#' @usage data(chapter_7_exercise_24)
#' @section Synonym:
#' C7E24
#' @examples
#' # Load the data
#' data(chapter_7_exercise_24)
#'
#' # Or, alternatively load the data as
#' data(C7E24)
#'
#' # View the structure
#' str(chapter_7_exercise_24)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_24)
#'
"chapter_7_exercise_24"

#' The data used in Chapter 7, Exercise 25
#'
#' Data from Chapter 7 Exercise 25 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item id.
#' \item switch.
#' \item cond.
#' \item change.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_7_exercise_25
#' @aliases chapter_7_exercise_25 Chapter_7_Exercise_25 C7E25 c7e25
#' @keywords datasets
#' @usage data(chapter_7_exercise_25)
#' @section Synonym:
#' C7E25
#' @examples
#' # Load the data
#' data(chapter_7_exercise_25)
#'
#' # Or, alternatively load the data as
#' data(C7E25)
#'
#' # View the structure
#' str(chapter_7_exercise_25)
#'
#' # Brief summary of the data.
#' summary(chapter_7_exercise_25)
#'
"chapter_7_exercise_25"

#' The data used in Chapter 8, Exercise 15
#'
#' Data from Chapter 8 Exercise 15 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ProportionTime.
#' \item Parent.
#' \item Child.
#' \item Months.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_8_exercise_15
#' @aliases chapter_8_exercise_15 Chapter_8_Exercise_15 C8E15 c8e15
#' @keywords datasets
#' @usage data(chapter_8_exercise_15)
#' @section Synonym:
#' C8E15
#' @examples
#' # Load the data
#' data(chapter_8_exercise_15)
#'
#' # Or, alternatively load the data as
#' data(C8E15)
#'
#' # View the structure
#' str(chapter_8_exercise_15)
#'
#' # Brief summary of the data.
#' summary(chapter_8_exercise_15)
#'
"chapter_8_exercise_15"

#' The data used in Chapter 8, Exercise 16
#'
#' Data from Chapter 8 Exercise 16 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Value.
#' \item Monitors.
#' \item Argument.
#' \item Source.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_8_exercise_16
#' @aliases chapter_8_exercise_16 Chapter_8_Exercise_16 C8E16 c8e16
#' @keywords datasets
#' @usage data(chapter_8_exercise_16)
#' @section Synonym:
#' C8E16
#' @examples
#' # Load the data
#' data(chapter_8_exercise_16)
#'
#' # Or, alternatively load the data as
#' data(C8E16)
#'
#' # View the structure
#' str(chapter_8_exercise_16)
#'
#' # Brief summary of the data.
#' summary(chapter_8_exercise_16)
#'
"chapter_8_exercise_16"

#' The data used in Chapter 8, Exercise 17
#'
#' Data from Chapter 8 Exercise 17 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item BehavioralAvoidance.
#' \item Condition.
#' \item Phobia.
#' \item Gender.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_8_exercise_17
#' @aliases chapter_8_exercise_17 Chapter_8_Exercise_17 C8E17 c8e17
#' @keywords datasets
#' @usage data(chapter_8_exercise_17)
#' @section Synonym:
#' C8E17
#' @examples
#' # Load the data
#' data(chapter_8_exercise_17)
#'
#' # Or, alternatively load the data as
#' data(C8E17)
#'
#' # View the structure
#' str(chapter_8_exercise_17)
#'
#' # Brief summary of the data.
#' summary(chapter_8_exercise_17)
#'
"chapter_8_exercise_17"

#' The data used in Chapter 8, Exercise 18
#'
#' Data from Chapter 8 Exercise 18 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item Partner.
#' \item Report.
#' \item Focus.
#' \item Negativity.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_8_exercise_18
#' @aliases chapter_8_exercise_18 Chapter_8_Exercise_18 C8E18 c8e18
#' @keywords datasets
#' @usage data(chapter_8_exercise_18)
#' @section Synonym:
#' C8E18
#' @examples
#' # Load the data
#' data(chapter_8_exercise_18)
#'
#' # Or, alternatively load the data as
#' data(C8E18)
#'
#' # View the structure
#' str(chapter_8_exercise_18)
#'
#' # Brief summary of the data.
#' summary(chapter_8_exercise_18)
#'
"chapter_8_exercise_18"

#' The data used in Chapter 8, Exercise 19
#'
#' Data from Chapter 8 Exercise 19 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item Gender.
#' \item Audience.
#' \item Presentation.
#' \item Persistence.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_8_exercise_19
#' @aliases chapter_8_exercise_19 Chapter_8_Exercise_19 C8E19 c8e19
#' @keywords datasets
#' @usage data(chapter_8_exercise_19)
#' @section Synonym:
#' C8E19
#' @examples
#' # Load the data
#' data(chapter_8_exercise_19)
#'
#' # Or, alternatively load the data as
#' data(C8E19)
#'
#' # View the structure
#' str(chapter_8_exercise_19)
#'
#' # Brief summary of the data.
#' summary(chapter_8_exercise_19)
#'
"chapter_8_exercise_19"

#' The data used in Chapter 9, Exercise 4
#'
#' Data from Chapter 9 Exercise 4 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Group.
#' \item Pre.
#' \item Post.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_9_exercise_4
#' @aliases chapter_9_exercise_4 Chapter_9_Exercise_4 C9E4 c9e4
#' @keywords datasets
#' @usage data(chapter_9_exercise_4)
#' @section Synonym:
#' C9E4
#' @examples
#' # Load the data
#' data(chapter_9_exercise_4)
#'
#' # Or, alternatively load the data as
#' data(C9E4)
#'
#' # View the structure
#' str(chapter_9_exercise_4)
#'
#' # Brief summary of the data.
#' summary(chapter_9_exercise_4)
#'
"chapter_9_exercise_4"

#' The data used in Chapter 9, Exercise 14
#'
#' Data from Chapter 9 Exercise 14 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item Condition.
#' \item EmotClose.
#' \item Anger.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_9_exercise_14
#' @aliases chapter_9_exercise_14 Chapter_9_Exercise_14 C9E14 c9e14
#' @keywords datasets
#' @usage data(chapter_9_exercise_14)
#' @section Synonym:
#' C9E14
#' @examples
#' # Load the data
#' data(chapter_9_exercise_14)
#'
#' # Or, alternatively load the data as
#' data(C9E14)
#'
#' # View the structure
#' str(chapter_9_exercise_14)
#'
#' # Brief summary of the data.
#' summary(chapter_9_exercise_14)
#'
"chapter_9_exercise_14"

#' The data used in Chapter 9, Exercise 15
#'
#' Data from Chapter 9 Exercise 15 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item Condition.
#' \item EmotClose.
#' \item Anger.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_9_exercise_15
#' @aliases chapter_9_exercise_15 Chapter_9_Exercise_15 C9E15 c9e15
#' @keywords datasets
#' @usage data(chapter_9_exercise_15)
#' @section Synonym:
#' C9E15
#' @examples
#' # Load the data
#' data(chapter_9_exercise_15)
#'
#' # Or, alternatively load the data as
#' data(C9E15)
#'
#' # View the structure
#' str(chapter_9_exercise_15)
#'
#' # Brief summary of the data.
#' summary(chapter_9_exercise_15)
#'
"chapter_9_exercise_15"

#' The data used in Chapter 9, Exercise 16
#'
#' Data from Chapter 9 Exercise 16 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item Condition.
#' \item EmotClose.
#' \item Anger.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_9_exercise_16
#' @aliases chapter_9_exercise_16 Chapter_9_Exercise_16 C9E16 c9e16
#' @keywords datasets
#' @usage data(chapter_9_exercise_16)
#' @section Synonym:
#' C9E16
#' @examples
#' # Load the data
#' data(chapter_9_exercise_16)
#'
#' # Or, alternatively load the data as
#' data(C9E16)
#'
#' # View the structure
#' str(chapter_9_exercise_16)
#'
#' # Brief summary of the data.
#' summary(chapter_9_exercise_16)
#'
"chapter_9_exercise_16"


#' The data used in Chapter 9 Extension, Exercise 1
#'
#' Data from Chapter 9 Extension Exercise 1 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item RSA.
#' \item Delay.
#' \item SES_group.
#' \item MaxDelay.
#' \item RSAdev.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @source Sturge-Apple, M. L.,  Suor, J. H., Davies, P. T., Cicchetti, D., Skibo, M. A., & Rogosch, F. A. (2016). Vagal tone and children's delay of gratification: Differential sensitivity in resource-poor and resource-rich environments. \emph{Psychological Science}, \emph{27}, 885--893.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data: {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_9_extension_exercise_1
#' @aliases chapter_9_extension_exercise_1 Chapter_9_Extension_Exercise_1 C9ExtE1 c9exte1
#' @keywords datasets
#' @usage data(chapter_9_extension_exercise_1)
#' @section Synonym:
#' C9ExtE1
#' @examples
#' # Load the data
#' data(chapter_9_extension_exercise_1)
#'
#' # Or, alternatively load the data as
#' data(C9ExtE1)
#'
#' # View the structure
#' str(chapter_9_extension_exercise_1)
#'
#' # Brief summary of the data.
#' summary(chapter_9_extension_exercise_1)
#'
"chapter_9_extension_exercise_1"

#' The data used in Chapter 9 Extension, Exercise 2
#'
#' Data from Chapter 9 Extension Exercise 2 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ParticipantNumber.
#' \item Group.
#' \item BaseHrsDrkTypWk.
#' \item FolHrsDrkTypWk.
#' \item DiffBaseFolHrsDrk.
#' \item BaseHrsCtrd.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_9_extension_exercise_2
#' @aliases chapter_9_extension_exercise_2 Chapter_9_Extension_Exercise_2 C9ExtE2 c9exte2
#' @keywords datasets
#' @usage data(chapter_9_extension_exercise_2)
#' @section Synonym:
#' C9ExtE2
#' @examples
#' # Load the data
#' data(chapter_9_extension_exercise_2)
#'
#' # Or, alternatively load the data as
#' data(C9ExtE2)
#'
#' # View the structure
#' str(chapter_9_extension_exercise_2)
#'
#' # Brief summary of the data.
#' summary(chapter_9_extension_exercise_2)
#'
"chapter_9_extension_exercise_2"


#' The data used in Chapter 9 Extension, Exercise 3
#'
#' Data from Chapter 9 Extension Exercise 3 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Grade.
#' \item Treatment.
#' \item IQPre.
#' \item IQ4.
#' \item IQ8.
#' \item IQGain.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_9_extension_exercise_2
#' @aliases chapter_9_extension_exercise_3 Chapter_9_Extension_Exercise_3 C9ExtE3 c9exte3
#' @keywords datasets
#' @usage data(chapter_9_extension_exercise_3)
#' @section Synonym:
#' C9ExtE3
#' @examples
#' # Load the data
#' data(chapter_9_extension_exercise_3)
#'
#' # Or, alternatively load the data as
#' data(C9ExtE3)
#'
#' # View the structure
#' str(chapter_9_extension_exercise_3)
#'
#' # Brief summary of the data.
#' summary(chapter_9_extension_exercise_2)
#'
"chapter_9_extension_exercise_3"


# Chapter 9, Extension Figures (Only Used in Figures)
#' The data used in Chapter 9 Extension Figures 4 and 5
#'
#' Data used in the Chapter 9 Extension, Figures 4 and 5, of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Grade
#' \item Treatment
#' \item IQPre
#' \item IQ4
#' \item IQ8
#' \item AvPost
#' \item IQPre_Mean
#' \item IQPre_Centered
#' \item TxX
#' \item Constant1}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data: {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_9_extension_figures_4_and_5
#' @aliases chapter_9_extension_figures_4_and_5 Chapter_9_Extension_Figures_4_and_5 c9extfigs4and5 C9ExtFigs4and5
#' @keywords datasets
#' @usage data(chapter_9_extension_figures_4_and_5)
#' @section Synonym:
#' C9ExtE1
#' @examples
#' # Load the data
#' data(chapter_9_extension_figures_4_and_5)
#'
#' # Or, alternatively load the data as
#' data(C9ExtFigs4and5)
#'
#' # View the structure
#' str(chapter_9_extension_figures_4_and_5)
#'
#' # Brief summary of the data.
#' summary(chapter_9_extension_figures_4_and_5)
#'
"chapter_9_extension_figures_4_and_5"


#' The data used in Chapter 10, Exercise 7
#'
#' Data from Chapter 10 Exercise 7 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Ratings.
#' \item Therapist.
#' \item Method.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_10_exercise_7
#' @aliases chapter_10_exercise_7 Chapter_10_Exercise_7 C10E7 c10e7
#' @keywords datasets
#' @usage data(chapter_10_exercise_7)
#' @section Synonym:
#' C10E7
#' @examples
#' # Load the data
#' data(chapter_10_exercise_7)
#'
#' # Or, alternatively load the data as
#' data(C10E7)
#'
#' # View the structure
#' str(chapter_10_exercise_7)
#'
#' # Brief summary of the data.
#' summary(chapter_10_exercise_7)
#'
"chapter_10_exercise_7"

#' The data used in Chapter 10, Exercise 9
#'
#' Data from Chapter 10 Exercise 9 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item BP.
#' \item ResearchAssistant.
#' \item Biofeedback.
#' \item Diet.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_10_exercise_9
#' @aliases chapter_10_exercise_9 Chapter_10_Exercise_9 C10E9 c10e9
#' @keywords datasets
#' @usage data(chapter_10_exercise_9)
#' @section Synonym:
#' C10E9
#' @examples
#' # Load the data
#' data(chapter_10_exercise_9)
#'
#' # Or, alternatively load the data as
#' data(C10E9)
#'
#' # View the structure
#' str(chapter_10_exercise_9)
#'
#' # Brief summary of the data.
#' summary(chapter_10_exercise_9)
#'
"chapter_10_exercise_9"

#' The data used in Chapter 10, Exercise 14
#'
#' Data from Chapter 10 Exercise 14 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Composite.
#' \item Therapist.
#' \item Modality.
#' \item Therapist_w_Modality}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_10_exercise_14
#' @aliases chapter_10_exercise_14 Chapter_10_Exercise_14 C10E14 c10e14
#' @keywords datasets
#' @usage data(chapter_10_exercise_14)
#' @section Synonym:
#' C10E14
#' @examples
#' # Load the data
#' data(chapter_10_exercise_14)
#'
#' # Or, alternatively load the data as
#' data(C10E14)
#'
#' # View the structure
#' str(chapter_10_exercise_14)
#'
#' # Brief summary of the data.
#' summary(chapter_10_exercise_14)
#'
"chapter_10_exercise_14"


#' The data used in Chapter 11, Exercise 3
#'
#' Data from Chapter 11 Exercise 3 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Location1.
#' \item Location2.
#' \item Location3.
#' \item Location4.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_11_exercise_3
#' @aliases chapter_11_exercise_3 Chapter_11_Exercise_3 C11E3 c11e3
#' @keywords datasets
#' @usage data(chapter_11_exercise_3)
#' @section Synonym:
#' C11E3
#' @examples
#' # Load the data
#' data(chapter_11_exercise_3)
#'
#' # Or, alternatively load the data as
#' data(C11E3)
#'
#' # View the structure
#' str(chapter_11_exercise_3)
#'
#' # Brief summary of the data.
#' summary(chapter_11_exercise_3)
#'
"chapter_11_exercise_3"

#' The data used in Chapter 11, Exercise 5
#'
#' Data from Chapter 11 Exercise 5 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item cond1.
#' \item cond2.
#' \item cond3.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_11_exercise_5
#' @aliases chapter_11_exercise_5 Chapter_11_Exercise_5 C11E5 c11e5
#' @keywords datasets
#' @usage data(chapter_11_exercise_5)
#' @section Synonym:
#' C11E5
#' @examples
#' # Load the data
#' data(chapter_11_exercise_5)
#'
#' # Or, alternatively load the data as
#' data(C11E5)
#'
#' # View the structure
#' str(chapter_11_exercise_5)
#'
#' # Brief summary of the data.
#' summary(chapter_11_exercise_5)
#'
"chapter_11_exercise_5"

#' The data used in Chapter 11, Exercise 17
#'
#' Data from Chapter 11 Exercise 17 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Day1.
#' \item Day2.
#' \item Day3.
#' \item Day4.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_11_exercise_17
#' @aliases chapter_11_exercise_17 Chapter_11_Exercise_17 C11E17 c11e17
#' @keywords datasets
#' @usage data(chapter_11_exercise_17)
#' @section Synonym:
#' C11E17
#' @examples
#' # Load the data
#' data(chapter_11_exercise_17)
#'
#' # Or, alternatively load the data as
#' data(C11E17)
#'
#' # View the structure
#' str(chapter_11_exercise_17)
#'
#' # Brief summary of the data.
#' summary(chapter_11_exercise_17)
#'
"chapter_11_exercise_17"

#' The data used in Chapter 11, Exercise 18
#'
#' Data from Chapter 11 Exercise 18 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Strong.
#' \item Medium.
#' \item Weak.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_11_exercise_18
#' @aliases chapter_11_exercise_18 Chapter_11_Exercise_18 C11E18 c11e18
#' @keywords datasets
#' @usage data(chapter_11_exercise_18)
#' @section Synonym:
#' C11E18
#' @examples
#' # Load the data
#' data(chapter_11_exercise_18)
#'
#' # Or, alternatively load the data as
#' data(C11E18)
#'
#' # View the structure
#' str(chapter_11_exercise_18)
#'
#' # Brief summary of the data.
#' summary(chapter_11_exercise_18)
#'
"chapter_11_exercise_18"

#' The data used in Chapter 11, Exercise 19
#'
#' Data from Chapter 11 Exercise 19 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Face.
#' \item Circle.
#' \item Paper.
#' \item White.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_11_exercise_19
#' @aliases chapter_11_exercise_19 Chapter_11_Exercise_19 C11E19 c11e19
#' @keywords datasets
#' @usage data(chapter_11_exercise_19)
#' @section Synonym:
#' C11E19
#' @examples
#' # Load the data
#' data(chapter_11_exercise_19)
#'
#' # Or, alternatively load the data as
#' data(C11E19)
#'
#' # View the structure
#' str(chapter_11_exercise_19)
#'
#' # Brief summary of the data.
#' summary(chapter_11_exercise_19)
#'
"chapter_11_exercise_19"

#' The data used in Chapter 11, Exercise 21
#'
#' Data from Chapter 11 Exercise 21 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Mother.
#' \item Rater.
#' \item Warmth.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_11_exercise_21
#' @aliases chapter_11_exercise_21 Chapter_11_Exercise_21 C11E21 c11e21
#' @keywords datasets
#' @usage data(chapter_11_exercise_21)
#' @section Synonym:
#' C11E21
#' @examples
#' # Load the data
#' data(chapter_11_exercise_21)
#'
#' # Or, alternatively load the data as
#' data(C11E21)
#'
#' # View the structure
#' str(chapter_11_exercise_21)
#'
#' # Brief summary of the data.
#' summary(chapter_11_exercise_21)
#'
"chapter_11_exercise_21"

#' The data used in Chapter 11, Exercise 22
#'
#' Data from Chapter 11 Exercise 22 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item AgeNumeric.
#' \item GenderNum.
#' \item AVGMeetMonkey.
#' \item AVGChildRecTreats.
#' \item AVGExpGivesCommon.
#' \item AVGChildGivesCommon.
#' \item AVGChildGivesOwn.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_11_exercise_22
#' @aliases chapter_11_exercise_22 Chapter_11_Exercise_22 C11E22 c11e22
#' @keywords datasets
#' @usage data(chapter_11_exercise_22)
#' @section Synonym:
#' C11E22
#' @examples
#' # Load the data
#' data(chapter_11_exercise_22)
#'
#' # Or, alternatively load the data as
#' data(C11E22)
#'
#' # View the structure
#' str(chapter_11_exercise_22)
#'
#' # Brief summary of the data.
#' summary(chapter_11_exercise_22)
#'
"chapter_11_exercise_22"

#' The data used in Chapter 11, Exercise 23
#'
#' Data from Chapter 11 Exercise 23 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item id.
#' \item position.
#' \item meanz.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_11_exercise_23
#' @aliases chapter_11_exercise_23 Chapter_11_Exercise_23 C11E23 c11e23
#' @keywords datasets
#' @usage data(chapter_11_exercise_23)
#' @section Synonym:
#' C11E23
#' @examples
#' # Load the data
#' data(chapter_11_exercise_23)
#'
#' # Or, alternatively load the data as
#' data(C11E23)
#'
#' # View the structure
#' str(chapter_11_exercise_23)
#'
#' # Brief summary of the data.
#' summary(chapter_11_exercise_23)
#'
"chapter_11_exercise_23"

#' The data used in Chapter 11, Exercise 24
#'
#' Data from Chapter 11 Exercise 24 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item id.
#' \item judgement.
#' \item activity.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_11_exercise_24
#' @aliases chapter_11_exercise_24 Chapter_11_Exercise_24 C11E24 c11e24
#' @keywords datasets
#' @usage data(chapter_11_exercise_24)
#' @section Synonym:
#' C11E24
#' @examples
#' # Load the data
#' data(chapter_11_exercise_24)
#'
#' # Or, alternatively load the data as
#' data(C11E24)
#'
#' # View the structure
#' str(chapter_11_exercise_24)
#'
#' # Brief summary of the data.
#' summary(chapter_11_exercise_24)
#'
"chapter_11_exercise_24"


#' The data used in Chapter 12, Exercise 9
#'
#' Data from Chapter 12 Exercise 9 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item GridLeft.
#' \item GridRight.
#' \item BraceLeft.
#' \item BraceRight.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_12_exercise_9
#' @aliases chapter_12_exercise_9 Chapter_12_Exercise_9 C12E9 c12e9
#' @keywords datasets
#' @usage data(chapter_12_exercise_9)
#' @section Synonym:
#' C12E9
#' @examples
#' # Load the data
#' data(chapter_12_exercise_9)
#'
#' # Or, alternatively load the data as
#' data(C12E9)
#'
#' # View the structure
#' str(chapter_12_exercise_9)
#'
#' # Brief summary of the data.
#' summary(chapter_12_exercise_9)
#'
"chapter_12_exercise_9"


#' The data used in Chapter 12, Exercise 17
#'
#' Data from Chapter 12 Exercise 17 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Day1.
#' \item Day2.
#' \item Day3.
#' \item Day4.
#' \item Group}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_12_exercise_17
#' @aliases chapter_12_exercise_17 Chapter_12_Exercise_17 C12E17 c12e17
#' @keywords datasets
#' @usage data(chapter_12_exercise_17)
#' @section Synonym:
#' C12E17
#' @examples
#' # Load the data
#' data(chapter_12_exercise_17)
#'
#' # Or, alternatively load the data as
#' data(C12E17)
#'
#' # View the structure
#' str(chapter_12_exercise_17)
#'
#' # Brief summary of the data.
#' summary(chapter_12_exercise_17)
#'
"chapter_12_exercise_17"


#' The data used in Chapter 12, Exercise 18
#'
#' Data from Chapter 12 Exercise 18 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Baseline.
#' \item Feedback.
#' \item Group.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_12_exercise_18
#' @aliases chapter_12_exercise_18 Chapter_12_Exercise_18 C12E18 c12e18
#' @keywords datasets
#' @usage data(chapter_12_exercise_18)
#' @section Synonym:
#' C12E18
#' @examples
#' # Load the data
#' data(chapter_12_exercise_18)
#'
#' # Or, alternatively load the data as
#' data(C12E18)
#'
#' # View the structure
#' str(chapter_12_exercise_18)
#'
#' # Brief summary of the data.
#' summary(chapter_12_exercise_18)
#'
"chapter_12_exercise_18"


#' The data used in Chapter 12, Exercise 19
#'
#' Data from Chapter 12 Exercise 19 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item September.
#' \item November.
#' \item April.
#' \item June.
#' \item July.
#' \item Group.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_12_exercise_19
#' @aliases chapter_12_exercise_19 Chapter_12_Exercise_19 C12E19 c12e19
#' @keywords datasets
#' @usage data(chapter_12_exercise_19)
#' @section Synonym:
#' C12E19
#' @examples
#' # Load the data
#' data(chapter_12_exercise_19)
#'
#' # Or, alternatively load the data as
#' data(C12E19)
#'
#' # View the structure
#' str(chapter_12_exercise_19)
#'
#' # Brief summary of the data.
#' summary(chapter_12_exercise_19)
#'
"chapter_12_exercise_19"


#' The data used in Chapter 12, Exercise 21
#'
#' Data from Chapter 12 Exercise 21 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Recall.
#' \item Subject.
#' \item Passage.
#' \item DifficultyCondition.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_12_exercise_21
#' @aliases chapter_12_exercise_21 Chapter_12_Exercise_21 C12E21 c12e21
#' @keywords datasets
#' @usage data(chapter_12_exercise_21)
#' @section Synonym:
#' C12E21
#' @examples
#' # Load the data
#' data(chapter_12_exercise_21)
#'
#' # Or, alternatively load the data as
#' data(C12E21)
#'
#' # View the structure
#' str(chapter_12_exercise_21)
#'
#' # Brief summary of the data.
#' summary(chapter_12_exercise_21)
#'
"chapter_12_exercise_21"

#' The data used in Chapter 13, Exercise 7
#'
#' Data from Chapter 13 Exercise 7 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Location1.
#' \item Location2.
#' \item Location3.
#' \item Location4.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_13_exercise_7
#' @aliases chapter_13_exercise_7 Chapter_13_Exercise_7 C13E7 c13e7
#' @keywords datasets
#' @usage data(chapter_13_exercise_7)
#' @section Synonym:
#' C13E7
#' @examples
#' # Load the data
#' data(chapter_13_exercise_7)
#'
#' # Or, alternatively load the data as
#' data(C13E7)
#'
#' # View the structure
#' str(chapter_13_exercise_7)
#'
#' # Brief summary of the data.
#' summary(chapter_13_exercise_7)
#'
"chapter_13_exercise_7"

#' The data used in Chapter 13, Exercise 10
#'
#' Data from Chapter 13 Exercise 10 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Face.
#' \item Circle.
#' \item Paper.
#' \item White.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_13_exercise_10
#' @aliases chapter_13_exercise_10 Chapter_13_Exercise_10 C13E10 c13e10
#' @keywords datasets
#' @usage data(chapter_13_exercise_10)
#' @section Synonym:
#' C13E10
#' @examples
#' # Load the data
#' data(chapter_13_exercise_10)
#'
#' # Or, alternatively load the data as
#' data(C13E10)
#'
#' # View the structure
#' str(chapter_13_exercise_10)
#'
#' # Brief summary of the data.
#' summary(chapter_13_exercise_10)
#'
"chapter_13_exercise_10"

#' The data used in Chapter 13, Exercise 13
#'
#' Data from Chapter 13 Exercise 13 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Day1.
#' \item Day2.
#' \item Day3.
#' \item Day4.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_13_exercise_13
#' @aliases chapter_13_exercise_13 Chapter_13_Exercise_13 C13E13 c13e13
#' @keywords datasets
#' @usage data(chapter_13_exercise_13)
#' @section Synonym:
#' C13E13
#' @examples
#' # Load the data
#' data(chapter_13_exercise_13)
#'
#' # Or, alternatively load the data as
#' data(C13E13)
#'
#' # View the structure
#' str(chapter_13_exercise_13)
#'
#' # Brief summary of the data.
#' summary(chapter_13_exercise_13)
#'
"chapter_13_exercise_13"

#' The data used in Chapter 13, Exercise 14
#'
#' Data from Chapter 13 Exercise 14 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Time1.
#' \item Time2.
#' \item Time3.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_13_exercise_14
#' @aliases chapter_13_exercise_14 Chapter_13_Exercise_14 C13E14 c13e14
#' @keywords datasets
#' @usage data(chapter_13_exercise_14)
#' @section Synonym:
#' C13E14
#' @examples
#' # Load the data
#' data(chapter_13_exercise_14)
#'
#' # Or, alternatively load the data as
#' data(C13E14)
#'
#' # View the structure
#' str(chapter_13_exercise_14)
#'
#' # Brief summary of the data.
#' summary(chapter_13_exercise_14)
#'
"chapter_13_exercise_14"

#' The data used in Chapter 13, Exercise 22
#'
#' Data from Chapter 13 Exercise 22 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Condition1.
#' \item Condition2.
#' \item Condition3.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_13_exercise_22
#' @aliases chapter_13_exercise_22 Chapter_13_Exercise_22 C13E22 c13e22
#' @keywords datasets
#' @usage data(chapter_13_exercise_22)
#' @section Synonym:
#' C13E22
#' @examples
#' # Load the data
#' data(chapter_13_exercise_22)
#'
#' # Or, alternatively load the data as
#' data(C13E22)
#'
#' # View the structure
#' str(chapter_13_exercise_22)
#'
#' # Brief summary of the data.
#' summary(chapter_13_exercise_22)
#'
"chapter_13_exercise_22"

#' The data used in Chapter 13, Exercise 23
#'
#' Data from Chapter 13 Exercise 23 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item AgeNumeric.
#' \item GenderNum.
#' \item AVGMeetMonkey.
#' \item AVGChildRecTreats.
#' \item AVGExpGivesCommon.
#' \item AVGChildGivesCommon.
#' \item AVGChildGivesOwn.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_13_exercise_23
#' @aliases chapter_13_exercise_23 Chapter_13_Exercise_23 C13E23 c13e23
#' @keywords datasets
#' @usage data(chapter_13_exercise_23)
#' @section Synonym:
#' C13E23
#' @examples
#' # Load the data
#' data(chapter_13_exercise_23)
#'
#' # Or, alternatively load the data as
#' data(C13E23)
#'
#' # View the structure
#' str(chapter_13_exercise_23)
#'
#' # Brief summary of the data.
#' summary(chapter_13_exercise_23)
#'
"chapter_13_exercise_23"

#' The data used in Chapter 13, Exercise 24
#'
#' Data from Chapter 13 Exercise 24 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item id.
#' \item position.
#' \item meanz.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_13_exercise_24
#' @aliases chapter_13_exercise_24 Chapter_13_Exercise_24 C13E24 c13e24
#' @keywords datasets
#' @usage data(chapter_13_exercise_24)
#' @section Synonym:
#' C13E24
#' @examples
#' # Load the data
#' data(chapter_13_exercise_24)
#'
#' # Or, alternatively load the data as
#' data(C13E24)
#'
#' # View the structure
#' str(chapter_13_exercise_24)
#'
#' # Brief summary of the data.
#' summary(chapter_13_exercise_24)
#'
"chapter_13_exercise_24"

#' The data used in Chapter 13, Exercise 25
#'
#' Data from Chapter 13 Exercise 25 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item self.
#' \item friend.
#' \item case.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_13_exercise_25
#' @aliases chapter_13_exercise_25 Chapter_13_Exercise_25 C13E25 c13e25
#' @keywords datasets
#' @usage data(chapter_13_exercise_25)
#' @section Synonym:
#' C13E25
#' @examples
#' # Load the data
#' data(chapter_13_exercise_25)
#'
#' # Or, alternatively load the data as
#' data(C13E25)
#'
#' # View the structure
#' str(chapter_13_exercise_25)
#'
#' # Brief summary of the data.
#' summary(chapter_13_exercise_25)
#'
"chapter_13_exercise_25"

#' The data used in Chapter 14, Exercise 10
#'
#' Data from Chapter 14 Exercise 10 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Angle0.
#' \item Angle4.
#' \item Angle8.
#' \item Group.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_14_exercise_10
#' @aliases chapter_14_exercise_10 Chapter_14_Exercise_10 C14E10 c14e10
#' @keywords datasets
#' @usage data(chapter_14_exercise_10)
#' @section Synonym:
#' C14E10
#' @examples
#' # Load the data
#' data(chapter_14_exercise_10)
#'
#' # Or, alternatively load the data as
#' data(C14E10)
#'
#' # View the structure
#' str(chapter_14_exercise_10)
#'
#' # Brief summary of the data.
#' summary(chapter_14_exercise_10)
#'
"chapter_14_exercise_10"

#' The data used in Chapter 14, Exercise 14
#'
#' Data from Chapter 14 Exercise 14 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Gender.
#' \item FaleFriend.
#' \item FemaleFriend.
#' \item Same.
#' \item Opposite.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_14_exercise_14
#' @aliases chapter_14_exercise_14 Chapter_14_Exercise_14 C14E14 c14e14
#' @keywords datasets
#' @usage data(chapter_14_exercise_14)
#' @section Synonym:
#' C14E14
#' @examples
#' # Load the data
#' data(chapter_14_exercise_14)
#'
#' # Or, alternatively load the data as
#' data(C14E14)
#'
#' # View the structure
#' str(chapter_14_exercise_14)
#'
#' # Brief summary of the data.
#' summary(chapter_14_exercise_14)
#'
"chapter_14_exercise_14"

#' The data used in Chapter 14, Exercise 15
#'
#' Data from Chapter 14 Exercise 15 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Baseline.
#' \item Feedback.
#' \item Group.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_14_exercise_15
#' @aliases chapter_14_exercise_15 Chapter_14_Exercise_15 C14E15 c14e15
#' @keywords datasets
#' @usage data(chapter_14_exercise_15)
#' @section Synonym:
#' C14E15
#' @examples
#' # Load the data
#' data(chapter_14_exercise_15)
#'
#' # Or, alternatively load the data as
#' data(C14E15)
#'
#' # View the structure
#' str(chapter_14_exercise_15)
#'
#' # Brief summary of the data.
#' summary(chapter_14_exercise_15)
#'
"chapter_14_exercise_15"

#' The data used in Chapter 14, Exercise 21
#'
#' Data from Chapter 14 Exercise 21 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Day1.
#' \item Day2.
#' \item Day3.
#' \item Day4.
#' \item Group.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_14_exercise_21
#' @aliases chapter_14_exercise_21 Chapter_14_Exercise_21 C14E21 c14e21
#' @keywords datasets
#' @usage data(chapter_14_exercise_21)
#' @section Synonym:
#' C14E21
#' @examples
#' # Load the data
#' data(chapter_14_exercise_21)
#'
#' # Or, alternatively load the data as
#' data(C14E21)
#'
#' # View the structure
#' str(chapter_14_exercise_21)
#'
#' # Brief summary of the data.
#' summary(chapter_14_exercise_21)
#'
"chapter_14_exercise_21"

#' The data used in Chapter 14, Exercise 22
#'
#' Data from Chapter 14 Exercise 22 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item September.
#' \item November.
#' \item April.
#' \item June.
#' \item July.
#' \item Group.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_14_exercise_22
#' @aliases chapter_14_exercise_22 Chapter_14_Exercise_22 C14E22 c14e22
#' @keywords datasets
#' @usage data(chapter_14_exercise_22)
#' @section Synonym:
#' C14E22
#' @examples
#' # Load the data
#' data(chapter_14_exercise_22)
#'
#' # Or, alternatively load the data as
#' data(C14E22)
#'
#' # View the structure
#' str(chapter_14_exercise_22)
#'
#' # Brief summary of the data.
#' summary(chapter_14_exercise_22)
#'
"chapter_14_exercise_22"


#' The data used in Chapter 15, Exercise 16
#'
#' Data from Chapter 15 Exercise 16 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item Time.
#' \item Y.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_15_exercise_16
#' @aliases chapter_15_exercise_16 Chapter_15_Exercise_16 C15E16 c15e16
#' @keywords datasets
#' @usage data(chapter_15_exercise_16)
#' @section Synonym:
#' C15E16
#' @examples
#' # Load the data
#' data(chapter_15_exercise_16)
#'
#' # Or, alternatively load the data as
#' data(C15E16)
#'
#' # View the structure
#' str(chapter_15_exercise_16)
#'
#' # Brief summary of the data.
#' summary(chapter_15_exercise_16)
#'
"chapter_15_exercise_16"


#' The data used in Chapter 15, Exercise 17
#'
#' Data from Chapter 15 Exercise 17 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Day1.
#' \item Day2.
#' \item Day3.
#' \item Day4.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_15_exercise_17
#' @aliases chapter_15_exercise_17 Chapter_15_Exercise_17 C15E17 c15e17
#' @keywords datasets
#' @usage data(chapter_15_exercise_17)
#' @section Synonym:
#' C15E17
#' @examples
#' # Load the data
#' data(chapter_15_exercise_17)
#'
#' # Or, alternatively load the data as
#' data(C15E17)
#'
#' # View the structure
#' str(chapter_15_exercise_17)
#'
#' # Brief summary of the data.
#' summary(chapter_15_exercise_17)
#'
"chapter_15_exercise_17"

#' The data used in Chapter 15, Exercise 18
#'
#' Data from Chapter 15 Exercise 18 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Subject.
#' \item September.
#' \item October.
#' \item November.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_15_exercise_18
#' @aliases chapter_15_exercise_18 Chapter_15_Exercise_18 C15E18 c15e18
#' @keywords datasets
#' @usage data(chapter_15_exercise_18)
#' @section Synonym:
#' C15E18
#' @examples
#' # Load the data
#' data(chapter_15_exercise_18)
#'
#' # Or, alternatively load the data as
#' data(C15E18)
#'
#' # View the structure
#' str(chapter_15_exercise_18)
#'
#' # Brief summary of the data.
#' summary(chapter_15_exercise_18)
#'
"chapter_15_exercise_18"

#' The data used in Chapter 15, Exercise 19
#'
#' Data from Chapter 15 Exercise 19 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Subject.
#' \item Cognitive70.
#' \item Cognitive73.
#' \item Cognitive74.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_15_exercise_19
#' @aliases chapter_15_exercise_19 Chapter_15_Exercise_19 C15E19 c15e19
#' @keywords datasets
#' @usage data(chapter_15_exercise_19)
#' @section Synonym:
#' C15E19
#' @examples
#' # Load the data
#' data(chapter_15_exercise_19)
#'
#' # Or, alternatively load the data as
#' data(C15E19)
#'
#' # View the structure
#' str(chapter_15_exercise_19)
#'
#' # Brief summary of the data.
#' summary(chapter_15_exercise_19)
#'
"chapter_15_exercise_19"


#' The data used in Chapter 15, Exercise 18 (Univariate)
#'
#' Data from Chapter 15 Exercise 18 (Univariate) of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item ID.
#' \item Time.
#' \item Y.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_15_exercise_18_univariate
#' @aliases chapter_15_exercise_18_univariate Chapter_15_Exercise_18_Univariate C15E18U c15e18u
#' @keywords datasets
#' @usage data(chapter_15_exercise_18_univariate)
#' @section Synonym:
#' C15E18U
#' @examples
#' # Load the data
#' data(chapter_15_exercise_18_univariate)
#'
#' # Or, alternatively load the data as
#' data(C15E18U)
#'
#' # View the structure
#' str(chapter_15_exercise_18_univariate)
#'
#' # Brief summary of the data.
#' summary(chapter_15_exercise_18_univariate)
#'
"chapter_15_exercise_18_univariate"


#' The data used in Chapter 15, Exercise 19 (Univariate)
#'
#' Data from Chapter 15 Exercise 19 (Univariate) of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Subject.
#' \item Age.
#' \item Ability.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_15_exercise_19_univariate
#' @aliases chapter_15_exercise_19_univariate Chapter_15_Exercise_19_Univariate C15E19U c15e19u
#' @keywords datasets
#' @usage data(chapter_15_exercise_19_univariate)
#' @section Synonym:
#' C15E19U
#' @examples
#' # Load the data
#' data(chapter_15_exercise_19_univariate)
#'
#' # Or, alternatively load the data as
#' data(C15E19U)
#'
#' # View the structure
#' str(chapter_15_exercise_19_univariate)
#'
#' # Brief summary of the data.
#' summary(chapter_15_exercise_19_univariate)
#'
"chapter_15_exercise_19_univariate"


#' The data used in Chapter 16, Exercise 5
#'
#' Data from Chapter 16 Exercise 5 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Severity.
#' \item Trainee.
#' \item Gender.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_16_exercise_5
#' @aliases chapter_16_exercise_5 Chapter_16_Exercise_5 C16E5 c16e5
#' @keywords datasets
#' @usage data(chapter_16_exercise_5)
#' @section Synonym:
#' C16E5
#' @examples
#' # Load the data
#' data(chapter_16_exercise_5)
#'
#' # Or, alternatively load the data as
#' data(C16E5)
#'
#' # View the structure
#' str(chapter_16_exercise_5)
#'
#' # Brief summary of the data.
#' summary(chapter_16_exercise_5)
#'
"chapter_16_exercise_5"


#' The data used in Chapter 16, Exercise 7
#'
#' Data from Chapter 16 Exercise 7 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Observation.
#' \item Room.
#' \item Condition.
#' \item Cognition.
#' \item Skill.
#' \item Inductive.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_16_exercise_7
#' @aliases chapter_16_exercise_7 Chapter_16_Exercise_7 C16E7 c16e7
#' @keywords datasets
#' @usage data(chapter_16_exercise_7)
#' @section Synonym:
#' C16E7
#' @examples
#' # Load the data
#' data(chapter_16_exercise_7)
#'
#' # Or, alternatively load the data as
#' data(C16E7)
#'
#' # View the structure
#' str(chapter_16_exercise_7)
#'
#' # Brief summary of the data.
#' summary(chapter_16_exercise_7)
#'
"chapter_16_exercise_7"


#' The data used in Chapter 16, Exercise 9
#'
#' Data from Chapter 16 Exercise 9 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Observation.
#' \item Room.
#' \item Condition.
#' \item Cognition.
#' \item Skill
#' \item Inductive.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name chapter_16_exercise_9
#' @aliases chapter_16_exercise_9 Chapter_16_Exercise_9 C16E9 c16e9
#' @keywords datasets
#' @usage data(chapter_16_exercise_9)
#' @section Synonym:
#' C16E9
#' @examples
#' # Load the data
#' data(chapter_16_exercise_9)
#'
#' # Or, alternatively load the data as
#' data(C16E9)
#'
#' # View the structure
#' str(chapter_16_exercise_9)
#'
#' # Brief summary of the data.
#' summary(chapter_16_exercise_9)
#'
"chapter_16_exercise_9"

#######################################################################################################################################

# Tutorials
#######################################################################################################################################
#' The data used in Tutorial 1, Table 1
#'
#' Data from Tutorial 1 Table 1 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Group.
#' \item Score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name tutorial_1_table_1
#' @aliases tutorial_1_table_1 tutorial_1_table_1 T1T1 t1t1
#' @keywords datasets
#' @usage data(tutorial_1_table_1)
#' @section Synonym:
#' T1T1
#' @examples
#' # Load the data
#' data(tutorial_1_table_1)
#'
#' # Or, alternatively load the data as
#' data(T1T1)
#'
#' # View the structure
#' str(tutorial_1_table_1)
#'
#' # Brief summary of the data.
#' summary(tutorial_1_table_1)
#'
"tutorial_1_table_1"


#' The data used in Tutorial 2, Table 1
#'
#' Data from Tutorial 2 Table 1 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Group.
#' \item Score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name tutorial_2_table_1
#' @aliases tutorial_2_table_1 tutorial_2_table_1 T2T1 t2t1
#' @keywords datasets
#' @usage data(tutorial_2_table_1)
#' @section Synonym:
#' T2T1
#' @examples
#' # Load the data
#' data(tutorial_2_table_1)
#'
#' # Or, alternatively load the data as
#' data(T2T1)
#'
#' # View the structure
#' str(tutorial_2_table_1)
#'
#' # Brief summary of the data.
#' summary(tutorial_2_table_1)
#'
"tutorial_2_table_1"


#' The data used in Tutorial 2, Table 2
#'
#' Data from Tutorial 2 Table 2 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Y.
#' \item X1.
#' \item X2.
#' \item X3.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name tutorial_2_table_1
#' @aliases tutorial_2_table_2 tutorial_2_table_2 T2T2 t2t2
#' @keywords datasets
#' @usage data(tutorial_2_table_2)
#' @section Synonym:
#' T2T2
#' @examples
#' # Load the data
#' data(tutorial_2_table_2)
#'
#' # Or, alternatively load the data as
#' data(T2T2)
#'
#' # View the structure
#' str(tutorial_2_table_2)
#'
#' # Brief summary of the data.
#' summary(tutorial_2_table_2)
#'
"tutorial_2_table_2"


#' The data used in Tutorial 3A, Table 1
#'
#' Data from Tutorial 3A Table 1 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Group.
#' \item Score.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name tutorial_3a_table_1
#' @aliases tutorial_3a_table_1 tutorial_3a_table_1 T3AT1 t3at1
#' @keywords datasets
#' @usage data(tutorial_3a_table_1)
#' @section Synonym:
#' T3AT1
#' @examples
#' # Load the data
#' data(tutorial_3a_table_1)
#'
#' # Or, alternatively load the data as
#' data(T3AT1)
#'
#' # View the structure
#' str(tutorial_3a_table_1)
#'
#' # Brief summary of the data.
#' summary(tutorial_3a_table_1)
#'
"tutorial_3a_table_1"


#' The data used in Tutorial 3A, Table 2
#'
#' Data from Tutorial 3A Table 2 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item Y.
#' \item X1.
#' \item X2.
#' \item X3.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name tutorial_3a_table_2
#' @aliases tutorial_3a_table_2 tutorial_3a_table_2 T3AT2 t3at2
#' @keywords datasets
#' @usage data(tutorial_3a_table_2)
#' @section Synonym:
#' T3AT2
#' @examples
#' # Load the data
#' data(tutorial_3a_table_2)
#'
#' # Or, alternatively load the data as
#' data(T3AT2)
#'
#' # View the structure
#' str(tutorial_3a_table_2)
#'
#' # Brief summary of the data.
#' summary(tutorial_3a_table_2)
#'
"tutorial_3a_table_2"


#' The data used in Tutorial 3A, Table 4
#'
#' Data from Tutorial 3A Table 4 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item group.
#' \item score.
#' \item X0.
#' \item X1.
#' \item X2.
#' \item x3.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name tutorial_3a_table_4
#' @aliases tutorial_3a_table_4 tutorial_3a_table_4 T3AT4 t3at4
#' @keywords datasets
#' @usage data(tutorial_3a_table_4)
#' @section Synonym:
#' T3AT4
#' @examples
#' # Load the data
#' data(tutorial_3a_table_4)
#'
#' # Or, alternatively load the data as
#' data(T3AT4)
#'
#' # View the structure
#' str(tutorial_3a_table_4)
#'
#' # Brief summary of the data.
#' summary(tutorial_3a_table_4)
#'
"tutorial_3a_table_4"


#' The data used in Tutorial 3A, Table 5
#'
#' Data from Tutorial 3A Table 5 of \emph{Designing Experiments and Analyzing Data: A Model Comparison Perspective} (3rd edition; Maxwell, Delaney, & Kelley).
#'
#' \itemize{
#' \item group.
#' \item score.
#' \item X0.
#' \item X1.
#' \item X2.
#' \item x3.}
#'
#' @author Ken Kelley \email{kkelley@nd.edu}
#' @source \url{https://designingexperiments.com/data/}
#' @source Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and
#' analyzing data: {A} model comparison perspective}. (3rd ed.). New York, NY: Routledge.
#' @references
#' Maxwell, S. E., Delaney, H. D., & Kelley, K. (2018). \emph{Designing experiments and analyzing data:
#' {A} model comparison perspective} (3rd ed.). New York, NY: Routledge.
#' @docType data
#' @name tutorial_3a_table_5
#' @aliases tutorial_3a_table_5 tutorial_3a_table_5 T3AT5 t3at5
#' @keywords datasets
#' @usage data(tutorial_3a_table_5)
#' @section Synonym:
#' T3AT5
#' @examples
#' # Load the data
#' data(tutorial_3a_table_5)
#'
#' # Or, alternatively load the data as
#' data(T3AT5)
#'
#' # View the structure
#' str(tutorial_3a_table_5)
#'
#' # Brief summary of the data.
#' summary(tutorial_3a_table_5)
#'
"tutorial_3a_table_5"
