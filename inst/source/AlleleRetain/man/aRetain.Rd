\name{aRetain}
\alias{aRetain}

\title{ Allele Retention, Inbreeding, and Demography }

\description{
  
Simulate the effect of management or demography on allele retention 
and inbreeding accumulation in bottlenecked populations of animals 
with overlapping generations. 

}
\usage{

aRetain(q0 = 0.05, sourceN = Inf, 
	startN = 20, startAge = "juvenile", startSR = 0.5, exactSSR= FALSE, 
	inisurv = c(1,1,1), addN = 0, addyrs = c(0),  addSR = 0.5, exactASR = FALSE,
	migrN = 0, migrfreq = 1, migrSR = 0.5, exactMSR = FALSE, mpriority = FALSE, 
	removeL = FALSE, harvN = 0, harvAge = "all", harvyrs = 0, K = 100, Klag = 0,
	KAdults = FALSE, reprolag = 0, mature = 1, matingSys = "monogamy", 
	matingLength = "seasonal", meanMLRS = 1, sdMLRS = 0, reproAgeM = c(1:200), 
	AgeOnMLRS = "age/age", nMatings = 1, retainBreeders = "male", MaxAge = 25, 
	SenesAge = 10, adsurvivalF = 0.80, adsurvivalM = 0.80, nonbrsurv = 0.80, 
	nonbrsurvK = 0.80, juvsurv = 0.80,  juvsurvK = 0.80, youngperF = 1.5, 
	SDypF = 0.25, ypF1 = 1, ypF1yr = 1, MAXypF = 2, MAXypFK = 2, 
	ypFsex = "female", youngSR = 0.5, trackall = TRUE, GeneCount = "adult", 
	nyears = 50, nrepl = 100, nreplprint = 10, printplots = FALSE)

}

\arguments{
 \item{q0}{ frequency of rare allele in the source population (range 0-1) }
 \item{sourceN}{ size of source population; must be > startN }
 \item{startN}{ number of starters (or size of bottleneck); not all will 
 become genetic founders; minimum 2 }
 \item{startAge}{ age class ("juvenile", "young adult", or "adult") of 
 starters, supplemental.  If "juvenile", all individuals added are assigned
 age 0; if "young adult", all are assigned age at maturity; if "adult", 
 ages are selected randomly based on the proportion of individuals in the 
 source population expected to be of each age (based on the survival rates 
 and senescence specified below) }
 \item{startSR}{ sex ratio (proportion male) of starters, supplementals, 
 and migrants }
 \item{exactSSR}{ logical: whether startSR gives the exact sex ratio 
 of individuals released }
 \item{inisurv}{ initial survival rate, as a proportion (range 0-1), of 
 individuals released.  Given as a vector, where the first value is for 
 starters, the second is for additional releases, and third is for migrants.  
 Annual mortality applies after this value is used.  Defaults to 1 for all three 
 groups. }
 \item{addN}{ list of numbers of individuals to release in years soon after 
 population establishment ("supplementals") }
 \item{addyrs}{ list of years in which to release supplementals.  Each year 
 corresponds to the number of individuals in the same position in the addN 
 list }
 \item{addSR}{ sex ratio (proportion male) of supplementals; defaults to 0.5 
 (must be between 0 and 1).  This can be either a single value, or a vector, 
 with each element in the vector corresponding to each instance of 
 supplementation (must be the same length as addN)}
 \item{exactASR}{ whether addSR gives the exact sex ratio of individuals 
 released (TRUE) or sexes are assigned randomly based on the probability given 
 by addSR (FALSE); defaults to FALSE.  This can be either a single value, or a 
 vector, with each element in the vector corresponding to each instance of 
 supplementation (must be the same length as addN)}
 \item{migrN}{ number of migrants to add }
 \item{migrfreq}{ interval (number of years) at which to add migrN 
 migrants; must be between 1 and nyears, below }
 \item{migrSR}{ sex ratio (proportion male) of supplementals; defaults to 0.5 
 (must be between 0 and 1) }
 \item{exactMSR}{ whether addSR gives the exact sex ratio of individuals 
 released (TRUE) or sexes are assigned randomly based on the probability given 
 by migrSR (FALSE); defaults to FALSE }
 \item{mpriority}{ logical:  whether migrants are given priority over 
 locally produced offspring to recruit into any available breeding 
 vacancies }
 \item{removeL}{ logical: whether to remove the corresponding number of 
 locally produced adults to make room for migrants in the population; only 
 necessary if retainBreeders = "both"/"female"/"male"; will only come into 
 play when population is at K }
 \item{harvN}{ number to be removed in each harvest year }
 \item{harvAge}{ age of individuals to be harvested (as for 'startAge'). If not 
 enough individuals of this age are available, the harvest quota (harvN) will 
 not be filled }
 \item{harvyrs}{ vector of years in which harvest occurs }
 \item{K}{ carrying capacity (population ceiling) }
 \item{Klag}{ number of years for which population is held at or below 
 initial size (breeding still occurs); indicates a prolonged bottleneck }
 \item{KAdults    }{ logical: whether K counts only adults, or all 
 individuals (subadults, nonbreeders, helpers) }
 \item{reprolag    }{ number of years after establishment for which no 
 reproduction occurs}
 \item{mature}{ average age (in years) at sexual maturity (first breeding) }
 \item{matingSys}{ mating system: "monogamy", "polygyny", or 
 "polygynandry"; to model a polyandrous system, set to "polygyny" and then 
 input female values for the "male" parameters (and male values for the 
 "female" parameters) }
 \item{matingLength}{ "seasonal" or "lifelong"; determines whether 
 individuals retain the same mate from year to year or divorce }
 \item{meanMLRS}{ mean lifetime reproductive success (LRS), in terms of 
 number of matings that produce young (NOT number of offspring) a male gets 
 over his lifetime; including those that never reproduce.  Each male will 
 be assigned an individual average from a gamma distribution with this mean 
 and sdMLRS (the shape of the gamma function = (meanMLRS^2)/(sdMLRS^2); 
 scale = (sdMLRS^2)/meanMLRS; see help for R function "rgamma" for more 
 information).  The gamma distribution was chosen because of its 
 flexibility in shape appropriate to polygynous mating systems (from 
 strongly right-skewed to nearly symmetrical).  The SD:mean ratio is more 
 important than the magnitude of the mean.  This individual mean indicates 
 the male's "quality" and will be used to assess his chance of mating, 
 relative to other males present, each year (does not translate directly 
 into actual LRS experienced by that male).  Not used if matingSys = 
 "monogamy" }
 \item{sdMLRS}{ among-male standard deviation in LRS; used with meanMLRS as 
 described above }
 \item{reproAgeM}{ list of ages at which males are able to mate 
 successfully }
 \item{AgeOnMLRS}{ expression describing the proportion of LRS achieved by 
 a male at a particular age (for ages contained within reproAgeM), e.g. 
 "-5.4 + 1.5*age - 0.08*age^2"; if there is no effect of age, use the 
 default value of "age/age" (equals 1 so all ages will be assigned the same 
 average, given by meanMLRS) }
 \item{nMatings}{ number of matings per female each year; only used when 
 matingSys = "polygynandry"; must be a whole number }
 \item{retainBreeders}{ should established breeders retain their breeding 
 status from year to year, and prevent young individuals from recruiting if 
 the population is at K?  Specify which sex should be retained: "none", 
 "both", "male", or "female".  Only used when matingSys = "monogamy."  When 
 "none", all new recruits are added to the breeding population; individuals 
 are randomly removed from that pool to truncate the population at K (so 
 new recruits may randomly replace established breeders).  When adults will 
 likely survive and prevent new individuals from recruiting, e.g. with 
 territorial species, set this at one of the other values as appropriate 
 for your species.  When pairing off widowed or divorced individuals, those 
 of the retained sex(es) that bred previously will be guaranteed a new mate 
 (if available); non-retained adults will compete with new recruits to mate 
 with available adults.  If the population is at K, new recruits will only 
 fill vacancies left by adults that died (they will not replace any 
 surviving adults, including females when retainBreeders = "male" and vice 
 versa; i.e. "both" functions the same as "male" and "female" in this part 
 of the model) }
 \item{MaxAge}{ maximum allowable lifespan (in years); can be Inf }
 \item{SenesAge}{ age (in years) after which annual survival will be 
 reduced by senescence.  Through this age, adult survival values are set 
 according to adsurvivalF and adsurvivalM (below).  After this age, annual 
 survival decreases linearly until MaxAge (at which it is 0):  new survival 
 =  survival - (survival / (MaxAge - SenesAge)) * (age - SenesAge) }
 \item{adsurvivalF}{ annual survival rate of adult females }
 \item{adsurvivalM}{ annual survival rate of adult males }
 \item{nonbrsurv}{ annual survival rate of nonbreeders (subadults or adults 
 that have never reproduced) }
 \item{nonbrsurvK}{ annual survival rate of nonbreeders when population is 
 at K (used instead of nonbrsurv).  If given, subadult survival probability 
 in each year depends on density of the population at the beginning of that 
 year, according to the Beverton-Holt function for density dependence in 
 survival (as in Morris & Doak 2002, Quantitative Conservation Biology):  
 S(E(t)) = S(0)/(1 + beta * E(t)), where S(E(t)) is survival rate at 
 population density E in year t, S(0) is survival when density is near 0, 
 beta is the decline in survival as density increases, and E(t) is 
 population density at time t.  "Density" is defined in our model as the 
 proportion of K that has been filled, as there is no spatial information 
 in the model.  The model solves for beta according to the user-specified 
 values for nonbrsurv (S(0)) and nonbrsurvK (S at carrying capacity, where 
 E = 1), then uses beta and S(0) to calculate density-dependent survival 
 probability in each year }
 \item{juvsurv}{ first year survival (from the stage described by 
 youngperF, below, to the beginning of the next breeding season) when 
 population is below K }
 \item{juvsurvK}{ first year survival when population is at K (used instead 
 of juvsurv); juvenile survival is density-dependent as for nonbrsurvK }
 \item{youngperF}{ average number of offspring produced per mating each 
 year (averaged over all pairs in population).  For a polyandrous female, 
 this is the average number of offspring produced each time she mates with 
 a male (each year): youngperF * nMatings = total average offspring per 
 year.  youngperF can be calculated for any reproductive stage (eggs, 
 chicks, independent juveniles) as long as juvsurv indicates the proportion 
 of individuals that survive from this stage to the beginning of the 
 following breeding season }
 \item{SDypF}{ among-individual standard deviation of youngperF, e.g. 0.50 
 or 2.}
 \item{ypF1}{ where younger breeders have reduced reproductive rates, this 
 can be used to define the reproductive success for first reproductive 
 stage (length of that stage is determined by ypF1yr, below); given as a 
 proportion of youngperF.   }
 \item{ypF1yr}{ age after which ypF1 changes to youngperF (e.g. 1 if ypF1 
 applies to one-year-olds only, or 5 if it applies for the first 5 years 
 and then increases to youngperF from age 6 onward) }
 \item{MAXypF}{ maximum annual number of offspring per individual (e.g. 
 based on biological constraints such as clutch size/renesting) }
 \item{MAXypFK}{ maximum annual number of offspring per individual when 
 population is at K, if different from MAXypF }
 \item{ypFsex}{ which member of a pair limits the reproductive output for 
 the year, based on the biology of the species of interest; can be "male", 
 "female", or "both" (the last will average the male's and female's values) }
 \item{youngSR}{ proportion of offspring that are male }
 \item{trackall}{ logical: whether to track all individuals from the 
 population through the whole simulation; must be TRUE to use indiv.summary 
 or pedigree.summary after running the simulation }
 \item{GeneCount}{ which alleles to count as retained:  those in the "adult" 
 population only, or those in "all" individuals (including subadults and other 
 nonbreeders)}
 \item{nyears}{ number of years to run the simulation }
 \item{nrepl}{ number of iterations (replicates) to run }
 \item{nreplprint}{ interval (number of replicates) at which to print a 
 message with the current system time.  Allows the user to gauge model 
 progress and to estimate time to completion }
 \item{printplots}{ logical: whether to plot the population growth (number of 
 individuals, as defined by KAdults, present each year) and allele frequency (in 
 the pool defined by GeneCount) as they change over time (TRUE or FALSE).  One 
 line will be plotted for each replicate immediately after it runs.  Can be used 
 to immediately gauge the demographics of the population (e.g. if it will grow 
 as expected); will slow down the simulation by ~ 10-20 percent.  Defaults to 
 FALSE. }
}

\details{

The user indicates the number of individuals released to establish the 
population (bottleneck size), along with their age class (adult or 
juvenile), sex ratio, and post-release survival rate. These individuals can 
be released all at once or gradually over a period of years. Released 
individuals are assumed to be unrelated and are randomly assigned genotypes 
(zero, one, or two copies of a hypothetical neutral allele) according to a 
user-specified frequency of the allele in the source population. The 
simulated population can be held at the initial size to simulate a 
prolonged bottleneck (while reproduction occurs) or the user can specify a 
post-release lag with no reproduction. Otherwise, released individuals 
mature and recruit according to user-specified parameters. Species with 
delayed sexual maturity are modeled by preventing subadults from breeding 
until a user-specified age. Adults are randomly sorted into monogamous or 
polygynous breeding pairs, which re-form for multiple matings within one 
season (in polygynandry systems), remain together seasonally (in monogamous 
or polygynous systems), or remain together lifelong (in monogamous systems) 
depending on user-specified settings. Reproduction occurs each year based 
on individual or population means (depending on settings for individual 
variation in reproductive output) for the number of offspring produced per 
pair per year, which can change with age of the parents. Sex is randomly 
assigned to each offspring according to the user-specified sex ratio for 
juveniles and offspring inherit alleles from their parents via Mendelian 
inheritance. When the simulated population reaches the specified carrying 
capacity, population growth ceases by either preventing recruitment of 
offspring or removing randomly selected individuals (as indicated by the 
user).  When individuals are removed at random, the user can choose to give 
priority to remain in the population to established breeders and/or 
immigrants (if any). Annual survival of individuals is determined by 
user-specified probabilities for each age class, with optional sex-specific 
survival rates for adults. The user can choose to incorporate density 
dependence into juvenile and subadult/nonbreeder survival probabilities 
and/or to specify senescence effects in survival of adults beyond a certain 
age.

The model simulates immigration at regular user-specified intervals by 
adding more individuals from the original source population. Immigrants can 
be prioritized over locally produced individuals to recruit into breeding 
vacancies. This scenario may be realistic when most locally produced 
juveniles would emigrate, or when locally produced juveniles would be 
removed by managers (e.g. to contribute to other populations or to make 
room for immigrants). Emigration can be simulated in this model by reducing 
the survival value for the appropriate age class, assuming emigrants and 
their descendants do not return to the modeled population. 

Frequency of the allele of interest in the simulated population is recorded 
after each year of the simulation. The proportion of iterations in which 
the allele is retained to the end of the specified time period is 
interpreted as the probability of retaining the allele in the population 
over that period. The model outputs a census of the simulated 
population (adults, subadults/nonbreeders, pairs, founders, and immigrants) 
each year, and a list of information for each individual in the simulated 
population.  The census information can be averaged across replicates with 
\code{aRetain.summary}.  An individual summary function (\code{indiv.summary})
can output pertinent information (e.g. proportion bred, average number that 
bred per generation) about individuals of each origin (founder, local, or 
immigrant) in the population.  A pedigree summary function 
(\code{pedigree.summary}) can be used to describe accumulation of inbreeding 
in the population by calling R package \bold{pedigree}.  

See the AlleleRetain Model Guide (available as a PDF file with this 
installation, or from the model's website at 
https://sites.google.com/site/alleleretain/) for more details and an example.

}

\value{
\code{aRetain} returns a list of sub-lists, with one sub-list for each replicate 
which contains two matrices: one that summarizes the population information for 
that replicate (one row for each year; columns = number of adults, number of 
breeding females, number of breeding males, number of copies of the rare allele, 
number of pairs, number of nonbreeders, number of founders remaining, number of 
migrants alive, mean age of breeding pairs), and one that gives pertinent 
information for all individuals in that replicate (one row for each individual; 
columns = ID, dam, sire, origin [1=starter, 2=supplemental, 3=local, 
4=immigrant], birth year, number of years alive in the population, number of 
years bred, year added to population).   The latter matrix includes all 
individuals if \code{trackall = TRUE}, or only those alive at the end of the 
simulation if \code{trackall = FALSE}.  Run \code{aRetain.summary}, 
\code{indiv.summary}, and \code{pedigree.summary} to generate useful output from 
this information (averaged across replicates).

}
\references{ 

Weiser, E.L., Grueber, C. E., and I. G. Jamieson. 2012. AlleleRetain: A program 
to assess management options for conserving allelic diversity in small, isolated 
populations. Molecular Ecology Resources 12:1161-1167.

}

\author{ Emily L. Weiser \email{emily.l.weiser@gmail.com} }

\examples{

## Simulate rare allele with frequency 0.05 in source population.
## 20 individuals are translocated to establish a new population with carrying 
## capacity = 100 individuals.  An additional 5 individuals are translocated 
## once every 10 years as immigrants. Demographic parameter values are for a 
## monogamous passerine bird species with a 5-yr average lifespan.  Only 30 
## iterations are run so the confidence intervals will be wide.

\dontrun{

aR.temp <- aRetain(q0 = 0.05, sourceN = 2000, startN = 20, startAge = 
"adult", startSR = 0.5, exactSSR = FALSE, inisurv = 0.90, addN = 0, addyrs 
= c(0),  migrN = 5, migrfreq = 10, mpriority = FALSE, removeL = FALSE, K = 
100, KAdults = FALSE, mature = 1, matingSys = "monogamy", matingLength = 
"seasonal", retainBreeders = "male", MaxAge = 16, SenesAge = 5, adsurvivalF = 
0.77, adsurvivalM = 0.77, nonbrsurv = 0.70, nonbrsurvK = 0.40, juvsurv = 0.60,  
juvsurvK = 0.30, youngperF = 2.85, SDypF = 1.25, MAXypF = 6, MAXypFK = 6, 
ypFsex = "female", youngSR = 0.5, trackall = TRUE, GeneCount = "all", nyears = 
10, nrepl = 20, nreplprint = 10)

aRetain.summary(aR.temp,  GeneCount = "all", alpha=0.05, dropextinct = TRUE)

}

}

