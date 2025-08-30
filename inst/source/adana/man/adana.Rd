\name{adana}
\alias{adana}
\title{Adaptive Nature-inspired Algorithms for Hybrid Genetic Optimization
}
\description{
The adana function is a GA function that can be used for any single-objective optimization problem.
}
\usage{
adana(gatype = "gga", objective = "max", maxiter = 100, initfunc = initbin, fitfunc,
      selfunc = seltour, crossfunc = px1, mutfunc = bitmut, replace = elitism,
      adapfunc = NULL, hgafunc = NULL, monitorfunc = NULL, n = 100, m = 8,
      lb = rep(0, 8), ub = rep(1, 8), nmode = "real", type = 1, permset = 0:9,
      prevpop = NULL, selt = 2, selbc = 0.5, selc = 2, selk = 1.005, sells = 1.5,
      selns = 0.5, selps = 0.5, sels = 1.5, selt0 = 50, selw = 2, reptype = FALSE,
      cxpc = 0.9, cxpc2 = 0.8, cxon = 2, cxk = 2, cxps = 0.5, cxa = 0, cxb = 0.15,
      cxealfa = 1, cxalfa = 0.5, mutpm = 0.05, mutpm2 = 0.2, mutb = 2, mutpow = 2,
      mutq = 0.5, mutmy = c(), mutsdy = c(), adapa = 0.75, adapb = 0.5, adapc = 0.1,
      adapd = 0.1, hgastep = 10, hgans = 1, hgaftype = "w", reps = 1, repk = 10,
      lambda = 1, tercrit = c(1), abdif = 1e-06, bestdif = 1e-06, objval = 0,
      optdif = 1e-06, rmcnt = 10, rmdif = 1e-06, phidif = 1e-06, rangedif = 1e-06,
      meandif = 1e-06, sddif = 1e-06, mincv = 0.001, simlev = 0.95, maxtime = 60,
      keepbest = TRUE, parfile = NULL, verbose = FALSE, ...)
}
\arguments{
  \item{gatype}{Type of GA. \itemize{\item{"gga": generational GA}  \item{"ssga": steady state GA} } {Default value is "gga"}}
  \item{objective}{Type of optimization. \itemize{\item{"min": minimization}  \item{"max": maximization} } {Default value is "max"}}
  \item{maxiter}{Maximum generation number 
  
               Default value is 100}
  \item{initfunc}{Name of fitness function. \itemize{\item{"initbin" : for binary-coded}  
                                                     \item{"initval" : for value-coded} 
                                                     \item{"initperm": for permutation-coded}
                                                     \item{or user-defined initialize function}} 
                 {Default value is "initbin"}}
  \item{fitfunc}{Fitness function}
  \item{selfunc}{Name of the selection function  
  
                 Default value is "seltour"}
  \item{crossfunc}{Name of the crossover function  
  
                 Default value is "px1"}
  \item{mutfunc}{Name of the mutation function  
  
                 Default value is "bitmut"}
  \item{replace}{Name of the replacement function  
  
                 Default value is "elitism"}
  \item{adapfunc}{Name of the adaptation function}
  \item{hgafunc}{The name of the function that will do the hybridization.}
  \item{monitorfunc}{Monitoring function}
  \item{n}{Population size}
  \item{m}{Length of chromosome}
  \item{lb}{A vector containing lower bounds for variables in value-coded problems}
  \item{ub}{A vector containing upper bounds for variables in value-coded problems}
  \item{nmode}{Value mode for initiating value-coding problems
               \itemize{\item{"integer"}  \item{"real"} } {Default value is "real"}}
  \item{type}{Integer indicating the type of initialization matrix.
              \itemize{\item{1 : contains the chromosome, fitness value and generation number.}
                       \item{2 : contains only the chromosome.} 
                      }
              }        
  \item{permset}{A vector containing the ordinal values to be used in permutation-coded initialization.
  
                 Default value is 0:9
}
  \item{prevpop}{A matrix containing previously prepared chromosomes during initialization.}
  \item{selt}{The tournament size for the seltour and seltour2 functions.
  
              Default value is 2.
}
  \item{selbc}{The base parameter for the selers function.
  
              Default value is 0.5.
}
  \item{selc}{Scale parameter for selsscale and selsscale2 functions
  
              Default value is 2.
}
  \item{selk}{Power parameter for selpscale function
  
              Default value is 1.005.
}
  \item{sells}{Scale parameter for sellscale function
  
              Default value is 1.5.
}
  \item{selns}{Polynomal coefficient for selnlrs function
  
              Default value is 0.5.
}
  \item{selps}{Cut-point threshold value for seltrunc function.
  
              Default value is 0.5.
}
  \item{sels}{Selection pressure for sellrs, sellrs3 and selrscale2 functions.
  
              Default value is 1.5.
}
  \item{selt0}{The starting temperature for selboltour function.
  
              Default value is 50.
}
  \item{selw}{Window size for selwscale function.
  
              Default value is 2.
}
  \item{reptype}{TRUE value is entered for the sampling with replacement when the seltour and seltour2 are used.
  
              Default value is FALSE.
}
  \item{cxpc}{Crossover ratio
  
              Default value is 0.9.
}
  \item{cxpc2}{Adapted crossover ratio for the leitingzhi function.}
  \item{cxon}{Number of offspring per mating in crossover.
  
              Default value is 2.
}
  \item{cxk}{Number of cut-points for multi-point crossover.
  
              Default value is 2.
}
  \item{cxps}{Probability threshold value for hux, ux, ux2, dc crossovers.
  
              Default value is 0.5.
}
  \item{cxa}{Location Parameter for lapx crossover
  
              Default value is 0.
}
  \item{cxb}{Scale Parameter for lapx crossover
  
              Default value is 0.15
}
  \item{cxealfa}{The random alpha value for sax, wax, ebx crosses. It is determined dynamically, but for some controlled studies, a fixed value can be assigned.}
  \item{cxalfa}{The random alpha value for sax, wax, ebx crosses. It is determined dynamically, but for some controlled studies, a fixed value can be assigned.}
  \item{mutpm}{Mutation rate
  
              Default value is 0.05
}
  \item{mutpm2}{Adaptive mutation rate}
  \item{mutb}{The exponent value used to avoid uniformity in unimut and nunimut2.
  
              Default value is 0.5
}
  \item{mutpow}{The exponent value for powmut and powmut2 functions.
  
              Default value is 2.
}
  \item{mutq}{A number. Value of q for bsearchmut1
  
              Default value is 0.5
}
  \item{mutmy}{A vector. Vector of means of genes}
  \item{mutsdy}{A vector. Vector of standard deviations of genes}
  \item{adapa}{Adaptation threshold a for leitingzhi function
  
              Default value is 0.5
}
  \item{adapb}{Adaptation threshold b for leitingzhi function
  
              Default value is 0.75
}
  \item{adapc}{Crossover adaptation threshold for adana3 function 
  
              Default value is 0.2
}
  \item{adapd}{Mutation adaptation threshold for adana3 function 
  
              Default value is 0.05
}
  \item{hgastep}{In a hybrid GA implementation, it is an integer indicating how many generations the hybrid optimization algorithm will be called.
  
              Default value is 10
}
  \item{hgans}{Number of individuals to be transferred to the Optim.
               
               Default value is 2}
  \item{hgaftype}{Types of fitness to transfer. \itemize{
    \item{w: individuals with the worst fitness value}
    \item{b: individuals with the best fitness value}
    \item{r: randomly selected individuals}
  }
    {Default value is "w"}
  }
  \item{reps}{The number of the best individuals to be selected when elitism is applied.
  
              Default value is 1.
}
  \item{repk}{The selection pressure parameter for the Round Robin function.
  
              Default value is 10.
}
  \item{lambda}{Total number of offspring in replacement algorithms in steady-state replacement type GAs
  
              Default value is 1.
}
  \item{tercrit}{A vector for termination criterion
  
              Default value is (1,13).
}
  \item{abdif}{It is an approach difference value used by the termination criterion 6.
  
              Default value is 1e-06.
}
  \item{bestdif}{The approach value to the global optimum value.
  
              Default value is 1e-06.
}
  \item{objval}{Global optimum. Used by some termination criterion. This criterion is used if the global optimum of the problem is known.}
  \item{optdif}{It is an approach difference value used by the termination criterion 3.
  
              Default value is 1e-06.
}
  \item{rmcnt}{k value used by the termination criterion 5.
  
              Default value is 10.
}
  \item{rmdif}{It is an approach difference value used by the termination criterion 5.
  
              Default value is 1e-06.
}
  \item{phidif}{It is an approach difference value used by the termination criterion 10.
  
              Default value is 1e-06.
}
  \item{rangedif}{It is an approach difference value used by the termination criterion 8.
  
              Default value is 1e-06.
}
  \item{meandif}{It is an approach difference value used by the termination criterion 4.
  
              Default value is 1e-06.
}
  \item{sddif}{It is an approach difference value used by the termination criterion 7.
  
              Default value is 1e-06.
}
  \item{mincv}{Minimum coefficient of variance used by the termination criterion 9.
  
              Default value is 0.001.
}
  \item{simlev}{It is an approach difference value used by the termination criterion 7.
  
              Default value is 0.99.
}
  \item{maxtime}{It is maximum runtime (minute) value used by the termination criterion 12.
  
              Default value is 60.
}
  \item{keepbest}{If the keepbest parameter is TRUE, the best solution value, chromosome, and generation number are saved in the list named bestsol.
  
              Default value is TRUE.
}
  \item{parfile}{The name of the file where the parameter values are defined if any.
  
              Default value is NULL.
}
  \item{verbose}{TRUE is assigned to display the statistics of fitness values obtained at the end of the loop in the GA study.
  
              Default value is FALSE.
}
  \item{\dots}{additional arguments to be passed to the \code{initialize}, \code{select}, \code{cross}, \code{mutate}, \code{hgafunc}, and \code{show} functions.}
}
\details{
\code{adana} function is a genetic algorithm function that can be used for all kinds of single-objective optimization problems. The adana function, unlike other GA packages, not only has adaptive GA functions, but also offers specially developed deterministic and self-adaptive techniques called \code{\link{adana1}}, \code{\link{adana2}}, and \code{\link{adana3}}, and is easily hybridized with other optimization methods inspired by nature. Besides, the adana function supports the use of monitors to monitor the progress of the GA run. In addition to containing many crossover and mutation operators, it is coded with a plug-and-play approach so that the user can add custom operator functions that he has developed.

The initialization population is created by using the name of the initialization function and other parameters passed to \code{initfunc} with the \code{\link{initialize}} function. The fit values of individuals in the population are calculated using \code{fitfunc}, which is passed to the \code{\link{evaluate}} function before each iteration. Then, the termination conditions are checked according to the criteria specified in the termination criteria (\code{tercrit}) argument via the \code{\link{terminate}} function. When the termination condition is not met, \code{adana} GA continues to run and searches for the best solution. If the \code{keepbest} argument is TRUE, the best solution value, chromosome, and generation number are saved in the list named \code{bestsol}.

Since the \code{adana} function allows adaptive GA studies, from which it is named, it runs a function that is passed with the \code{adapfunc} argument and contains the code of the adaptation algorithm. This adaptation function returns the crossover and mutation rates by recalculating. Thus, it strengthens the GA with its exploitation and exploration adaptations.

In order to determine the individuals to be selected for the mating pool, the selection process is done with the \code{selfunc} selection function transferred to the \code{\link{select}} function.

The crossover of selected individuals is executed with the crossover operator in the \code{crossfunc} argument passed to the \code{\link{cross}} function.

Mutation operations are performed with the mutation operator function assigned to \code{mutfunc} by the \code{\link{mutate}} function.

Finally, for GA, replacement is performed by passing the parent population and the offspring population to the \code{\link{replace}} function.

Hybridization with other optimization techniques can also be done before an iteration of the GA is complete. For this, the hybridization function passed to the \code{hgafunc} argument is used. Other parameters that need the optimization technique called in the hybridization function call, are also passed.

Progress made in each GA generation can be monitored visually with a monitor function. For this, the monitor function passed to the \code{monitorfunc} argument is used.

In GA implementations, if the required parameters for R functions that perform selection, crossover, mutation, renewal, and other operations are not entered in the function call, their default values are used. The user can change these parameters during the function call to suit the problem. However, there are many parameters used by the \code{adana} function and the functions it calls. It may be more practical to use the parameters by saving them to a file. The \code{parfile} argument can be used for this purpose.
}
\value{
  \item{genfits}{A matrix containing statistics for generations.}
  \item{initpop}{A matrix containing the initial population}
  \item{finalpop}{A matrix containing the final population}
  \item{bestsol}{Value of the best solution}
  \item{objective}{Objective of the optimization, \emph{min} or \emph{max}}
  \item{tcode}{Termination code}
}
\references{
Cebeci, Z. (2021). R ile Genetik Algoritmalar ve Optimizasyon Uygulamalari, 535 p. Ankara:Nobel Akademik Yayincilik. 
}
\author{
Zeynel Cebeci & Erkut Tekeli
}

\seealso{
GA Operators:
\code{\link{initialize}},
\code{\link{evaluate}},
\code{\link{terminate}},
\code{\link{select}},
\code{\link{cross}},
\code{\link{mutate}}

Initialize Functions:
  \code{\link{initbin}},
  \code{\link{initval}},
  \code{\link{initperm}},
  \code{\link{initnorm}}

Selection Functions:
  \code{\link{selrand}},
  \code{\link{selrswrp}},
  \code{\link{selrws}},
  \code{\link{selrws2}},
  \code{\link{selrss}},  
  \code{\link{selsus}},
  \code{\link{seldet}},
  \code{\link{selwscale}},
  \code{\link{selsscale}},
  \code{\link{selsscale2}},  
  \code{\link{sellscale}},
  \code{\link{selrscale}},
  \code{\link{selrscale2}},
  \code{\link{selpscale}},
  \code{\link{selescale}},
  \code{\link{seltour}},
  \code{\link{seltour2}},
  \code{\link{selboltour}},
  \code{\link{sellrs}},
  \code{\link{sellrs2}},
  \code{\link{sellrs3}},
  \code{\link{selnlrs}},
  \code{\link{selers}},
  \code{\link{seltrunc}} 

Crossover Functions:
  \code{\link{px1}},
  \code{\link{kpx}},
  \code{\link{sc}},
  \code{\link{rsc}},
  \code{\link{hux}},
  \code{\link{ux}},
  \code{\link{ux2}},
  \code{\link{mx}},
  \code{\link{rrc}},
  \code{\link{disc}},
  \code{\link{atc}},
  \code{\link{cpc}},
  \code{\link{eclc}},
  \code{\link{raoc}},
  \code{\link{dc}},
  \code{\link{ax}},
  \code{\link{hc}},
  \code{\link{sax}},
  \code{\link{wax}},
  \code{\link{lax}},
  \code{\link{bx}},
  \code{\link{ebx}},
  \code{\link{blxa}},
  \code{\link{blxab}},
  \code{\link{lapx}},
  \code{\link{elx}},
  \code{\link{geomx}},
  \code{\link{spherex}},
  \code{\link{pmx}},
  \code{\link{mpmx}},
  \code{\link{upmx}},
  \code{\link{ox}},
  \code{\link{ox2}},
  \code{\link{mpx}},
  \code{\link{erx}},
  \code{\link{pbx}},
  \code{\link{pbx2}},
  \code{\link{cx}},
  \code{\link{icx}},
  \code{\link{smc}}

Mutation Functions:
  \code{\link{bitmut}},
  \code{\link{randmut}},
  \code{\link{randmut2}},
  \code{\link{randmut3}},
  \code{\link{randmut4}},
  \code{\link{unimut}},
  \code{\link{boundmut}},
  \code{\link{nunimut}},  
  \code{\link{nunimut2}},
  \code{\link{powmut}},   
  \code{\link{powmut2}},   
  \code{\link{gaussmut}},   
  \code{\link{gaussmut2}},   
  \code{\link{gaussmut3}},   
  \code{\link{bsearchmut1}},        
  \code{\link{bsearchmut2}},   
  \code{\link{swapmut}},   
  \code{\link{invmut}},   
  \code{\link{shufmut}},   
  \code{\link{insmut}},  
  \code{\link{dismut}},
  \code{\link{invswapmut}},
  \code{\link{insswapmut}},
  \code{\link{invdismut}}
 
Replacement Functions:
 \code{\link{grdelall}},
 \code{\link{elitism}},
 \code{\link{grmuplambda}}, 
 \code{\link{grmuplambda2}}, 
 \code{\link{grmuplambda3}}, 
 \code{\link{grmuplambda4}}, 
 \code{\link{grmuvlambda}}, 
 \code{\link{grrobin}}, 
 \code{\link{ssrmup1}}, 
 \code{\link{ssrmup1}}, 
 \code{\link{ssrgenitor}}, 
 \code{\link{ssrfamtour}}, 
 \code{\link{ssrx}}

Adaptation Functions:
\code{\link{fixpcmut}},
\code{\link{ilmdhc}},
\code{\link{adana1}},
\code{\link{adana2}},
\code{\link{adana3}},
\code{\link{leitingzhi}}

Hybridization Functions:
\code{\link{hgaoptim}},
\code{\link{hgaoptimx}},
\code{\link{hgaroi}}
}
\examples{
# Preparing data
material = c("knife", "tin", "potato", "coffee", "sleeping bag", "rope", "compass")
weigth = c(1, 5, 10, 1, 7, 5, 1)
point = c(10, 20, 15, 2, 30, 10, 30)
kspdata = data.frame(material, weigth, point)
capacity = 100

# Fitness Function
kspfit2 = function(x, ...) {
  tpoint = x \%*\% kspdata$point
  tweigth = x \%*\% kspdata$weigth
  G1 = tweigth-capacity 
  fitval = tpoint-max(0,G1)^2     
  return(fitval)
}

# Run GA
n = 20
m = nrow(kspdata)
niter = 100
kspGGA = adana(n=n, m=m, maxiter=niter, objective="max", gatype="gga",
  initfunc=initbin, fitfunc=kspfit2, selfunc=seltour, 
  crossfunc=kpx, mutfunc=bitmut, replace=elitism,
  selt=2, reps=4, repk=5, cxon=2, cxk=3, cxpc=0.8, 
  mutpm=0.05, tercrit=c(1), keepbest=TRUE,
  verbose=TRUE, monitorfunc=monprogress)
  
head(kspGGA$finalpop)        # Display Final Population
head(kspGGA$genfits)         # Display Fitness Values According to Generations
bestsol(kspGGA)              # Display Best Solution  
kspdata[kspGGA$bestsol$chromosome == 1, ]    # Display Best Chromosome
}
