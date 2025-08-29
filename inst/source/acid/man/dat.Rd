\name{dat}
\alias{dat}
\docType{data}
\title{
ACID Simulated Data
}
\description{ This is some simulated income data from a mixture model as used in Sohn et al (2014).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{data(dat)}
\format{
  The format is:
List of 4
 $ dag.para:'data.frame':	8 obs. of  1 variable:
  ..$ parameters: num [1:8] 0.2 0.1 0 25000 20000 5 0.5 1
 $ dag.s   :'data.frame':	100 obs. of  3 variables:
  ..$ cat: int [1:100] 3 1 3 1 2 3 3 1 3 3 ...
  ..$ y  : num [1:100] 36410 0 58165 0 15034 ...
  ..$ w  : int [1:100] 1 1 1 2 1 3 2 1 1 1 ...
 $ LN.para :'data.frame':	6 obs. of  1 variable:
  ..$ parameters: num [1:6] 0.2 0.1 0 25000 10 2
 $ LN.s    :'data.frame':	100 obs. of  3 variables:
  ..$ cat: int [1:100] 3 3 1 3 3 3 3 3 3 3 ...
  ..$ y  : num [1:100] 29614 29549 0 33068 463941 ...
  ..$ w  : int [1:100] 1 2 1 1 1 1 1 2 1 1 ...
}
\details{ The data contains information on whether the person is unemployed (cat=1), precariously employed (cat=2) or in standard employment(cat=3), the corresponding parameters used to generate the truncated distribution - both for Log-normal and Dagum.
}
\references{ 
Sohn, A., Klein, N., Kneib. T. (2014): A New Semiparametric Approach to Analysing Conditional Income Distributions, in: SOEPpapers, No. 676.
}
\examples{
data(dat)
str(dat)
}
\keyword{datasets}
