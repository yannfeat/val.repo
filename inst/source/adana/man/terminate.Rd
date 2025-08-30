\name{terminate}
\alias{terminate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Termination Control Function
}
\description{
The function of terminating the genetic algorithm
}
\usage{
terminate(tercrit, maxiter, objective, t, genfits,
                     fitvals, objval, optdif, rmcnt, rmdif, abdif, mincv,
                     sddif, rangedif, simlev, phidif, meandif, bestdif,
                     stime, maxtime)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tercrit}{A vector. Indications of termination criteria.}
  \item{maxiter}{Maximum iteration}
  \item{objective}{?????}
  \item{t}{Generation number}
  \item{genfits}{A matrix. Best fitness of each generation}
  \item{fitvals}{Fitness values of current generation}
  \item{objval}{Global optimum value}
  \item{optdif}{Difference from global optimum value}
  \item{rmcnt}{k value for minimum difference of the mean of the last k best fitness values.}
  \item{rmdif}{The minimum difference between the mean of the last k best fitness values and the best fitness value in the current generation.}
  \item{abdif}{Minimum difference between best fitness value and mean of fitness values}
  \item{mincv}{Minimum coefficient of variance}
  \item{sddif}{The minimum difference between the last two standard deviations.}
  \item{rangedif}{Minimum and maximum difference (range of change)}
  \item{simlev}{Similarity percentage of fitness values}
  \item{phidif}{Phi convergence}
  \item{meandif}{The minimum difference between the last two fitness values}
  \item{bestdif}{Percentage of difference between the last two best fitness values}
  \item{stime}{System time saved before starting GA}
  \item{maxtime}{Maximum running time}
}
\value{
Termination criterion

    0 : No termination
    
    1 : Maximum iteration
    
    2 : Reaching the global optimum value
    
    3 : Converging the global optimum
    
    4 : The minimum difference between the last two fitness values
    
    5 : Percentage of difference between the last two best fitness values
    
    6 : Minimum difference of the mean of the last k best fitness values
    
    7 : Minimum difference between best fitness value and mean of fitness values
    
    8 : The minimum difference between the last two standard deviations.
    
    9 : Minimum and maximum difference (range of change)
    
    10: Minimum coefficient of variance
    
    11: Phi convergence
    
    12: Similarity percentage of fitness values
    
    13: Maximum running time
}
\author{
Zeynel Cebeci & Erkut Tekeli
}
