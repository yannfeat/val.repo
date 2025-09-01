\name{aRetain.summary}
\alias{aRetain.summary}

\title{ Summarize Demographics and Allele Retention from \code{aRetain} }

\description{
  
Summarize pertinent information about the population simulated by \code{aRetain}.

}
\usage{

aRetain.summary(adata, GeneCount, alpha=0.05, dropextinct = FALSE)

}

\arguments{
 \item{adata}{ list of output from \code{aRetain} }
 \item{GeneCount}{ "adult" or "all" as specified for \code{aRetain} }
 \item{alpha}{ confidence level e.g. 0.05 for 95\% intervals}
 \item{dropextinct}{ logical: should simulated populations that decline to zero 
 be omitted from the summary? }
}


\value{

A matrix in which the rows correspond to simulation years (0,...,nyears) and
the columns are summary variables:

\item{MeanN}{ mean number of adults }
\item{SEN}{ SE for # adults }
\item{MeanNNonbr}{ mean # of nonbreeders }
\item{MeanBrF}{ mean # of breeding females }
\item{SEBrF}{ SE for # of breeding females }
\item{MeanBrM}{ mean # of breeding males }
\item{SEBrM}{ SE for # of breeding males }
\item{MeanNFound}{ mean # of founders (starters and supplemental 
individuals) remaining }
\item{MeanNMigr}{ mean # of immigrants present }
\item{MeanAge}{ mean age of breeding adults (those that are actually 
paired that year) }
\item{P.extant}{ probability that the population is extant (proportion of 
simulations in which the population does not become extinct) }
\item{P.xLCL}{ lower bound of the confidence interval for P.extant }
\item{P.xUCL}{ upper bound of the confidence interval for P.extant }
\item{P.retain}{ probability that the rare allele is present in the 
population }
\item{P.LCL}{ lower bound of the confidence interval for P.retain }
\item{P.UCL}{ upper bound of the confidence interval for P.retain }
\item{A.Freq}{ frequency of the rare allele in the simulated population (in adults or in all individuals, as per GeneCount argument in aRetain) }
\item{A.SE}{ standard error for A.Freq }

}
\references{ 

Weiser, E.L., Grueber, C. E., and I. G. Jamieson. 2012. AlleleRetain: A program 
to assess management options for conserving allelic diversity in small, isolated 
populations. Molecular Ecology Resources 12:1161-1167.

}

\author{ Emily L. Weiser \email{emily.l.weiser@gmail.com}  }


