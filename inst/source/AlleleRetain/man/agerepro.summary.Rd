\name{agerepro.summary}
\alias{agerepro.summary}

\title{ Summary of Average Reproductive Success by Age }

\description{
  
average reproductive success, by age and sex, in the final year of the simulation
by \code{aRetain}.  Must be run after \code{aRetain} is run with \code{trackall =
TRUE}.

}
\usage{
agerepro.summary(adata, maxage, sex)
}

\arguments{
 \item{adata}{ list of output from \code{aRetain} }
 \item{maxage}{ value of MaxAge used in \code{aRetain}, or the maximum age of 
 interest for the summary, whichever is smaller }
 \item{sex}{ sex to summarize; "male" or "female" }
}


\value{

A matrix with one row for each age and seven columns:

\item{age}{ age of each individual, from 0 to MaxAge }
\item{alive.mean}{ mean number of individuals that lived to this age }
\item{alive.sd}{ SD across replicates in the number of individuals that lived 
to this age }
\item{matings.mean}{ mean number of matings per individual at this age }
\item{matings.sd}{ SD among individuals in matings.mean }
\item{offspring.mean}{ mean number of offspring produced by individuals 
of this age }
\item{offspring.sd}{ SD among individuals in offspring.mean }

}
\references{ 

Weiser, E.L., Grueber, C. E., and I. G. Jamieson. 2012. AlleleRetain: A program 
to assess management options for conserving allelic diversity in small, isolated 
populations. Molecular Ecology Resources 12:1161-1167.

}

\author{ Emily L. Weiser \email{emily.l.weiser@gmail.com} }
