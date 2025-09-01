\name{LRS.summary}
\alias{LRS.summary}

\title{ Summary of Lifetime Reproductive Success }

\description{
  
List of number of matings for each individual of the specified sex, over whole
lifetime and all years, as simulated by \code{aRetain}.  Must be run after 
\code{aRetain} is run with \code{trackall = TRUE}.

}
\usage{
LRS.summary(adata, sex)
}

\arguments{
 \item{adata}{ list of output from \code{aRetain} }
 \item{sex}{ sex to summarize; "male" or "female" }
}


\value{

A matrix with one row for each individual and two columns:

\item{ID}{ ID number of the individual. ID numbers are unique within each replicate, but will be repeated across replicates }
\item{NMatings}{ number of times the individual mated during its lifetime. The 
individual-level data is not expected to be useful, but can be used to calculate 
the mean or a histogram of mating success }

}
\references{ 

Weiser, E.L., Grueber, C. E., and I. G. Jamieson. 2012. AlleleRetain: A program 
to assess management options for conserving allelic diversity in small, isolated 
populations. Molecular Ecology Resources 12:1161-1167.

}

\author{ Emily L. Weiser \email{emily.l.weiser@gmail.com} }
