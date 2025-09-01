\name{pedigree.summary}
\alias{pedigree.summary}

\title{ Pedigree Simulation and Summary }

\description{
  
Summarizes inbreeding accumulation in the population simulated by 
\code{aRetain}. Must be run after \code{aRetain} is run with 
\code{trackall = TRUE}.  
Requires package \bold{pedigree}.
 
}

\usage{

pedigree.summary(adata)

}

\arguments{
 \item{adata}{ list of output from \code{aRetain} }
}


\value{

A matrix in which the rows correspond to simulation years (0,...,nyears) and
the columns are summary variables:

\item{year}{ year of simulation }
\item{meanF}{ mean F, averaged across individuals and then across replicates }
\item{varF}{ inter-replicate variance in F }
\item{indivVarF}{ inter-individual variance of F, averaged across replicates }

}
\references{ 

Weiser, E.L., Grueber, C. E., and I. G. Jamieson. 2012. AlleleRetain: A program 
to assess management options for conserving allelic diversity in small, isolated 
populations. Molecular Ecology Resources 12:1161-1167.

}

\author{ Emily L. Weiser \email{emily.l.weiser@gmail.com}  }


