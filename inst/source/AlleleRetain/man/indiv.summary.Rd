\name{indiv.summary}
\alias{indiv.summary}

\title{ Summary of Individual Information }

\description{
  
Summarize information for individuals of each origin (starters, supplementals,
locals, immigrants) simulated by \code{aRetain}.  Must be run after 
\code{aRetain} is run with \code{trackall = TRUE}.

}
\usage{
indiv.summary(adata, genlength, alpha=0.05)
}

\arguments{
 \item{adata}{ list of output from \code{aRetain} }
 \item{genlength}{ mean age of simulated individuals (breeding pairs only), as 
 returned by \code{census.summary} (use the MeanAge value where it stabilizes 
 after recovering from any founder age effects) }
 \item{alpha}{ confidence level e.g. 0.05 for 95\% intervals}
}


\value{

A matrix with 4 rows, one for each origin (starter, supplemental, local, 
immigrant) and the columns are summary variables:

\item{n}{ total number of individuals from each origin, summed within each replicate and then averaged across replicates }
\item{pbreed}{ probability of each individual breeding (proportion that bred) }
\item{pbreed.LCL}{ lower confidence limit for pbreed }
\item{pbreed.UCL}{ upper confidence limit for pbreed }
\item{YrsBred}{ mean # years bred per individual (including those that never 
bred) }
\item{YrsBredBr}{ mean # of years bred per individual (that bred at least once) }
\item{lifespan}{ mean lifespan in the population }
\item{effectivegen}{ mean # of individuals that bred each generation }
\item{NMatings}{ mean # of total lifetime matings, across individuals }

}
\references{ 

Weiser, E.L., Grueber, C. E., and I. G. Jamieson. 2012. AlleleRetain: A program 
to assess management options for conserving allelic diversity in small, isolated 
populations. Molecular Ecology Resources 12:1161-1167.

}

\author{ Emily L. Weiser \email{emily.l.weiser@gmail.com} }
