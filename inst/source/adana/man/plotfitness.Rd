\name{plotfitness}
\alias{plotfitness}

\title{
Fitness statistics graph by GA generations
}
\description{
Fitness statistics graph by GA generations
}
\usage{
plotfitness(genfits, options)
}
\value{No return value, called for the side effect of drawing a plot.}
\arguments{
  \item{genfits}{A matrix. Best fitness of each generation}
  \item{options}{A vector. Statistics to be plotted. 
                  \itemize{ \item{1: minimum} \item{2: maximum} \item{3: average} \item{4: Q1}   \item{5: median}  \item{6: Q3} }}
}
\author{
Zeynel Cebeci & Erkut Tekeli
}
