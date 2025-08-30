\encoding{latin1}
\name{ara}
\alias{ara}
\docType{data}
\title{
Araucaria Seedlings
}
\description{
Counts of \emph{Araucaria araucana} (Molina) K. Koch seedlings in 26 plots (0.2 ha each) to investigate the effect of management (small stakeholders vs. forestry companies) and cattle pressure (measured indirectly by the number of dungs in each plot). 
}
\usage{data("ara")}
\format{
  A data frame with 26 observations on the following 3 variables.
  \describe{
    \item{\code{seedlings}}{Number of seedlings in each plot.}
    \item{\code{dungs}}{Number of cattle dungs in each plot.}
    \item{\code{property}}{Owner of the terrain where each plot was set up. A factor with 2 levels, \code{Campesino} (small stakeholder) and \code{Empresa Forestal} (forestry company).}
  }
}
\source{
\enc{Zamorano-Elgueta, C., Cayuela, L.,  González-Espinosa, C., Lara, A. and  Parra-Vázquez, M.R. 2012. Impacts of cattle on the South American temperate forests: Challenges for the conservation of the endangered monkey puzzle tree (\emph{Araucaria araucana}) in Chile.  \emph{Biological Conservation} 152: 110-118.}{Zamorano-Elgueta, C., Cayuela, L.,  Gonzalez-Espinosa, C., Lara, A. and  Parra-Vazquez, M.R. 2012. Impacts of cattle on the South American temperate forests: Challenges for the conservation of the endangered monkey puzzle tree (\emph{Araucaria araucana}) in Chile.  \emph{Biological Conservation} 152: 110-118.}
}
\examples{
data(ara)
str(ara) 
}
\keyword{datasets}
