\name{zeroindx}
\alias{zeroindx}

\title{
The indices to force to zero correlations.
}
\description{
The indices to force to zero correlations of E.coli expresseion data. Because REgulonDB database provided only TF-TF or TF-gene interactions. We need to remove gene-gene interactions before the performance analysis.
}
\usage{data("zeroindx")}

\details{
The indices to force to zero correlations of E.coli expresseion data. Because REgulonDB database provided only TF-TF or TF-gene interactions. We need to remove gene-gene interactions before the performance analysis.
These indice are also from (Faith, 2007),  available along with the expression data.
}
\source{
This data was downloaded from m3d.mssm.edu/network_inference.html .
}
\references{
Faith JJ, Hayete B, Thaden JT, Mogno I, Wierzbowski J, Cottarel G, Kasif S, Collins JJ, Gardner TS. Large-scale mapping and validation of Escherichia coli transcriptional regulation from a compendium of expression profiles. PLoS Biol. 2007 Jan;5(1):e8
}
\examples{

#data(zeroindx)

}
\keyword{datasets}
