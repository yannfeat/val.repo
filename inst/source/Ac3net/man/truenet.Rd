\name{truenet}
\alias{truenet}

\title{
The known intreactions of E.coli from RegulonDB database.
}
\description{
This true interactions of E.coli from RegulonDB database. 
}
\usage{data("truenet")}

\details{
These true interactions are mainly from  RegulonDB database.
We compiled the one from (Faith, 2007), and the RegulonDB database (TF-TF and TF-gene).We removed the links where the gene names is not evailable in our example   E.coli dataset. See the references for the usage information of the data.
}
\source{
This data was downloaded from http://regulondb.ccg.unam.mx/menu/download/datasets/index.jsp and  m3d.mssm.edu/network_inference.html .
}
\references{
Gama-Castro S, et.al., RegulonDB version 9.0: high-level integration of gene regulation, coexpression, motif clustering and beyond., Nucleic Acids Res. 2016 Jan 4;44(D1):D133-43.

Faith JJ, Hayete B, Thaden JT, Mogno I, Wierzbowski J, Cottarel G, Kasif S, Collins JJ, Gardner TS. Large-scale mapping and validation of Escherichia coli transcriptional regulation from a compendium of expression profiles. PLoS Biol. 2007 Jan;5(1):e8
}
\examples{

data(truenet)

}
\keyword{datasets}
