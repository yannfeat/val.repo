\name{ahptopsis2n}
\alias{ahptopsis2n}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
AHP-TOPSIS-2N method
}
\description{
AHP-TOPSIS-2N is a hybrid multi-criteria decision-making method build from the AHP (Analytic Hierarchy Process) and TOPSIS-2N (Technique for Order of Preference by Similarity to Ideal Solution - with two normalizations).
}
\usage{
ahptopsis2n(decision, criteria, minmax)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{decision}{
a matrix where rows correspond to the alternatives and columns correspond to criteria.
}
  \item{criteria}{
a matrix with pairwise comparison of criteria as in Analytic Hierarchy Process method.
}
  \item{minmax}{
a vector with objectives, minimize or maximize, to each criteria.
}
}
\details{
criteria must be in the same order on the arguments.
}
\value{
a list with consistency ratio and two dataframes with priority sorting of the alternatives.
}
\references{

Souza, L. P. de, Gomes, C. F. S. and De Barros, A. P. (2018). Implementation of New Hybrid AHP–TOPSIS-2N Method in Sorting and Prioritizing of an it CAPEX Project Portfolio. International Journal of Information Technology & Decision Making. DOI: 10.1142/S0219622018500207.

}

\author{Raquel Coutinho \email{rdouradocoutinho@gmail.com},
        Marcos dos Santos \email{marcosdossantos@ime.eb.br}}


\examples{


decision<-matrix(c(64, 48, 1400,
                   128,64, 1900,
                   64, 32, 1100), ncol=3, byrow=TRUE)

rownames(decision)<- c("A1", "A2", "A3")


criteria<-matrix(c(1, 3, 1/3,
                   1/3, 1, 1/5,
                   3, 5, 1), ncol=3, byrow=TRUE)

minmax<-c("max", "max", "min")

ahptopsis2n(decision=decision, criteria=criteria, minmax=minmax)

}


