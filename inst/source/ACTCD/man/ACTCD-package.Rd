\name{ACTCD-package}
\alias{ACTCD}

\docType{package}
\title{
ACTCD: Asymptotic Classification Theory for Cognitive Diagnosis
}

\description{
Cluster analysis for cognitive diagnosis based on the Asymptotic Classification Theory (Chiu, Douglas & Li, 2009). 
}
\details{


Cognitive Diagnosis aims primarily to obtain examinees' mastery or non-mastery on a set of attributes or skills of interest, based on their responses to test items and a pre-specified Q-matrix (Tatsuoka, 1985). The Asymptotic Classification Theory (Chiu, Douglas & Li, 2009) provides mathematical grounds for cognitive diagnosis using cluster analysis. 

Briefly speaking, given the responses of \eqn{N} examinees to a test of \eqn{J} items with \eqn{K} attributes, let \eqn{\bm{W}=(W_{1}, W_{2},\ldots, W_{K})'} be a vector of summed scores on the \eqn{K} attributes, where the \eqn{k_{th}} component is defined as
\deqn{W_{k}=\sum_{j=1}^{J}Y_{j}q_{jk},}
where \eqn{Y_j} is the vector of responses of the \eqn{j^{th}} examinee and \eqn{q_{jk}} is the \eqn{(j,k)} entry of the Q-matrix. The sample statistic \eqn{\bm{W}} is then taken as the input for cluster analysis, such as \emph{K}-means (MacQueen, 1976) and Hierarchical Agglomerative Cluster Analysis (HACA; Hartigan, 1975). This theory indicated that given two different attribute patterns \eqn{\bm{\alpha}} and \eqn{\bm{\alpha}^*}, the corresponding conditional expectations, \eqn{E[\bm{W}|\bm{\alpha}]} and \eqn{E[\bm{W}|\bm{\alpha}^*]}, will be distinct, implying that with \eqn{\bm{W}} as the input, cluster analysis will group subjects correctly as the number of items is sufficiently large. Refer to Chiu, Douglas and Li (2009) for details about this theory. Because cluster analysis does not provide labels for the clusters, various labeling methods (Chiu & Ma, 2013) have been developed to obtain the attribute profiles .

The package \code{\link{ACTCD}} is an easy-to-use tool. The responses matrix and Q-matrix (Tatsuoka, 1985) are required by the main function of this package, \code{\link{npar.CDM}}, and the examinees' attribute profiles can be obtained directly using user-specified clustering and labeling methods. It is also possible to conduct cluster analysis without labeling algorithm using function \code{\link{cd.cluster}} based on HACA or \emph{K}-means. The labeling algorithms can be employed by \code{\link{labeling}} separately.

}
\author{
Chia-Yi Chiu (Rutgers, the State University of New Jersey) and Wenchao Ma (Rutgers, the State University of New Jersey).
  
 Maintainer: Wenchao Ma \email{wenchao.ma@ua.edu}
}
\references{
  Chiu, C. Y., Douglas, J. A., & Li, X. (2009). Cluster analysis for cognitive diagnosis: theory and applications. \emph{Psychometrika, 74}(4), 633-665.

  Chiu, C. Y., & Ma, W. (2013). \emph{Assignment of clusters to attribute profiles for cognitive diagnosis}. Manuscript in preparation.
  
  Hatigan, J. A. (1975). \emph{Clustering algorithms.} New York: Wiley.
  
  MacQueen, J. (1967). Some methods of classification and analysis of multiariate observations. In L. M. Le Cam & J. Neyman (Eds.), \emph{Proceedings of the fifth Bekeley Symposium on Mathematical Statistics and Probability} (pp.281-307).Berkeley: University of California Press.
  
  Tatsuoka, K. (1985). A probabilistic model for diagnosing misconceptions in the pattern classification approach. \emph{Journal of Educational Statistics, 12}, 55-73.
  }

\keyword{ package }

