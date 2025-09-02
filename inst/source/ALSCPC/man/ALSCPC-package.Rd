\name{ALSCPC-package}
\alias{ALSCPC-package}
\title{Accelerated line search algorithm for simultaneous orthogonal transformation of several positive definite symmetric  matrices to nearly diagonal form.}
\description{
Let
\deqn{
\Phi (\bold{D}) = {\sum\limits_{i = 1}^G  {{n_i}} \ log{\left[ {{{\det (diag(\bold{D}'{\bold{S}_i}\bold{D}))}}} \right]}}
                 -{\sum\limits_{i = 1}^G  {{n_i}} \ log{\left[ {{{\det (\bold{D}'{\bold{S}_i}\bold{D})}}} \right]}}
}
where \eqn{G}  is a positive integer and called as the number of groups, 
\eqn{{n_1}+1}, \eqn{{n_2}+1}, \dots, \eqn{{n_G}+1} are  positive integers and called as the sample sizes,
\eqn{\bold{D}} is an orthonormal matrix, and
\eqn{\bold{S}_1}, \eqn{\bold{S}_2}, \dots, \eqn{\bold{S}_G} are positive-definite and are usually sample covariance matrices.
The minimization of the objective function \eqn{\Phi (\bold{D})\ge 0} that depends on
a orthonormal matrix \eqn{\bold{D}}  is required for a potpourri of statistical problems. 
\eqn{\Phi (\bold{D})= 0} means that \eqn{\bold{S}_1}, \eqn{\bold{S}_2}, \dots, \eqn{\bold{S}_G} are simultaneously simultaneously diagonalizable.
This situation is encountered when looking for common principal components, for example, and the
Flury and Gautschi (1986) method is a popular approach.
Lefkomtch (2004), Boik (2007), and Browne and McNicholas (2012) report that the Flury and Gautschi method is not effective for higher dimensional problems.
Browne and  McNicholas (2013) 
obtain several simple majorization-minizmation (MM) algorithms that provide solutions
to this problem and are effective in higher dimensions. They  compare these solutions
with each others in terms of convergence and computational time.
They found that the accelerated line search (ALS) algorithm  is a computationally efficient procedure  to this problem.
Extensive review of the this algorithm and similar algorithms can be found in Absil et al. (2008).
In the following, we briefly describe the ALS algorithm  used to minimize the objective function
\eqn{\Phi (\bold{D})}. 
ALS algorithm  is based on the update formula
\deqn{\bold{D_{k+1}} = R_{\bold{D_k}} (-\beta^{m_k} \ \alpha  \ grad(\Phi (\bold{D}_k)))} 
where
\eqn{R_{\bold{D_k}}(\bold{V}) = qf({\bold{D_k}} + \bold{V})}, where \eqn{qf(\bold{M}) = \bold{Q}} in the sense of the
QR decomposition of a matrix \eqn{\bold{M}}; 
The \eqn{\bold{Q}\bold{R}} decomposition of a matrix
\eqn{\bold{M}} is the decomposition of \eqn{\bold{M} } as \eqn{\bold{M}=\bold{Q}\bold{R}}, where  \eqn{\bold{Q}} belongs  to the orthogonal group and
\eqn{\bold{R}} belongs to  the set of all upper triangular matrices with strictly
positive diagonal elements, 
\deqn{
grad(\Phi(\bold{D}_k))=\overline{grad}(\Phi(\bold{D}_k))
-\bold{D}_k\left[\frac{\bold{D}'_k\ \overline{grad}(\Phi(\bold{D}_k))+\overline{grad}(\Phi(\bold{D}_k))' \bold{D}_k  }{2}\right] 
}
where
\deqn{\overline{grad}(\Phi(\bold{D}_k))=
{\sum\limits_{i = 1}^G  2{{n_i}}{\bold{S}'_i}{\bold{D}_k}
{\left[ {{{diag(\bold{D}'_k{\bold{S}_i}\bold{D}_k)}}} \right]^{-1}}},  
}
and for \eqn{\beta, \sigma \in (0,1)} and \eqn{\alpha>0}, \eqn{m_k} 
is the smallest nonnegative integer \eqn{m} such that
\deqn{
\Phi(\bold{D}_k)-\Phi (\bold{D}_{k+1}) \ge -\sigma <grad(\Phi(\bold{D}_k)) \ , \ -\beta^{m} \ \alpha \ grad(\Phi(\bold{D}_k))>
}
where \eqn{< . \ , \ . >} is the Frobenius inner product.
Starting from initial iterate \eqn{\bold{D}_0}, for a given \eqn{\epsilon > 0},  
we stop the algorithm when
\deqn{|\Phi(\bold{D}_k)-\Phi (\bold{D}_{k+1})| \le \epsilon.}

}



\details{
\tabular{ll}{
Package: \tab ALSCPC\cr
Version: \tab 1.0\cr
Date: \tab 2013-09-05\cr
License: \tab GPL (>= 2)\cr
} 
}
\author{
Dariush Najarzadeh

Maintainer: Dariush Najarzadeh <D_Najarzadeh@sbu.ac.ir>
}
\references{
Absil, P. A., Mahony, R., & Sepulchre, R. (2009). Optimization algorithms on matrix manifolds. Princeton University Press.

Boik, R. J. (2007). Spectral models for covariance matrics. Biometrika, 89, 159-182.

Browne, R. P., and McNicholas, P. D. (2012). Orthogonal Stiefel manifold optimization for eigen-decomposed covariance parameter estimation in mixture models. 
Statistics and Computing, 1-8.

Browne, R. P., and  McNicholas, P. D. (2013). Estimating common principal components in high dimensions. arXiv preprint arXiv:1302.2102.

Flury, B. N., and Gautschi, W. (1986). An algorithm for simultaneous orthogonal transformation
 of several positive definite symmetric matrices to nearly diagonal form. 
SIAM Journal on Scientific and Statistical Computing, 7(1), 169-184.

Lefkomtch, L. P. (2004). Consensus principal components. Biometrical Journal, 35, 567-580.
}


\keyword{Accelerated line search algorithm }
\keyword{Common principal components}


