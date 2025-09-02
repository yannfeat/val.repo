\name{ALS.CPC}
\alias{ALS.CPC}

\title{minimize the objective function \eqn{\Phi(\bold{D})} by using of the accelerated line search algorithm}

\description{
The \code{ALS.CPC} function implement 
ALS algorithm based on the update formula
\deqn{\bold{D_{k+1}} = R_{\bold{D_k}} (-\beta^{m_k} \ \alpha  \ grad(\Phi (\bold{D}_k)))}
until convergence (i.e. \eqn{|\Phi(\bold{D}_k)-\Phi (\bold{D}_{k+1})| \le \epsilon})
and return the orthogonal  matrix 
\eqn{\bold{D}_r}, \eqn{r} is the smallest nonnegative integer \eqn{k} such that
\eqn{|\Phi(\bold{D}_k)-\Phi (\bold{D}_{k+1})| \le \epsilon}.
}
\usage{ALS.CPC(alpha,beta,sigma,epsilon,G,nval,D,S)}
\arguments{
  \item{alpha}{  positive real number.}
  \item{beta}{real number belong to (0,1).}
  \item{sigma}{real number belong to (0,1).}
  \item{epsilon}{small positive constant controlling error term.}
  \item{G}{number of groups in common principal components analysis.}
  \item{nval}{a numeric vector containing the positive integers of sample sizes minus one in each group.}
  \item{D}{an initial  square orthogonal matrix of order \code{p}, where \code{p} is group dimensionality.}
  \item{S}{a list of length \code{G} of positive definite symmetric matrices of order \code{p}.}
}
\value{An orthogonal matrix  such that minimize \eqn{\Phi(\bold{D})}.}

\references{

Absil, P. A., Mahony, R., & Sepulchre, R. (2009). Optimization algorithms on matrix manifolds. Princeton University Press.
}

\author{Dariush Najarzadeh}
\examples{
nval<-numeric(3) 
nval[[1]]<-49
nval[[2]]<-49
nval[[3]]<-49
S<-vector("list",length=3)
setosa<-iris[1:50,1:4]; names(setosa)<-NULL
versicolor<-iris[51:100,1:4]; names(versicolor)<-NULL
virginica<-iris[101:150,1:4]; names(virginica)<-NULL
S[[1]]<-as.matrix(var(versicolor))
S[[2]]<-as.matrix(var(virginica))
S[[3]]<-as.matrix(var(setosa))
D<-diag(4)
ALS.CPC(10,0.5,0.4,1e-5,G=3,nval,D,S)
}
