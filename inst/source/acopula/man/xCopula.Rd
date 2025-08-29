\name{xCopula}

\alias{cCopula}
\alias{dCopula}
\alias{eCopula}
\alias{gCopula}
\alias{pCopula}
\alias{qCopula}
\alias{rCopula}
\alias{isCopula}

\alias{eCopulaArchimax}
\alias{eCopulaGeneric}
\alias{gCopulaEmpirical}
\alias{pCopulaEmpirical}
\alias{rCopulaArchimax2D}

\alias{print.eCopulaArchimax}
\alias{print.eCopulaGeneric}
\alias{print.gCopula}
\alias{print.isCopula}

\title{
Archimax and generic copula distribution functions
}
\description{
Copula cumulative distribution function ('p'), probability density function ('d'), conditional probability cdf (c), quantile (q), random vector sampling ('r'), parameters estimation ('e'), goodness-of-fit test ('g'), checking copula properties ('is').
}
\usage{
pCopula(data, generator=genGumbel(), depfun=dep1(), copula=NULL, 
  gpars=generator$parameters, dpars=depfun$parameters, 
  pars=if(is.null(copula)) list(gpars,dpars) else copula$parameters,
  subdivisions=50, quantile=NULL,probability=data[,quantile])
  
pCopulaEmpirical(data, base = data)

dCopula(data,generator=genGumbel(),depfun=dep1(),copula=NULL,
  gpars=generator$parameters, dpars=depfun$parameters,
  pars=if(is.null(copula)) list(gpars,dpars) else copula$parameters,
  difference=1e-4,area=c(0), shrinkdiff=FALSE)

cCopula(data, conditional.on=c(1), generator=genGumbel(), depfun=dep1(), copula=NULL, 
  gpars=generator$parameters, dpars=depfun$parameters, 
  pars=if(is.null(copula)) list(gpars,dpars) else copula$parameters, 
  difference=1e-4,area=c(0), quantile=NULL, probability=data[,quantile])

qCopula(data, quantile=1, probability=0.95, conditional.on=NULL,
  generator=genGumbel(), depfun=dep1(), copula=NULL, 
  gpars=generator$parameters, dpars=depfun$parameters, 
  pars=if(is.null(copula)) list(gpars,dpars) else copula$parameters, 
  difference=1e-4, area=c(0))

rCopula(n, dim=2, generator=genGumbel(), depfun=dep1(), copula=NULL, 
  gpars=generator$parameters, dpars=depfun$parameters,
  pars=if(is.null(copula)) list(gpars,dpars) else copula$parameters)
                    
rCopulaArchimax2D(n, generator=genLog(), depfun=dep1(), 
  gpars=generator$parameters, dpars=depfun$parameters, pars=list(gpars,dpars))

eCopula(data, generator=genGumbel(), depfun=dep1(), copula=NULL,
  glimits=list(generator$lower,generator$upper), dlimits=list(depfun$lower,depfun$upper),
  limits=list(copula$lower,copula$upper),
  ggridparameters=if(!is.null(unlist(glimits))) do.call(
    function(...) mapply(c,...,length.out=pgrid,SIMPLIFY=FALSE), glimits) else NULL, 
  dgridparameters=if(!is.null(unlist(dlimits))) do.call(
    function(...) mapply(c,...,length.out=pgrid,SIMPLIFY=FALSE), dlimits) else NULL, 
  gridparameters=if(!is.null(unlist(limits))) do.call(function(...) 
    mapply(c,...,length.out=pgrid,SIMPLIFY=FALSE), limits) else NULL,
  technique=c("ML","LS","icorr"), procedure=c("optim","nlminb","nls","grid"), 
  method="default", corrtype = c("kendall","spearman"), control=NULL, pgrid=10)

gCopula(data, generator, depfun=dep1(), copula=NULL,
  glimits=list(generator$lower,generator$upper), 
  dlimits=list(depfun$lower,depfun$upper),
  limits=list(copula$lower,copula$upper),
  etechnique=c("ML","LS","icorr"), eprocedure=c("optim","nlminb","nls"), 
  emethod="default", ecorrtype=c("kendall","spearman"), econtrol=NULL,  
  N=100, m=nrow(data), ncores=1)

gCopulaEmpirical(data,N=100,ncores=1)

isCopula(generator=genLog(),depfun=dep1(), copula=NULL,
  glimits=list(generator$lower,generator$upper), 
  dlimits=list(depfun$lower,depfun$upper),
  limits=list(copula$lower,copula$upper),  
  ggridparameters=if(!is.null(unlist(glimits))) do.call(
    function(...) mapply(c,...,length.out=pgrid,SIMPLIFY=FALSE), glimits) else NULL,
  dgridparameters=if(!is.null(unlist(dlimits))) do.call(
    function(...) mapply(c,...,length.out=pgrid,SIMPLIFY=FALSE), dlimits) else NULL, 
  gridparameters=if(!is.null(unlist(limits))) do.call(
    function(...) mapply(c,...,length.out=pgrid,SIMPLIFY=FALSE), limits) else NULL,
  dagrid=10, pgrid=10, dim=3, tolerance=1e-15)


\method{print}{eCopulaArchimax}(x,...)
\method{print}{eCopulaGeneric}(x,...)
\method{print}{gCopula}(x,...)
\method{print}{isCopula}(x,...)
}

\arguments{
  \item{data}{
vector, matrix or data frame with as many columns as variables. List of two such objects in case of \code{gCopulaEmpirical}.
}
  \item{generator}{
list containing archimedean generator, it's inverse, parameter bounds and possibly derivatives. See \code{\link{generator}}.
}
  \item{depfun}{
list containing Pickand's dependence function, parameter bounds and possibly derivatives. See \code{\link{depfun}}.
}
  \item{copula}{
list containing generic copula CDF and/or PDF, parameter values and bounds. See \code{\link{copula}}.
}
  \item{pars}{
either numeric vector of generic copula parameters, or list of two vectors: generator parameters and dependence function parameters.
}
  \item{gpars}{
numeric vector of \code{generator} parameters, \code{NULL} in trivial case.
}
  \item{dpars}{
numeric vector of \code{depfun} parameters, \code{NULL} in trivial case.
}
  \item{quantile}{
numeric. Index of the quantile variable among other variables; if \code{NULL} (default) then no quantile is computed.
}
  \item{probability}{
numeric. Probability corresponding to the wanted quantile. Replicated to length of the data. By default \code{quantile}-th column of \code{data}.
}
  \item{subdivisions}{
integer. Parameter passed to \code{\link{nintegrate}}.
}
  \item{difference}{
parameter passed to \code{\link{nderive}}.
}
  \item{base}{
data.frame or matrix. Data set from which an empirical copula is constructed.
}
  \item{area}{
parameter passed to \code{\link{nderive}}.
}
  \item{shrinkdiff}{
logical. Whether to shrink \code{difference} if [0,1] interval is exceeded. By default, \code{area} is changed instead.
}
  \item{conditional.on}{
numeric. Index of variables to be conditioned on.
}
  \item{n}{
number of random observations to be generated.
}
  \item{dim}{
number of dimensions of copula = number of variables.
}
  \item{glimits, dlimits, limits}{
list of two vectors: lower and upper bound of \code{generator}, \code{depfun} and generic copula parameters, respectively.
}
  \item{technique, etechnique}{
copula parameters estimation method: Maximum pseudo-likelihood \code{"ML"}, Least square distance to empirical copula \code{"LS"} and Inversion of correlation coefficient relation to copula parameter \code{"icorr"}.
}
  \item{procedure, eprocedure}{
R optimization routine to estimate parameters (\code{"optim"}, \code{"nlminb"}, \code{"nls"}) or "brute force" search over parameter grid (\code{"grid"}). The last one is useful when the other methods give unsatisfactory results. \code{"nls"} cannot be used with ML technique. Ignored with "icorr" technique.
}
  \item{method, emethod}{
optimisation algorithm used by \code{optim} procedure.
}
  \item{corrtype,ecorrtype}{
character. Correlation coeficient used by \code{icorr} technique, either "kendall" and "spearman".
}
  \item{control,econtrol}{
list of control settings passed to optimization routines.
}
  \item{ggridparameters, dgridparameters, gridparameters}{
list of parameters values to create a grid from; any list item can be vector of elements named to match \code{seq()} arguments; by default the sequence is constructed from \code{glimits},\code{dlimits},\code{limits}, respectively, and \code{pgrid}.
}
  \item{pgrid}{
number of grid points in each dimension of parameters space. Used when \code{gparameters, dparameters} are not supplied.
}
  \item{N}{
number of bootstrap cycles.
}
  \item{m}{
number of Monte Carlo cyles needed in approximation of parametric function if there is no analytical expression available.
}
  \item{ncores}{
number of cores to be used for p-value simulation. Parallelization requires package \code{\link{parallel}} or \code{multicore} and may not work on Windows OS.
}
  \item{dagrid}{
integer. Number of data grid nodes used to check copula properties in.
}
  \item{tolerance}{
numeric. How much to tolerate departure of numeric results from theoretical values/limits.
}
  \item{x}{
an object used to select a method.
}
  \item{...}{
further arguments passed to or from other methods.
}

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Numeric vector, in case of \code{p},\code{d},\code{c} and \code{qCopula}.

Matrix from \code{rCopula}.

\code{eCopula} returns list containing
 \item{parameters}{list of numeric vectors. Generator, depfu or generic copula parameters.}
 \item{approach}{character vector of length 3 describing estimation and optimization method.}
 \item{fvalue}{numeric. Value of the optimized function.}
 \item{procedure.output}{list. Full outcome of an optimization function.}

\code{gCopula} and \code{gCopulaEmpirical} return list containing
 \item{statistic}{numeric. GOF test statistic.}
 \item{q95}{numeric. 95\% quantile or critical value from bootstrap simulations.}
 \item{p.value}{numeric. p-value from bootstrap simulations.}
 \item{estimate}{numeric vector. Copula parameters estimates.}
 \item{data.name}{character. Name of the supplied data object.}
 \item{method}{character. Identification of GoF test used.}
 \item{fitting_method}{character vector of length 3. Sumarizes estimation and optimiztion method.}
 \item{copula_id}{character. Generator and depfu id, or copula id.}

\code{isCopula} returns list containing
 \item{is.copula}{logical. FALSE if any of copula properties is violated.}
 \item{issues}{data frame. Consists of 1) index of variable, 2) violated property, 3) deviation from allowed range, 4) copula parameters for which the issue emerged.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Genest, C., \enc{Rémillard}{Remillard}, B. and Beaudoin, D. (2009): Goodness-of-fit tests for copulas: A review and a power study. \emph{Insurance: Mathematics and Economics} 44, 199--213.

\enc{Rémillard}{Remillard}, B., Scaillet, O. (2009): Testing for equality between two copulas. \emph{Journal of Multivariate Analysis} 100(3), 377--386.
}
\author{
Tomas Bacigal
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{generator}}, \code{\link{depfun}}, \code{\link{pCopula}}
}
\examples{
## assign generator definition list with specific parameter
ge <- genGumbel(parameters=4)

## probability P(U<0.3,V<0.5)
pCopula(c(0.3,0.5),ge)  #0.2906142
## quantile q for which P(U<q,V<0.5)=0.2906142
pCopula(c(0.2906142,0.5),ge,quantile=1)  #0.3000175
pCopula(c(NA,0.5),ge,quantile=1,probability=0.2906142)
qCopula(c(0.5),quantile=1,probability=0.2906142,generator=ge)

## conditional probability P(U<0.3|V=0.5)
cCopula(c(0.3,0.5),ge,conditional.on=2)  #0.1025705
## quantile q for which conditional probability P(U<q|V=0.5)=0.1025705
cCopula(c(0.1025705,0.5),conditional.on=2,generator=ge,quantile=1)  #0.2999861
cCopula(c(NA,0.5),conditional.on=2,generator=ge,quantile=1,probability=0.1025705)
qCopula(c(0.5),quantile=1,probability=0.1025705,conditional.on=2,generator=ge)

## copula density
dCopula(c(0.3,0.5),ge) #1.083797
local({
x <- y <- seq(0,1,length.out=20)
persp(x,y,matrix(dCopula(expand.grid(x,y),ge),nrow=length(x)),r=2,zlab="density")
})

## simulate random vector
rge <- rCopula(100,dim=2,ge) 
plot(rge)
# Observe that using rCopula(100,dim=2,cop=copGumbel(parameters=4)) 
# would take much more time to sample, since numerical derivative needs to be employed. 

## --- fit copula to data set
# maximum likelihood (using density)
eCopula(rge,ge,technique="ML")
# some methods has no support for parameters bounds (do not mind a warning message)
eCopula(rge,ge,technique="ML",method="BFGS")  
# least-square fit to empirical copula
eCopula(rge,ge,technique="LS",procedure="nlminb")  
# maximizing discretized likelihood function
eCopula(rge,ge,technique="ML",procedure="grid",glimits=list(2.,6.),pgrid=20)  
# specify nodes of the grid
eCopula(rge,ge,tech="ML",proc="grid",ggridparameters=list(c(2.,6.,length.out=20))) 
# without naming, it won't create sequence
eCopula(rge,ge,technique="ML",procedure="grid",ggridparameters=list(c(2.,6.,20)))
# inversion of Kendall's tau
eCopula(rge,ge,technique="icorr",corrtype="kendall")

## --- GoF test, set substantially higher N to increase precision of p-value
gCopula(rge,ge,etechnique="ML",N=5)
# parallel computing takes lesser time, but the progress is not displayed
# not available on Windows OS
if(.Platform$OS.type!="windows") {
  gCopula(rge,ge,etechnique="ML",N=5,ncores=2)
}

## testing if two data sets has equal copulas
rge1 <- rCopula(80,dim=2,genClayton(),gpars=3)
gCopula(list(rge,rge1),N=5)

## check whether some hypotheticaly-copula function does not violate  
## copula properties (over data and parameters grid)
isCopula(genGumbel(),dagrid=10,pgrid=10,tolerance=1e-15)

## all the above functions are ready for archimax or generic copulas 
## as well as for higher dimensions
pCopula(c(0.3,0.5,1.0),genClayton(),depGumbel(),gpars=0.01,dpars=4.)  #0.2907613
pCopula(c(0.3,0.5,1.0),copula=copGumbel(),pars=4.)  #0.2906142

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cumulative distribution function }
\keyword{ probability density function }
\keyword{ conditional probability }
\keyword{ quantile }
\keyword{ sampling }
\keyword{ empirical copula }
\keyword{ goodness-of-fit test }
\keyword{ d-increasing }
\keyword{ maximum likelihood }
\keyword{ inverse of correlation coefficient }
