#Functions from the trawl package

#'Evaluates the exponential trawl function
#'@name trawl_Exp
#'@param x the argument at which the exponential trawl function will be
#'  evaluated
#'@param lambda the parameter \eqn{\lambda} in the exponential trawl
#'@return The exponential trawl function evaluated at x
#'@details The trawl function is parametrised by parameter \eqn{\lambda > 0} as
#'  follows: \deqn{g(x) = e^{\lambda x},  \mbox{ for }  x \le 0.}
#'@examples
#'trawl_Exp(-1,0.5)
#'@export
trawl_Exp <- function(x,lambda){exp(x*lambda)}




#'Evaluates the supIG trawl function
#'@name trawl_supIG
#'@param x the argument at which the supIG trawl function will be evaluated
#'@param delta the parameter \eqn{\delta} in the supIG trawl
#'@param gamma the parameter \eqn{\gamma} in the supIG trawl
#'@return The supIG trawl function evaluated at x
#'@details The trawl function is parametrised by the two parameters \eqn{\delta
#'  \geq 0} and \eqn{\gamma \geq 0} as follows: \deqn{gd(x) =
#'  (1-2x\gamma^{-2})^{-1/2}\exp(\delta \gamma(1-(1-2x\gamma^{-2})^{1/2})),
#'  \mbox{ for } x \le 0.} It is assumed that \eqn{\delta} and \eqn{\gamma} are
#'  not simultaneously equal to zero.
#'@examples
#'trawl_supIG(-1,0.5,0.2)
#'@export
trawl_supIG <-function(x,delta,gamma){
  r<-(1-2*x/gamma^2)^(-1/2)*exp(delta*gamma*(1-(1-2*x/gamma^2)^(1/2)))
  return(r)
}

#'Evaluates the long memory trawl function
#'@name trawl_LM
#'@param x the argument at which the supOU/long memory trawl function will be
#'  evaluated
#'@param alpha the parameter \eqn{\alpha} in the long memory trawl
#'@param H the parameter \eqn{H} in the long memory trawl
#'@return the long memory trawl function evaluated at x
#'@details The trawl function is parametrised by the two parameters \eqn{H> 1}
#'  and \eqn{\alpha > 0} as follows: \deqn{g(x) = (1-x/\alpha)^{-H},
#'  \mbox{ for  }  x \le 0.} If \eqn{H \in (1,2]}, then the resulting trawl
#'  process has long memory, for \eqn{H>2}, it has short memory.
#'@examples
#'trawl_LM(-1,0.5, 1.5)
#'@export
trawl_LM <- function(x,alpha, H){(1-x/alpha)^(-H)}


#'Autocorrelation function of the exponential trawl function
#'
#'This function computes the autocorrelation function associated with the
#'exponential trawl function.
#'@name acf_Exp
#'@param x The argument (lag) at which the autocorrelation function associated
#'  with the exponential trawl function will be evaluated
#'@param lambda parameter in the exponential trawl
#'@return The autocorrelation function of the exponential trawl function
#'  evaluated at x
#'@details The trawl function is parametrised by the parameter \eqn{\lambda > 0}
#'  as follows:
#'\deqn{g(x) = e^{\lambda x},  \mbox{ for } x \le 0.}
#'Its autocorrelation function is given by:
#'\deqn{r(x) = e^{-\lambda x},  \mbox{ for } x \ge 0.}
#'@examples
#'acf_Exp(1,0.1)
#'@export
acf_Exp <- function(x,lambda){exp(-lambda*x)}



#'Autocorrelation function of the supIG trawl function
#'
#'This function computes the autocorrelation function associated with the supIG
#'trawl function.
#'@name acf_supIG
#'@param x The argument (lag) at which the autocorrelation function associated
#'  with the supIG trawl function will be evaluated
#'@param delta parameter in the supIG trawl
#'@param gamma parameter in the supIG trawl
#'@return The autocorrelation function of the supIG trawl function evaluated at
#'  x
#'@details The trawl function is parametrised by the two parameters \eqn{\delta
#'  \geq 0} and \eqn{\gamma \geq 0} as follows: \deqn{g(x) =
#'  (1-2x\gamma^{-2})^{-1/2}\exp(\delta \gamma(1-(1-2x\gamma^{-2})^{1/2})),
#'  \mbox{ for } x \le 0.} It is assumed that \eqn{\delta} and \eqn{\gamma} are
#'  not simultaneously equal to zero. Its autocorrelation function is given by:
#'  \deqn{r(x) = \exp(\delta\gamma (1-\sqrt{1+2 x/\gamma^2})),  \mbox{ for } x
#'  \ge 0.}
#'@examples
#'acf_supIG(1,0.3,0.1)
#'@export
acf_supIG <- function(x,delta,gamma){exp(delta*gamma*(1-sqrt(1+2*x/gamma^2)))}

#'Autocorrelation function of the long memory trawl function
#'
#'This function computes the autocorrelation function associated with the long
#'memory trawl function.
#'@name acf_LM
#'@param x The argument (lag) at which the autocorrelation function associated
#'  with the long memory trawl function will be evaluated
#'@param alpha parameter in the long memory trawl
#'@param H parameter in the long memory trawl
#'@return The autocorrelation function of the long memory trawl function
#'  evaluated at x
#'@details The trawl function is parametrised by the two parameters \eqn{H> 1}
#'  and \eqn{\alpha > 0} as follows: \deqn{g(x) = (1-x/\alpha)^{-H},
#'   \mbox{ for  }  x \le 0.} Its autocorrelation function is given by
#'  \deqn{r(x)=(1+x/\alpha)^{(1-H)}, \mbox{ for } x \ge 0.}
#'@examples
#'acf_LM(1,0.3,1.5)
#'@export
acf_LM <- function(x,alpha, H){(1+x/alpha)^(1-H)}



