
#' @title ai
#' @description Calculate Agreement Interval of Two Measurement Methods and quantify the agreement
#' @author Jialin Xu, Jason Liao
#' @export
#' @rdname ai
#' @param x A continous numeric vector from measurement method 1
#' @param y A continous numeric vector from measurement method 2, the same length as x.
#' @param lambda Reliability ratio of x vs y. default 1.
#' @param alpha Discordance rate to estimate confidence interval
#' @param clin.limit Clinically meaningful limit (optional)
#' @return Function ai returns an object of class "ai".
#'
#' An object of class "ai" is a list containing the following components:
#'
#' alpha:   Alpha input for confidence interval estimates
#'
#' n: Sample size
#'
#' conf.level: Confidence level calculated from alpha
#'
#' lambda: Reliability ratio input of x vs y
#'
#' summaryStat: Summary statistics of input data
#'
#' sigma.e: Random error estimates
#'
#' indexEst: Agreement estimates (CI.) based on index approaches
#'
#' intervalEst: Agreement estimates (CI.) based on interval approaches
#'
#' biasEst: Bias estimate
#'
#' intercept: Intercept of linear regression line from measure error model
#'
#' slope: Slope of linear regression line from measure error model
#'
#' x.name: x variable name extracted from input, used for plotting
#'
#' y.name: y variable name extracted from input, used for plotting
#'
#' tolProb.cl: Tolrance probability calculated based on optional clinically meaningful limit
#'
#' k.cl: Number of discordance pairs based on optional clinically meaningful limit
#'
#' alpha.cl: Discordance rate based on clinically meaningful limit
#'
#' @details This is the function to calculate agreement interval (confidence interval) of two continuous numerical vectors from two measurement methods on the same samples. Note that this function only works for scenario with two evaluators, for example, comparing the concordance between two evaluators. We are working on the scenario with more than two evaluators.
#' The two numerical vectors are \code{x} and \code{y}. It also provides commonly used measures based on index approaches,
#' for example, Pearson's correlation coefficient, the intraclass correlation coefficient (ICC),
#' the concordance correlation coefficient (Lin's CCC), and improved CCC (Liao's ICCC).
#'
#' @examples
#' ai(x=1:4, y=c(1, 1, 2, 4))
#' a <- c(1, 2, 3, 4, 7)
#' b <- c(1, 3, 2, 5, 3)
#' ai(x=a, y=b)
#' ai(x=IPIA$Tomography, y=IPIA$Urography)
#' ai(x=IPIA$Tomography, y=IPIA$Urography, clin.limit=c(-15, 15))
#' @importFrom stats aov cor pt qnorm qt var
#' @importFrom psych ICC
#' @references Luiz RR, Costa AJL, Kale PL, Werneck GL. Assessment of agreement of a quantitative variable: a new graphical approach. J Clin Epidemiol 2003; 56:963-7.
#' @references Jason J. Z. Liao, Quantifying an Agreement Study, Int. J. Biostat. 2015; 11(1): 125-133
#' @references Shrout, Patrick E. and Fleiss, Joseph L. Intraclass correlations: uses in assessing rater reliability. Psychological Bulletin, 1979, 86, 420-3428.
#' @references Lin L-K., A Concordance Correlation Coefficient to Evaluate Reproducibility. Biometrics 1989; 45:255-68
#' @references Liao JJ. An Improved Concordance Correlation Coefficient. Pharm Stat 2003; 2:253-61
#' @references Nicole Jill-Marie Blackman, Reproducibility of Clinical Data I: Continuous Outcomes, Pharm Stat 2004; 3:99-108
#'
# This is the main function to calculate agreement interval and create "ai" object
# based on continuous numeric vector \code {x} and y of the same length.
#
# Author: Jialin Xu, jxx120@gmail.com
#
# Reference: Jason J. Z. Liao, Quantifying an Agreement Study, Int. J. Biostat. 2015; 11(1): 125-133

ai<- function(x,y,lambda=1, alpha=0.05, clin.limit=NA)
{
  #
  # x and y are two observations with error
  ## lambda is the reliability ratio of x vs y, usually it is 1
  # the approach is from JL 2011 paper
  if (sum(is.na(clin.limit))==0 & (length(clin.limit)!=2 | !is.numeric(clin.limit)))
    stop("Error: clin.limit has to be numeric vector of length 2")

  x.name=deparse(substitute(x))
  y.name=deparse(substitute(y))

  if (grepl(x=x.name, pattern="\\$")){
    x.name=strsplit(x=x.name, split="\\$")[[1]][2]
  }
  if (grepl(x=y.name, pattern="\\$")){
    y.name=strsplit(x=y.name, split="\\$")[[1]][2]
  }

  # first, use error in variable model to estimate the parameters
  # formulas are from Casella and Berger book

  ## Check the class and length of x, y
  if (!is.numeric(x) | !is.vector(x) |
      !is.numeric(y) | !is.vector(y))
    stop("Error: Please enter x and y as numeric vectors")
  if (length(x) != length(y) | length(x)<3)
    stop("Error: Please enter paired data between x and y with the same length (at least 3) ")
  if (length(lambda)>1 | lambda<=0 | !is.numeric(lambda))
    stop("Error: Please enter reliability ratio between x and y as lambda, a positive value. (default 1)")
  if (length(alpha)>1 | alpha<=0 | alpha>1 | !is.numeric(alpha))
    stop("Error: Please enter discordance rate, a value between 0 and 1, default 0.05")


  n <- length(x)
  x.mean <- mean(x)
  x.std <- sqrt(var(x))

  y.mean <- mean(y)
  y.std <- sqrt(var(y))

  newx <- x - x.mean
  newy <- y - y.mean
  s12 <- sum(newx * newy)
  s11 <- sum(newx * newx)
  s22 <- sum(newy * newy)
  tem <- (s11 - lambda * s22)^2 + 4 * lambda * s12^2
  slope <- ( - (s11 - lambda * s22) + sqrt(tem))/(2 * lambda * s12)
  int <- y.mean - x.mean * slope

  #
  # now construct variance
  #
  sigmab <- ((1 + lambda * slope^2)^2 * (s11 * s22 - s12^2))/tem
  sigmae <- (s22 - slope * s12)/n
  sigmaa <- sigmae/n + sigmab *x.mean^2

  #CI of bias
  ci.fixed <- int + c(-1,1)*qnorm(1-alpha/2)*sqrt(sigmaa)/sqrt(n)
  slope.adj <- slope - 1
  ci.proportion <- slope.adj + c(-1,1)*qnorm(1-alpha/2)*sqrt(sigmab)/sqrt(n)

  #agreement interval
  ai <- c(-1,1)*qt(1-alpha/2,n-1)*sqrt(2*sigmae)
  ai.fixed.adjusted <- int + ai

  # Bland-Altman's limit of agreement
  loa <- mean(y-x) + c(-1,1)*qnorm(1-alpha/2)*sqrt(var(y-x))

  #correlation between x and y
  #Pearson CC
  cor.xy <- cor(x, y)
  # ICC
  #require(psych)
  x1=data.frame(x=x, y=y)
  ICC.ans=ICC(x=x1, alpha=alpha)$results
  cor.icc=unlist( subset(ICC.ans, ICC.ans$type=="ICC1", select="ICC") )
  ci.cor.icc=unlist( subset(ICC.ans, ICC.ans$type=="ICC1", select=c("lower bound", "upper bound")) )

    # Lin's CCC
  cor.lin <- (var(x)+var(y)-var(x-y))/(var(x)+var(y)+mean(x-y)*mean(x-y))
  # new CCC
  cor.new <- cor(x,y)*(4*sqrt(var(x)*var(y))-cor(x,y)*(var(x)+var(y)))/
    ((2-cor(x,y))*(var(x)+var(y))+mean(x-y)*mean(x-y))

  #CI of Pearson's CC
  #z value
  z.xy <- log((1+cor.xy)/(1-cor.xy))/2
  #std of z value
  var.z.xy <- 1/sqrt(n - 3)
  #CI of z value
  ci.z.xy <- z.xy + c(-1,1)*1.96*var.z.xy
  #CI of Pearson's CC
  ci.cor.xy <- (exp(2*ci.z.xy) - 1) / (exp(2*ci.z.xy) + 1)
  #CI of ICC
  #var.icc.xy<-2*(2*n-1)*(1-cor.icc^2)^2/(2*2*n*(n-1))
  # the formula from eqn (13) of Blackman (2004a)
  #var.z.icc<-var.icc.xy/((1-cor.icc^2)^2)
  #ci.cor.zicc<-0.5*log((1+cor.icc)/(1-cor.icc))+c(-1,1)* 1.96*sqrt(var.z.icc)
  #ci.cor.icc<-(exp(2*ci.cor.zicc) - 1) / (exp(2*ci.cor.zicc) + 1)
  # CI of Lin's CCC
  diff12 <- (mean(x) - mean(y)) / sqrt(x.std * y.std)
  var.cc.lin <- (1 - cor.xy^2)*cor.lin^2/((1-cor.lin^2)*cor.xy^2) +
    4*cor.lin^3*(1-cor.lin)*diff12^2/(cor.xy*(1-cor.lin^2)^2) -
    2*cor.lin^4*diff12^4/(cor.xy^2*(1-cor.lin^2)^2)
  # log Lin's CCC
  log.cor.lin <- log((1+cor.lin)/(1-cor.lin))/2
  # CI of log Lin's CCC
  ci.log.cor.lin <- log.cor.lin + c(-1,1)*1.96*sqrt(var.cc.lin / (length(x) - 2))
  # CI of Lin's CCC
  ci.cor.lin <- (exp(2*ci.log.cor.lin) - 1) / (exp(2*ci.log.cor.lin) + 1)

  # CI of Liao's CCC
  s12 <- var(x)
  s22 <- var(y)
  rho <- cor(x, y)
  rc <- cor.new
  diff <- mean(y) - mean(x)
  ratio <- sqrt(s22 / s12)
  up <- 4 * ratio - rho * (1 + ratio^2)
  dow <- (2 - rho) * (1 + ratio^2) + diff^2/s12
  do <- s12*s22*((2 - rho) * (s12 + s22) + diff^2)
  tem <- sqrt(s12*s22)
  # log-scaled new CCC
  z <- 0.5*log((1+rc)/(1-rc))
  # now construct variance
  v1 <- rho*tem*(4*s22-rho*tem)/do-rc^2/s12-rc*s22*(2*s12+0.5*rho*s12+1.5*rho*s22)/do
  v2 <-  rho*tem*(4*s12-rho*tem)/do-rc^2/s22-rc*s12*(2*s22+0.5*rho*s22+1.5*rho*s12)/do
  v3 <- 2*tem*(2*sqrt(s12*s22)-rho*s22-rho*s12)/do+rc*tem*(s12+s22)/do
  v4 <- -rc*2*tem^2*diff/do
  vz <- 2*(s12*v1)^2+4*rho^2*s12*s22*v1*v2+4*rho*tem*s12*v1*v3+2*(s22*v2)^2+4*rho*tem*s22*v2*v3+(1+rho^2)*s12*s22*v3*v3+(s12+s22-rho*tem)*v4*v4
  n <- length(x)
  bound <- qnorm(1 - 0.05/2) * sqrt(vz/n)  #/(1-rc^2) #need to check this out part with ccc lin
  # CI for log-scaled new CCC
  zbound <- z+c(-1,1)*bound
  # CI for Liao's CCC
  ci.new.ccc <- (exp(2*zbound)-1)/(exp(2*zbound)+1)

  ci.cor.xy <- (exp(2*ci.z.xy) - 1) / (exp(2*ci.z.xy) + 1)

  conf.level=round((1-alpha)*100, 3)
  indexEst=data.frame(matrix(NA, 4, 3))
  indexEst[1, ]=c(cor.xy, ci.cor.xy)
  indexEst[2, ]=c(cor.icc, ci.cor.icc)
  indexEst[3, ]=c(cor.lin, ci.cor.lin)
  indexEst[4, ]=c(cor.new, ci.new.ccc)
  colnames(indexEst)=c("Est", paste(conf.level, c("LL", "UL"), sep=""))
  rownames(indexEst)=c("Pearson", "ICC", "Lin.CCC", "Liao.ICCC")

  fixed.bias=data.frame(matrix(c(int, ci.fixed), 1, 3))
  colnames(fixed.bias)=c("Est", paste(conf.level, c("LL", "UL"), sep=""))
  rownames(fixed.bias)="Fixed bias"

  propo.bias=data.frame(matrix(c(slope.adj, ci.proportion), 1, 3))
  colnames(fixed.bias)=c("Est", paste(conf.level, c("LL", "UL"), sep=""))
  rownames(fixed.bias)="Proportional bias at x=1"

  summaryStat=data.frame(matrix(NA, 2, 3))
  summaryStat[1, ]=c(n, x.mean, x.std)
  summaryStat[2, ]=c(n, y.mean, y.std)
  colnames(summaryStat)=c("n", "mean", "sd")
  rownames(summaryStat)=c("x", "y")

  intervalEst=data.frame(matrix(NA, 5, 2))
  intervalEst[1, ]=loa
  intervalEst[2, ]=ai
  intervalEst[3, ]=ai.fixed.adjusted
  intervalEst[4, ]=ai+slope.adj*x.mean
  intervalEst[5, ]=ai.fixed.adjusted+slope.adj*x.mean
  colnames(intervalEst)=paste(conf.level, c("LL", "UL"), sep="")
  rownames(intervalEst)=c("Bland-Altman.LOA", "Liao.AI", "Liao.AI.Adj.Fixed", "Liao.AI.Adj.Propo.xMean", "Liao.AI.Adj.Total.xMean")

  biasEst=data.frame(matrix(NA, 2, 3))
  biasEst[1, ]=fixed.bias
  biasEst[2, ]=propo.bias
  colnames(biasEst)=c("Est", paste(conf.level, c("LL", "UL"), sep=""))
  rownames(biasEst)=c("Fixed.Bias", "Propo.Bias.at.x=1")


  cutoff=intervalEst["Liao.AI", ]
  k=sum( (y-x)<min(cutoff) | (y-x)>max(cutoff) )
  tol.prob=tolProb(n=n, k=k, alpha=alpha)

  if (sum(is.na(clin.limit))==0){
    alpha.cl=agrInt2alpha(clin.limit=clin.limit, n=n, sigmae=sigmae)
    k.cl=sum( (y-x)<min(clin.limit) | (y-x)>max(clin.limit) )
    tol.prob.cl=tolProb(n=n, k=k.cl, alpha=alpha.cl)
  }

  if (sum(is.na(clin.limit))==0){
    result=list(
      x=x, y=y, x.name=x.name, y.name=y.name, alpha=alpha, n=n, conf.level=conf.level, lambda=lambda,
      summaryStat=summaryStat, sigma.e=sigmae,
      indexEst=indexEst,
      intervalEst=intervalEst,
      biasEst=biasEst,
      intercept=int, slope=slope, tolProb=tol.prob, k=k,
      clin.limit=clin.limit,
      tolProb.cl=tol.prob.cl, k.cl=k.cl, alpha.cl=alpha.cl # results triggered by clin.limit
    )
  } else {
    result=list(
      x=x, y=y, x.name=x.name, y.name=y.name, alpha=alpha, n=n, conf.level=conf.level, lambda=lambda,
      summaryStat=summaryStat, sigma.e=sigmae,
      indexEst=indexEst,
      intervalEst=intervalEst,
      biasEst=biasEst,
      intercept=int, slope=slope, tolProb=tol.prob, k=k,
      clin.limit=clin.limit
    )
  }
  class(result)="ai"



  # output the results

  cat(x.name, " and ", y.name, "agree with each other at discordance rate of", alpha, "with tolerance probability of", round(tol.prob, 2), "from sample size n=", n, ".\n")
  if (sum(is.na(clin.limit))==0){
    cat("Given clinical relevant limit,", x.name, " and ", y.name, "agree with each other at discordance rate of", round(alpha.cl, 3), "with tolerance probability of", round(tol.prob.cl, 2), "from the same sample size n=", n, ".\n")
  }
  cat("\n")

  return(result)



}
