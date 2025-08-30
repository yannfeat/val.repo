#' Power Calculation for Two Coprimary Endpoints.
#'
#' Given the group sequential design information, returns the overall power.
#'
#' @usage
#' CopriEndpt.Power(n, tau, mu1, mu2, rho, alpha1, alpha2, alternative)
#' @param n sample size for the design.
#' @param tau information time for the interim analysis.
#' @param mu1 mean value for coprimary endpoint 1.
#' @param mu2 mean value for coprimary endpoint 2.
#' @param rho correlation coefficient between two coprimary endpoints.
#' @param alpha1 significant level for the first stage.
#' @param alpha2 significant level for the second stage.
#' @param alternative indicates the alternative hypothesis and must be one of \code{"two.sided"} or \code{"two.sided"}.
#' @return
#' The evaluated power with attributes and computational error.
#' @author Yalin Zhu
#' @references Chang, M. (2014). Adaptive design theory and implementation using SAS and R.
#' \emph{CRC Press}.
#'
#' @examples
#' # Example in Chang (2014) page  272
#' CopriEndpt.Power(n=197, tau=0.5, mu1=0.2, mu2=0.2, rho=0.5,
#' alpha1=0.0025, alpha2=0.024, alternative="one.sided")
#'  sapply(c(-0.8,-0.5,-0.2,0,0.2,0.5,0.8),CopriEndpt.Power,
#' n=197, tau=0.5, mu1=0.2, mu2=0.2, alpha1=0.0025, alpha2=0.024, alternative="one.sided")
#' @import stats
#' @import mvtnorm
#' @export

CopriEndpt.Power <- function (n, tau, mu1, mu2, rho, alpha1, alpha2, alternative = c("two.sided", "one.sided")){
  alternative <- match.arg(alternative)
  za <- switch (alternative,
    two.sided = {c(qnorm(1-alpha1/2), qnorm(1-alpha2/2))},
    one.sided = {c(qnorm(1-alpha1), qnorm(1-alpha2))}
  )
  u1 <- sqrt(n*tau)*mu1; u2 <- sqrt(n)*mu2
  r12 <- rho; r13 <- sqrt(tau); r14 <- r12*r13
  r23 <- r14; r24 <- r13; r34 <- r12
  s1 <- matrix(c(1,r12, r12,1), 2,2)
  s2 <- matrix(c(1,r12,r13, r14, r12,1, r23, r24, r13, r23,1, r34, r14, r24, r34, 1), 4,4)
  power1 <-  pmvnorm(lower = c(za[1],za[1]), upper = c(Inf, Inf), rep(u1,2), s1)
  power2 <-  pmvnorm(lower = c(za[2],za[2]), upper = c(Inf, Inf), rep(u2,2), s1)
  powerOverlap <-  pmvnorm(lower = c(za[1], za[1], za[2], za[2]), upper = rep(Inf, 4), c(u1, u1, u2, u2), s2)
  power <- power1 + power2 - powerOverlap
  return (power)
}


#' Power Simulation for Two Group Two Coprimary Endpoints Group Sequential Design.
#'
#' Given the group sequential design information, returns the simulated overall power.
#'
#' @usage
#' TwoGrpCopriEndpt.SimPower(mu11,mu12, mu21, mu22, rho, tau,
#'  alpha1, alpha2, alternative , Nmax, B)
#' @param mu11 standardized mean value for coprimary endpoint 1 in group 1.
#' @param mu12 standardized mean value for coprimary endpoint 2 in group 1.
#' @param mu21 standardized mean value for coprimary endpoint 1 in group 2.
#' @param mu22 standardized mean value for coprimary endpoint 2 in group 2.
#' @param rho correlation coefficient between two coprimary endpoints.
#' @param tau information time for the interim analysis.
#' @param alpha1 significant level for the first stage.
#' @param alpha2 significant level for the second stage.
#' @param alternative indicates the alternative hypothesis and must be one of \code{"two.sided"} or \code{"two.sided"}.
#' @param Nmax maximum sample size per group.
#' @param B the simulation iterative time.
#' @return
#' The evaluated power with attributes and computational error.
#' @author Yalin Zhu
#' @references Chang, M. (2014). Adaptive design theory and implementation using SAS and R.
#' \emph{CRC Press}.
#'
#' @examples
#' # Example in Chang (2014) page  275
#' TwoGrpCopriEndpt.SimPower(mu11=0.2,mu12=0.25, mu21=0.005, mu22=0.015, rho=0.25,
#' tau=0.5, alpha1=0.0025, alpha2=0.024, alternative = "two.sided",Nmax=584, B=10000)
#' @export
TwoGrpCopriEndpt.SimPower <- function(mu11,mu12, mu21, mu22, rho, tau, alpha1, alpha2, alternative= c("two.sided", "one.sided"), Nmax, B=10000){
  alternative <- match.arg(alternative)
  za <- switch (alternative,
                two.sided = {c(qnorm(1-alpha1/2), qnorm(1-alpha2/2))},
                one.sided = {c(qnorm(1-alpha1), qnorm(1-alpha2))}
  )
  rej <- rep(0,B)
  for (i in 1:B){
    n <- round(Nmax*tau)
    varcov <- matrix(c(1,rho,rho,1),2,2)

    trtStg1 <-  rmvnorm(n,mean=c(mu11,mu12), sigma=varcov)
    ctStg1  <-  rmvnorm(n,mean=c(mu21,mu22), sigma=varcov)
    t11 <- t.test(trtStg1[,1],ctStg1[,1])$statistic
    t12 <- t.test(trtStg1[,2],ctStg1[,2])$statistic

    trtStg2 <-  rmvnorm(Nmax-n, mean=c(mu11,mu12), sigma=varcov)
    ctStg2  <-  rmvnorm(Nmax-n, mean=c(mu21,mu22), sigma=varcov)
    trt1 <- c(trtStg1[,1], trtStg2[,1]); trt2 <- c(trtStg1[,2], trtStg2[,2])
    ct1  <- c(ctStg1[,1], ctStg2[,1]); ct2  <- c(ctStg1[,2], ctStg2[,2])
    t21 <- t.test(trt1,ct1)$statistic; t22 <- t.test(trt2,ct2)$statistic

    rej[i]=(t11>za[1] & t12>za[1]) | (t21>za[2] & t22>za[2])
    return (mean(rej))
  }
}


#' Conditional power for one-arm, two-stage design with two primary endpoints
#'
#' Given the group sequential design information, returns the conditional power.
#'
#' @usage
#' OneArm.CondPower(mu1, mu2, n1, n2, rho, tau, alpha2, alternative)
#' @param mu1 mean value for the first stage (endpoint 1).
#' @param mu2 mean value for the second stage (endpoint 2).
#' @param n1 sample size for the first stage.
#' @param n2 sample size for the second stage.
#' @param rho correlation coefficient between two coprimary endpoints.
#' @param tau information time for the interim analysis.
#' @param alpha2 significant level for the second stage.
#' @param alternative indicates the alternative hypothesis and must be one of \code{"two.sided"} or \code{"two.sided"}.
#' @return
#' The evaluated power with attributes and computational error.
#' @author Yalin Zhu
#' @references Chang, M. (2014). Adaptive design theory and implementation using SAS and R.
#' \emph{CRC Press}.
#'
#' @examples
#' # Example in Chang (2014) page  277
#' OneArm.CondPower(mu1=0.1333, mu2=0.1605, n1=130, n2=130, rho=0.35,
#'  tau=0.5, alpha2=0.024, alternative = "one.sided")
#' OneArm.CondPower(mu1=0.1333, mu2=0.1605, n1=130, n2=414, rho=0.35,
#'  tau=0.5, alpha2=0.024, alternative = "one.sided")
#' @export

OneArm.CondPower <- function(mu1, mu2, n1, n2, rho, tau, alpha2, alternative= c("two.sided", "one.sided")){
  alternative <- match.arg(alternative)
  za2 <- switch (alternative,
                two.sided = {qnorm(1-alpha2/2)},
                one.sided = {qnorm(1-alpha2)}
  )
  Z1t=sqrt(n1)*mu1; Z2t=sqrt(n1)*mu2
  mean= c(sqrt(n2)*mu1,sqrt(n2)*mu2)
  s=matrix(c(1,rho, rho,1), 2,2)
  c1=(za2-sqrt(tau)*Z1t)/(1-tau)
  c2=(za2-sqrt(tau)*Z2t)/(1-tau)
  return( pmvnorm(lower=c(c1,c2), upper=c(Inf, Inf), mean, s))
}


#' Conditional power for two-group design, two-stage design with two primary endpoints
#'
#' Given the group sequential design information, returns the conditional power.
#'
#'
#' @usage
#' TwoArms.CondPower(mu1, mu2, sigma1, sigma2, n1, n2, rho, tau, alpha2, alternative)
#' @param mu1 mean value for the first stage (endpoint 1).
#' @param mu2 mean value for the second stage (endpoint 2).
#' @param sigma1 standard deviation for the first stage.
#' @param sigma2 standard deviation for the second stage.
#' @param n1 sample size for the first stage.
#' @param n2 sample size for the second stage.
#' @param rho correlation coefficient between two coprimary endpoints.
#' @param tau information time for the interim analysis.
#' @param alpha2 significant level for the second stage.
#' @param alternative indicates the alternative hypothesis and must be one of \code{"two.sided"} or \code{"two.sided"}.
#' @return
#' The evaluated power with attributes and computational error.
#' @author Yalin Zhu
#' @references Chang, M. (2014). Adaptive design theory and implementation using SAS and R.
#' \emph{CRC Press}.
#'
#' @examples
#' # Example in Chang (2014) page  278
#' TwoArms.CondPower(mu1=0.28, sigma1=1.9, mu2=0.35, sigma2=2.2, n1=340, n2=340,
#' rho=0.3, tau=0.5, alpha2=0.024, alternative = "one.sided")
#' TwoArms.CondPower(mu1=0.28, sigma1=1.9, mu2=0.35, sigma2=2.2, n1=340, n2=482,
#' rho=0.3, tau=0.5, alpha2=0.024, alternative = "one.sided")
#' TwoArms.CondPower(mu1=0.32, sigma1=2, mu2=0.4, sigma2=1.8, n1=340, n2=340,
#' rho=0.3, tau=0.5, alpha2=0.024, alternative = "one.sided")
#' @export
TwoArms.CondPower <- function(mu1, mu2, sigma1, sigma2, n1, n2, rho, tau, alpha2, alternative= c("two.sided", "one.sided")){
  alternative <- match.arg(alternative)
  za2 <- switch (alternative,
                 two.sided = {qnorm(1-alpha2/2)},
                 one.sided = {qnorm(1-alpha2)}
  )
  delta1=mu1/sqrt(2)/sigma1; delta2=mu2/sqrt(2)/sigma2
  Z1t=sqrt(n1)*delta1; Z2t=sqrt(n1)*delta2
  mean= c(sqrt(n2)*delta1,sqrt(n2)*delta2)
  s=matrix(c(1,rho, rho,1), 2,2)
  c1=(za2-sqrt(tau)*Z1t)/(1-tau); c2=(za2-sqrt(tau)*Z2t)/(1-tau)
  return ( pmvnorm(lower=c(c1,c2), upper=c(Inf, Inf), mean, s))
}
