#' Power calculation for Biomarker-Informed Design with Hierarchical Model
#'
#' Given the Biomarker-Informed design information, returns the overall power and probability of the arm is selected as the winner.
#'
#' @usage
#' BioInfo.Power(uCtl, u0y, u0x, rhou, suy, sux, rho, sy, sx, Zalpha, N1, N, nArms, nSims)
#' @param uCtl mean value for the control group.
#' @param u0y mean parameter of the group 1 for the parent model.
#' @param u0x mean parameter  of the group 2 for the parent model.
#' @param rhou correlation coefficient between two groups for the parent model.
#' @param suy standard deviation of the group 1 for the parent model.
#' @param sux standard deviation of the group 2 for the parent model.
#' @param rho correlation coefficient between two groups for the lower level model.
#' @param sy standard deviation of the group 1 for the lower level model.
#' @param sx standard deviation of the group 2 for the lower level model.
#' @param Zalpha crtical point for rejection.
#' @param  N1 sample size per group at interim analysis.
#' @param  N sample size per group at final analysis.
#' @param nArms number of active groups.
#' @param nSims number of simulation times.
#' @return
#' The evaluated power and probability of selecting the arm as the winner.
#' @author Yalin Zhu
#' @references Chang, M. (2014). Adaptive design theory and implementation using SAS and R.
#' \emph{CRC Press}.
#'
#' @examples
#' ## Determine critical value Zalpha for alpha (power) =0.025
#' u0y=c(0,0,0); u0x=c(0,0,0)
#' BioInfo.Power(uCtl=0, u0y, u0x, rhou=1, suy=0, sux=0, rho=1, sy=4, sx=4,
#'  Zalpha=2.772, N1=100, N=300, nArms=3, nSims=1000)
#' ## Power simulation
#' u0y=c(1,0.5,0.2)
#' u0x=c(2,1,0.5)
#' BioInfo.Power(uCtl=0, u0y, u0x, rhou=0.2, suy=0.2, sux=0.2, rho=0.2, sy=4, sx=4,
#'  Zalpha=2.772, N1=100, N=300, nArms=3, nSims=500)
#'
#'@import stats
#'@import  mvtnorm
#' @export
BioInfo.Power <- function(uCtl, u0y, u0x, rhou, suy, sux, rho, sy, sx, Zalpha, N1, N, nArms, nSims){
  uy=rep(0,nArms); ux=rep(0,nArms); probWinners=rep(0,nArms); power = 0
  varcov0=matrix(c(suy^2,rhou*suy*sux,rhou*suy*sux, sux^2),2,2)
  varcov=matrix(c(sy^2, rho*sy*sx, rho*sx*sy, sx^2),2,2)
  for (i in 1: nSims) {
    winnerMarker= -Inf
    for (j in 1: nArms) {
      u=  rmvnorm(1,mean=c(u0y[j],u0x[j]), sigma=varcov0)
      uy[j]=u[1]; ux[j]=u[2]
      dataStg1=  rmvnorm(N1, mean=c(uy[j], ux[j]), sigma=varcov)
      meanxMarker=mean(dataStg1[,2])
      if (meanxMarker>winnerMarker)
      {winner=j; winnerMarker=meanxMarker; winnerY=dataStg1[,1]}
    } ## End of j ##
    for (j in 1:nArms) {if (winner==j) {probWinners[j]=probWinners[j]+1/nSims}}
    yStg1=winnerY
    yStg2=rnorm(N-N1, mean=uy[winner], sd=sy)
    yTrt=c(yStg1+yStg2)
    yCtl=rnorm(N, mean=uCtl, sd=sy)
    tValue=t.test(yTrt,yCtl)$statistic
    if (tValue>=Zalpha) {power=power+1/nSims} } ##Endofi##
  return (c(power, probWinners))
}
