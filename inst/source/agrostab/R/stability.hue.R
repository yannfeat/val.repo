#' Nonparametric stability analysis
#'
#' This function calculates the Nassar&Huehn's stability parameters.
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @param alpha the significance level; default is 0.5
#' @return Returns a list of two objects:
#' \describe{
#'   \item{statistic}{the data frame object of stability analysis results:}
#'   \itemize{
#'     \item{\code{S1}-value of genotype}
#'     \item{\code{Z1}-value of genotype}
#'     \item{\code{S2}-value of genotype}
#'     \item{\code{Z2}-value of genotype}
#' }
#'   \item{scores}{the data frame object of summary results:}
#'   \itemize{
#'     \item{\code{Z1.sum} sum of Z1}
#'     \item{\code{Z2.sum} sum of Z2}
#'     \item{\code{chi.ind} chi-squared for (choosen alpha level)/(number of genotypes) and one degree of freedom}
#'     \item{\code{chi.sum} chi-squared for choosen alpha level and number of genotypes degree of freedom}
#' }
#' }
#' @references
#' Nassar, R. and M. Huehn. 1987. Studies on estimation of phenotypic stability: Tests of significance for nonparametric measures of phenotypic stability. Biometrics 43: 45-53. doi: 10.2307/2531947
#' @examples
#' data(exp_data)
#' stability.hue(exp_data,"yield","gen","env","rep")
#' @importFrom stats qchisq
#' @export

stability.hue <-
  function (dataf, res_var, gen_var, env_var, rep_var,alpha = 0.05,plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes

    Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE)

    Yi. <- apply(Yij,1,mean,na.rm = TRUE)
    Y.. <- mean(Yij,na.rm = TRUE)
    Yij_1 <- Yij-(Yi.-Y..)  #corrected values
    Rij <- apply(Yij_1,2,rank)
    S1_i <- rep(0,ge)
   for (i in 1:ge){
     for(j in 1:(en-1)){
       for (jj in (j+1):en){
         S1_i[i] <- S1_i[i]+abs(Rij[i,j]-Rij[i,jj])
       }
     }
   }
    S1_i <- S1_i*2/(en*(en-1))
    Ri. <- apply(Rij,1,mean)
    S2_i <- apply(((Rij - Ri.)^2),1,sum)/(en-1)

    S1_mean <- (ge^2 - 1)/(3*ge)
    S1_var <- (ge^2-1)*((ge^2-4)*(en+3)+30)/(45*ge^2*en*(en-1))
    Z1_i <- (S1_i-S1_mean)^2/S1_var

    S2_mean <- (ge^2 - 1)/12
    S2_var <- (ge^2-1)*(2*(ge^2-4)*(en-1)+5*(ge^2-1))/(360*en*(en-1))
    Z2_i <- (S2_i-S2_mean)^2/S2_var

    chi.ind <- qchisq(1-alpha/ge, 1)
    chi.sum <- qchisq(1-alpha, ge)
    Z1.sum <- sum(Z1_i)
    Z2.sum <- sum(Z2_i)

    outdat <-  data.frame(S1=S1_i,Z1=Z1_i,S2=S2_i,Z2=Z2_i)
    rownames(outdat) <-  rownames(Yij)
    stat2 <- data.frame(Z1.sum,Z2.sum,chi.ind,chi.sum)
    results = list(statistic = outdat, chi = stat2)
    if(plotIt){
      barplot(S1_i,names.arg = rownames(outdat),ylab="S1",las=3)
      barplot(S2_i,names.arg = rownames(outdat),ylab="S2",las=3)
    }
    return (results)

  }
