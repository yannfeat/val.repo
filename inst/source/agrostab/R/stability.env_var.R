#' Environmental variance
#'
#' This function calculates the Roemer's environmental variance.
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @return A numeric vector with environmental variances of genotypes.
#' @references
#' Becker, H.C. and J. Leon. 1988. Stability analysis in plant breeding. Plant Breeding 101: 1-23.
#' @examples
#' data(exp_data)
#' stability.env_var(exp_data,"yield","gen","env","rep")
#' @importFrom graphics barplot
#' @export

stability.env_var <-
  function (dataf, res_var, gen_var, env_var, rep_var,plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes

    Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE)
    Yi. <- apply(Yij,1,mean,na.rm = TRUE)
    env_variance <- apply( (Yij - Yi.),1,function (x) sum(x^2)/(en-1)  )
    if(plotIt){
      barplot(env_variance,ylab="environmental variance",las=3)
    }
    return (env_variance)

  }
