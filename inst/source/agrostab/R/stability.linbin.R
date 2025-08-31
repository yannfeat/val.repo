#' Superiority measure
#'
#' This function calculates the Lin&Binn's superiority measure.
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @return A numeric vector with superiority measure Pi of genotypes.
#' @references
#' Lin, C.S. and M.R. Binns. 1988. A superiority measure of cultivar performance for cultivar x location data. Can J Plant Sci 68: 193?198. doi: 10.4141/cjps88-018
#' @examples
#' data(exp_data)
#' stability.linbin(exp_data,"yield","gen","env","rep")
#' @importFrom graphics barplot
#' @export

stability.linbin <-
  function (dataf, res_var, gen_var, env_var, rep_var,plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes

    Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE) #mean values
    Mj <- apply(Yij,2,max) #max genotype's yield for j environment
    Pi <- apply( t(t(Yij)-Mj),1,function (x) sum(x^2)/(en*2)  )
    if(plotIt){
      barplot(Pi,ylab="superiority measure",las=3)
    }
    return (Pi)

  }
