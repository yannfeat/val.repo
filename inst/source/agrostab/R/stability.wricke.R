#' Ecovalence
#'
#' This function calculates the Wricke's ecovalence.
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @return A numeric vector with  genotype's ecovalence.
#' @references
#' Wricke, G., 1962. Tjber eine Methode zur Erfassung der okologischen Streubreite in Feldversuchen. Z. Pflanzenzuchtg. 47: 92-96.
#' @examples
#' data(exp_data)
#' stability.wricke(exp_data,"yield","gen","env","rep")
#' @importFrom graphics barplot
#' @export

stability.wricke <-
  function (dataf, res_var, gen_var, env_var, rep_var, plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes

    Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE)
    Yi. <- apply(Yij,1,mean,na.rm = TRUE)
    Y.j <- apply(Yij,2,mean,na.rm = TRUE)
    Y.. <- mean(dataf$res_var,na.rm = TRUE)
    Wij <- t(t(Yij-Yi.)-Y.j)+Y..

    Wi <-  apply(Wij^2,1,sum)
    if(plotIt){
      barplot(Wi,ylab="ecovalence",las=3)
    }
    return (Wi)

  }
