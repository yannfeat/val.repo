#' Coefficient of variation
#'
#' This function calculates the Francis&Kannenberg's parameters of stability
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @return Returns a data frame:
#' \describe{
#'   \item{CV}{the genotype's coefficient of variation}
#'   \item{Mean}{the genotype's mean}
#' }
#' @references
#' Francis, T.R. and L.W. Kannenberg. 1978. Yield stability studies in short-season maize. I. A descriptive method for grouping genotypes. Can J Plant Sci 58: 1029?1034. doi: 10.4141/cjps78-157
#' @examples
#' data(exp_data)
#' stability.cv(exp_data,"yield","gen","env","rep")
#' @import ggplot2
#' @importFrom stats sd
#' @importFrom rlang .data
#' @export

stability.cv <-
  function (dataf, res_var, gen_var, env_var, rep_var, plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes

    Sdi <- tapply(dataf$res_var,dataf$gen_var,sd,na.rm = TRUE)  #genotype's variation
    Yi. <- tapply(dataf$res_var,dataf$gen_var,mean,na.rm = TRUE)  # genotype's mean
    Y.. <- mean(dataf$res_var,na.rm = TRUE)
    Sd <- sd(dataf$res_var,na.rm = TRUE)
    CVi <- 100*Sdi/Yi.
    CV <- 100*Sd/Y..
    if (plotIt){
    df <- data.frame(CV=CVi,Mean=Yi.)
    CV_plot <-  ggplot(df, aes(x=.data$CV, y=.data$Mean)) +
       geom_point() +
       geom_text(aes(label=rownames(df)), size=4, vjust=1) +
       geom_hline(yintercept=Y..) +
       geom_vline(xintercept=CV)+
       xlab('coefficient of variation')+
       ylab('mean value')
    print(CV_plot)
    }
    return (df)

  }
