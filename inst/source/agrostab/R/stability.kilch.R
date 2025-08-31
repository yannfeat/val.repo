#' Variance of specific adaptive ability
#'
#' This function calculates several stability parameters suggested by Kilchevsky & Khotyleva.
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @return Returns a list of two objects:
#' \describe{
#'   \item{ANOVA}{the analysis of variance table}
#'   \item{scores}{the data frame object of stability analysis results:}
#'   \itemize{
#'     \item{\code{mean} mean value}
#'     \item{\code{OAC} common adaptive ability}
#'     \item{\code{sigma_ge} variance of GE interaction}
#'     \item{\code{sigma_CAC} variance of specific adaptive ability}
#'     \item{\code{S_g} relative stability}
#' }
#' }
#' @references
#' Kilchevsky A.V., Khotyleva L.V. 1989. Genotype and environment in plant breeding. - Minsk: Science and technology. (In Russian).
#' @examples
#' data(exp_data)
#' stability.kilch(exp_data,"yield","gen","env","rep")
#' @import ggplot2
#' @importFrom stats anova lm
#' @export

stability.kilch <-
  function (dataf, res_var, gen_var, env_var, rep_var, plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes
    model1 <- lm(res_var ~ gen_var + env_var +  env_var:gen_var, data = dataf)
    modav <- anova(model1)

    pooled_error <- modav['Residuals','Mean Sq']/rps

    Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE)
    #..............
    Yi. <- apply(Yij,1,mean,na.rm = TRUE)
    Y.j <- apply(Yij,2,mean,na.rm = TRUE)
    Y.. <- mean(Yij,na.rm = TRUE)
    OACi <- Yi.-Y..
    VDik <- t(t(Yij-Yi.)-Y.j)+Y..
    CACik <- Yij-Yi.
    S_ge <- apply( VDik,1,function (x) sum(x^2))/(en-1)- pooled_error *(en*ge-en-ge+1)/(en*ge)
    S_cac <- apply( CACik,1,function (x) sum(x^2))/(en-1)- pooled_error *(en-1)/(en)
    S_g <- 100*sqrt(S_cac)/(Y..+OACi)
    outdat = data.frame(mean=Yi.,OAC=OACi,sigma_ge = S_ge, sigma_CAC = S_cac, S_g=S_g)
    results = list(ANOVA = modav, scores = outdat)
    if (plotIt){
      df <- data.frame(OACi,S_cac)
      CAC_plot <-  ggplot(df, aes(x=S_cac, y=OACi)) +
        geom_point() +
        geom_text(aes(label=rownames(df)), size=4, vjust=1) +
        ylab('common adaptive ability') +
        xlab('variance of specific adaptive ability')
      print(CAC_plot)
    }
    return (results)
  }
