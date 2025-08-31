#' Coefficient of homeostaticity
#'
#' This function calculates the Khangildin's coefficient of homeostaticity
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @return Returns a data frame:
#' \describe{
#'   \item{mean_all}{the genotype's mean}
#'   \item{mean_opt}{the genotype's max yield value}
#'   \item{mean_lim}{the genotype's min yield value}
#'   \item{sd}{the genotype's standard deviation}
#'   \item{hom}{the genotype's coefficient of homeostaticity}
#' }
#' @references
#' Khangildin V.V., Shayakhmetov I.F., Mardamshin A.G. 1979. Homeostasis of crop components and prerequisites for creating a model of a spring wheat variety. In Genetic analysis of quantitative traits of plants, 5-39. Ufa. (In Russian)
#' @examples
#' data(exp_data)
#' stability.hom(exp_data,"yield","gen","env","rep")
#' @importFrom stats sd
#' @importFrom graphics barplot
#' @export

stability.hom <-
  function (dataf, res_var, gen_var, env_var, rep_var, plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes

    Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE)
    Yi. <- tapply(dataf$res_var, list(dataf$gen_var), mean, na.rm = TRUE) # genotype's mean
    Sdi <- tapply(dataf$res_var,dataf$gen_var,sd,na.rm = TRUE)  #genotype's standard deviation
    max_val <- apply(Yij,1,max)
    min_val <- apply(Yij,1,min)
    hom <- Yi.^2/(Sdi*(max_val-min_val))
    if(plotIt){
      barplot(hom,ylab="coefficient of homeostaticity",las=3)
    }
    res_tab <- data.frame(mean_all=Yi.,mean_opt=max_val,mean_lim=min_val,sd=Sdi,hom=hom)
    return (res_tab)

  }
