#' Steadiness of stability index
#'
#' This function calculates the Udachin's parameters of stability
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @return Returns a data frame:
#' \describe{
#'   \item{Ust}{the genotype's Steadiness of stability index}
#'   \item{intensity}{the genotype's intensity value}
#'   \item{max_val}{the genotype's yield max value}
#'   \item{min_val}{the genotype's yield min value}
#'   \item{S_opt}{the genotype's standard deviation at optimal environment}
#'   \item{S_lim}{the genotype's standard deviation at limited environment}
#'   \item{I_opt}{the genotype's stability index at optimal environment}
#'   \item{I_lim}{the genotype's stability index at limited environment}
#'   }
#' @references
#' Udachin R.A. 1990. Methods of assessing the ecological plasticity of wheat varieties. Selection and seed production. 5: 2-6. (In Russian)
#' @examples
#' data(exp_data)
#' stability.udach(exp_data,"yield","gen","env","rep")
#' @import ggplot2
#' @importFrom stats sd
#' @export

stability.udach <-
  function (dataf, res_var, gen_var, env_var, rep_var, plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes

    Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE) #mean
    Sij <- tapply(dataf$res_var,list(dataf$gen_var, dataf$env_var),sd,na.rm = TRUE)  #standard deviation
    Iij <- Yij^2/Sij  #Index of stability
    I_mean <- mean(Iij) #mean Index
    Y.. <- mean(dataf$res_var,na.rm = TRUE) #mean yield
    max_val <- apply(Yij,1,max)
    min_val <- apply(Yij,1,min)
    intensity <- apply(Yij,1,function(x) (max(x)-min(x))*100/Y..) #genotype's intensity value
    S_lim <- rep(0,ge) #genotype's standard deviation at limited environment
    S_opt <- rep(0,ge) #genotype's standard deviation at optimal environment
    I_lim <- rep(0,ge) #genotype's stability index at limited environment
    I_opt <- rep(0,ge) #genotype's stability index at optimal environment
    names(S_lim) <- rownames(Yij)
    names(S_opt) <- rownames(Yij)
    names(I_lim) <- rownames(Yij)
    names(I_opt) <- rownames(Yij)
    for ( i in 1: ge) {
      S_opt[i] <- Sij[i,which.max(Yij[i,])]
      I_opt[i] <- max(Yij[i,])^2/S_opt[i]
      S_lim[i] <- Sij[i,which.min(Yij[i,])]
      I_lim[i] <- min(Yij[i,])^2/S_lim[i]
    }
    stdi <- 100*(1-(I_opt-I_lim)/I_mean) # genotype's steadiness of stability index
    results <- data.frame(Ust=stdi,intensity,max_val,min_val,S_opt,S_lim,I_opt,I_lim)
    if (plotIt){
      df <- data.frame(stdi,intensity)
      U_plot <-  ggplot(df, aes(x=stdi, y=intensity)) +
        geom_point() +
        geom_text(aes(label=rownames(df)), size=4, vjust=1) +
        xlab('steadiness of stability index') +
        ylab('intensity value')
      print(U_plot)
    }
    return (results)

  }
