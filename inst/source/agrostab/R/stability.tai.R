#' Tai's stability analysis
#'
#' This function calculates the Tai's stability parameters.
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
#'     \item{\code{alpha} regression of genotype means on environmental means}
#'     \item{\code{t_value} t-values for gypothesis that alpha=0}
#'     \item{\code{p_value} p-values for gypothesis that alpha=0 }
#'     \item{\code{lambda} deviation from linear responses}
#'     \item{\code{pf_value} p-values for gypothesis that lambda=0 }
#' }
#' }
#' @references
#' Tai, G.C.C. 1971. Genotypic stability analysis and application to potato regional trials. Crop Sci. 11: 184-190. doi:10.2135/cropsci1971.0011183X001100020006x
#' @examples
#' data(exp_data)
#' stability.tai(exp_data,"yield","gen","env","rep")
#' @import ggplot2
#' @importFrom stats aggregate anova lm pf qt sd qf cor
#' @importFrom rlang .data
#' @export

stability.tai <- function(dataf, res_var, gen_var, env_var, rep_var, plotIt=TRUE) {

  dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
  dataf$rep_var <- as.factor(dataf$rep_var)
  dataf$gen_var <- as.factor(dataf$gen_var)
  dataf$env_var <- as.factor(dataf$env_var)
  rps = length(levels(dataf$rep_var))  # number of reps
  en = length(levels(dataf$env_var))   # number of  environments
  ge = length(levels(dataf$gen_var))   # number of genotypes

  model1 <- lm(res_var ~ env_var  + rep_var%in%env_var + gen_var +  env_var:gen_var, data = dataf)
  modav <- anova(model1)
  MSL <- modav['env_var','Mean Sq'] #Mean Sq environments
  MSB <- modav['env_var:rep_var','Mean Sq'] #Mean Sq rep in env
  MSV <- modav['gen_var','Mean Sq'] #Mean Sq genotypes
  MSVL <- modav['env_var:gen_var','Mean Sq'] #Mean Sq GxE
  MSE <- modav['Residuals','Mean Sq'] #Mean Sq residuals

  Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE)
  Yi. <- apply(Yij,1,mean,na.rm = TRUE)
  Y.j <- apply(Yij,2,mean,na.rm = TRUE)
  Y.. <- mean(dataf$res_var,na.rm = TRUE)

  Lj <- Y.j-Y..                 # environment effect
  GLij <- t(t(Yij-Yi.)-Y.j)+Y.. #GxE effect
  Slgli <- apply(t(Lj*t(GLij)),1,sum,na.rm = TRUE)/(en-1) #covariance between environment and GxE effects
  S2gli <- apply(GLij^2,1,sum,na.rm = TRUE)/(en-1) #variance of the interaction effects

  alphai <- Slgli/((MSL-MSB)/(ge*rps)) # regression coefficients
  lambdai <- (S2gli-alphai*Slgli)/((ge-1)*MSE/(ge*rps)) #deviation from linear responses

  corr_coefi <- apply(GLij,1, function(x) cor(Lj,x))
  T_alphai <- sqrt((en-2)*corr_coefi^2/(1-corr_coefi^2))
  P_alphai <- (1 - pt(T_alphai,(en-2)))*2

  PF_lambdai <- 1-pf(lambdai,en-2,en*(ge-1)*(rps-2))

  SS_GE_Lin <- sum(alphai^2)*sum(Lj^2)*rps   #Squared sum of linear responses
  SS_GE_Dev <- modav['env_var:gen_var','Sum Sq'] - SS_GE_Lin # Squared sum of deviations from linear responses

  #ANOVA
  Df <- c(
    modav['env_var','Df'],         # environments
    modav['env_var:rep_var','Df'], # rep in env
    modav['gen_var','Df'],         # genotypes
    modav['env_var:gen_var','Df'], # GxE
    ge-1,                          #linear response
    (ge-1)*(en-2),                 #deviation from linear response
    modav['Residuals','Df']        #Error
  )
  SSS <- c(
    modav['env_var','Sum Sq'],         # environments
    modav['env_var:rep_var','Sum Sq'], # rep in env
    modav['gen_var','Sum Sq'],         # genotypes
    modav['env_var:gen_var','Sum Sq'], # GxE
    SS_GE_Lin,                          #linear response
    SS_GE_Dev,                         #deviation from linear response
    modav['Residuals','Sum Sq']           #Error
  )
  MSSS <- SSS/Df

  FVAL <- c(
    MSSS[1]/MSSS[2],         # environments
    MSSS[2]/MSSS[7], # rep in env
    MSSS[3]/MSSS[4],         # genotypes
    MSSS[4]/MSSS[7], # GxE
    MSSS[5]/MSSS[6],                          #linear response
    MSSS[6]/MSSS[7],                #deviation from linear response
    NA                                           #Error
  )

pval <-
  c(
    1- pf(FVAL[1], Df[1], Df[2]),
    1- pf(FVAL[2], Df[2], Df[7]),
    1- pf(FVAL[3], Df[3], Df[4]),
    1- pf(FVAL[4], Df[4], Df[7]),
    1- pf(FVAL[5], Df[5], Df[6]),
    1- pf(FVAL[6], Df[6], Df[7]),
    NA
  )
anovadf <- data.frame(Df, `Sum Sq`=SSS, `Mean Sq`=MSSS, `F value`= FVAL, `Pr(>F)`= pval, check.names=FALSE)
rownames(anovadf) <- c("Environments","Rep. in Env.", "Genotypes", "Gen. x Env.","  Linear responses","  Dev. from lin.res.","Error")
class(anovadf) <- c("anova","data.frame")

outdat = data.frame(alpha = alphai, t_value = T_alphai, p_value = P_alphai, lambda = lambdai, pf_value=PF_lambdai)

results = list(ANOVA = anovadf, scores = outdat)

if (plotIt){
  lambdai[lambdai < 0] <- 0
  confint = 0.95
  lmax <- max(c(lambdai, qf(1 - (1 - confint) / 2, en - 2, en * (ge - 1) * (rps - 1)))) * 1.1
  lx <- seq(0, lmax, lmax / 100)
  ta <- qt(1 - (1 - confint) / 2, en - 2)

  diff1 <- (en - 2) * MSL - (ta^2 + en - 2) * MSB

  if (diff1 < 0) {
    amax <- max(abs(alphai)) * 1.05
  } else {
    alpha.predlim <- ta * ((lx * (ge - 1) * MSE * MSL) / ((MSL - MSB) * diff1))^0.5
    amax <- max(c(abs(alphai), alpha.predlim))
  }

  S_plot <-  ggplot(outdat, aes(x=.data$lambda, y=.data$alpha)) +
    coord_cartesian(xlim = c(-0.05 * lmax, lmax), ylim = c(-amax, amax))+
    geom_point() +
    geom_text(aes(label=rownames(outdat)), size=4, vjust=1) +
    ylab(expression(alpha)) +
    xlab(expression(lambda))

  # alpha limits

  if (diff1 > 0) {
    dt2 <- as.data.frame(cbind(lx, alpha.predlim))
    S_plot <- S_plot +
      geom_path(data = dt2, aes(x = lx, y = alpha.predlim), col = "red") +
      geom_path(data = dt2, aes(x = lx, y = -alpha.predlim), col = "red")
  }

  # lambda limits

  S_plot <- S_plot +
    geom_vline(xintercept = qf((1 - confint) / 2, en - 2, en * ge * (rps - 1)), col = "gray") +
    geom_vline(xintercept = qf(1 - (1 - confint) / 2, en - 2, en * ge * (rps - 1)), col = "gray")

  print(S_plot)
}
return (results)

}

