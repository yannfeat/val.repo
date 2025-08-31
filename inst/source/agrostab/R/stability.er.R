#' Regression on Environmental Index
#'
#' This function calculates the Erberhart&Rassel's stability parameters and the Dragavtsev's coefficient of multiplicativity.
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @return Returns a list of three objects:
#' \describe{
#'   \item{ANOVA}{the analysis of variance table}
#'   \item{scores}{the data frame object of stability analysis results:}
#'   \itemize{
#'     \item{\code{bi} regression of genotype means on environmental index}
#'     \item{\code{t_value} t-values for gypothesis that bi=1}
#'     \item{\code{p_value} p-values for gypothesis that bi=1 }
#'     \item{\code{s2di} individual squared deviation from regression }
#'     \item{\code{pf_value} p-values for gypothesis that s2di=0 }
#'     \item{\code{ai} Dragavtsev's coefficient of multiplicativity}
#'   }
#'   \item{Ij}{enviromental indexes}
#' }
#' @references
#' Eberhart, S.A. and W.A. Russell. 1966. Stability parameters for comparing varieties. Crop Sci 6: 36-40. doi:10.2135/cropsci1966.0011183X000600010011x
#' @examples
#' data(exp_data)
#' stability.er(exp_data,"yield","gen","env","rep")
#' @import dplyr ggplot2
#' @importFrom stats aggregate anova lm pf qt sd pt
#' @importFrom rlang .data
#' @export

stability.er <- function(dataf, res_var, gen_var, env_var, rep_var, plotIt=TRUE) {

  dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
  dataf$rep_var <- as.factor(dataf$rep_var)
  dataf$gen_var <- as.factor(dataf$gen_var)
  dataf$env_var <- as.factor(dataf$env_var)
  rps = length(levels(dataf$rep_var))  # number of reps
  en = length(levels(dataf$env_var))   # number of  environments
  ge = length(levels(dataf$gen_var))   # number of genotypes

    # ANOVA
  model <- lm(res_var ~ gen_var + env_var +  env_var:gen_var, data = dataf)
  amod1 <- anova(model)
  Df_error <- amod1['Residuals','Df'] #Df residuals
  SS_error <- amod1['Residuals','Sum Sq']/rps
  pooled_error <- amod1['Residuals','Mean Sq']/rps


  #Compute bi,sdi Er&Ras
  Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE)
  Yi. <- apply(Yij,1,mean,na.rm = TRUE)
  Y.j <- apply(Yij,2,mean,na.rm = TRUE)
  Y.. <- mean(dataf$res_var,na.rm = TRUE)
  Ij <- Y.j - Y..  #enviromental index
  SumKv1 <- sum(Ij^2)
  YijIj <- t(t(Yij)*Ij)
  Bi <- apply(YijIj,1,sum)/SumKv1 #regression coefficient

  deltai_SS <- (apply(Yij ^2, 1, sum)) - (((apply(Yij, 1, sum))^2) /en) - (apply(YijIj,1,sum))^2/SumKv1
# --- another variant to compute deltai
#  devij <- Yij-Yi.-matrix(Bi)%*%matrix(Ij,nrow=1)
#  deltai_SS <- apply(devij ^2, 1, sum)
#  ----
  S2di = ( deltai_SS / (en -2))  # squared deviation from regression

  S2bi =  deltai_SS /((en -2)*SumKv1) # squared standard error of Bi

  T_value <- abs(1-Bi)/sqrt(S2bi)
  P_value <- (1 - pt(T_value,(en-2)))*2

  # Anova for mean data

  meandf = data.frame(aggregate (res_var ~ gen_var + env_var, data = dataf, mean))
  model2 <- lm(res_var ~ gen_var*env_var , data = meandf)
  amod2 <- anova(model2)

  # sum of squares
  SS.G = amod2['gen_var','Sum Sq']
  SS.E = amod2['env_var','Sum Sq']
  SS.GxE = amod2['gen_var:env_var','Sum Sq']
  SS.E.GxE = SS.E + SS.GxE #same as  sum(Yij^2)- sum((apply(Yij, 1, sum))^2/en)
  SS.E.Linear = 1/ge * sum((apply(Yij, 2, sum)*Ij))^2/SumKv1 #
  SS.GxE.Linear = sum(Bi*YijIj) - SS.E.Linear # same as sum((apply(YijIj,1,sum))^2)/SumKv1 - SS.E.Linear
  pooled_dev <- sum(deltai_SS) # SS.E.Linear+SS.GxE.Linear+pooled_dev = SS.E.GxE

  #  Anova  result
  Df <-
    c(
      sum(amod2[,'Df']),                                    #1. total
      amod2['gen_var','Df'],                                #2. genotype
      amod2['env_var','Df']+amod2['gen_var:env_var','Df'],  #3. environment + GxE
      1,                                                   #4. env (linear)
      ge -1,                                               #5. GxE (linear)
      ge *(en-2),                                         #6. pooled deviations
      rep(en-2, ge),                                        #7.variety deviations
      Df_error                                             #7+ge pooled_error
      )
  SSS <-
    c(
      sum(amod2[,'Sum Sq']),
      amod2['gen_var','Sum Sq'],
      SS.E.GxE,
      SS.E.Linear,
      SS.GxE.Linear,
      pooled_dev,
      deltai_SS,
      SS_error
      )
  MSSS = (SSS/Df)
  FVAL <-
    c(
      NA,              #total
      MSSS[2]/MSSS[6], #for genotype = MS_gen/MS_pooled_dev
      NA,              #for environment + GxE
      NA,              #for environment (linear)
      MSSS[5]/MSSS[6], #for GxE (linear) = MS_GxE_Linear/MS_pooled_dev
      MSSS[6]/MSSS[length(MSSS)],# for pooled deviations = MS_pooled_dev/MS_error
      MSSS[7:(length(MSSS)- 1)]/MSSS[length(MSSS)],
      NA
      )

  PLINES = 1- pf(FVAL[7:(length(MSSS)- 1)],Df[7:(length(MSSS)- 1)], Df[length(Df)])

  pval <-
    c(
      NA,
      1- pf(FVAL[2], Df[2], Df[6]),
      NA,
      NA,
      1- pf(FVAL[5], Df[5], Df[6]),
      1- pf(FVAL[6], Df[6], Df[length(Df)]),
      PLINES,
      NA
      )
  anovadf <- data.frame(Df, `Sum Sq`=SSS, `Mean Sq`=MSSS, `F value`= FVAL, `Pr(>F)`= pval, check.names=FALSE)
  rownames(anovadf) <- c("Total","Genotypes","Env + (Gen x Env)", " Env (linear)", " Gen x Env(linear)", " Pooled deviation",paste0('  ',names(deltai_SS)), "Pooled error" )
  class(anovadf) <- c("anova","data.frame")


  #Dragavtsev
  ai <- (Yi.- Bi * Y..)/Yi.
  outdat = data.frame(bi = Bi,t_value = T_value, p_value=P_value, s2di = S2di, pf_value=PLINES,ai =ai)
  results = list(ANOVA = anovadf, scores = outdat,Ij = Ij)

  if (plotIt){

    S_plot <-  ggplot(outdat, aes(x=.data$s2di, y=.data$bi)) +
      geom_point() +
      geom_text(aes(label=rownames(outdat)), size=4, vjust=1) +
      geom_hline(yintercept=1) +
      xlab('Squared deviations (S2di)') +
      ylab('Regression (Bi)')
    print(S_plot)

    mydf = data.frame(aggregate (res_var ~ gen_var + env_var, dataf, mean, na.rm = TRUE))
    mydf1 <- mydf %>%
      dplyr::group_by(gen_var) %>%
      dplyr::mutate(I=Ij)
    mydf2 <- mydf1%>%
      dplyr::group_by(env_var) %>%
      dplyr::mutate(Fit=Yi.+Bi*I)
    mid <- data.frame(x=Ij,y=Y..+Ij)
    S_plot2 <-  ggplot(mydf2, aes(x=.data$I, y=.data$Fit, col= .data$gen_var)) +
      geom_line() +
      geom_line(data=mid,aes(x=.data$x,y=.data$y),colour='black',linetype="dashed",size=1)+
      xlab('Environment index Ij')+
      ylab('Predicted yield')+
      labs(col='')
      print(S_plot2)
  }
  return (results)
}

