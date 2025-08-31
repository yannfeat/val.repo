#' Stability variance
#'
#' This function calculates the Shukla's stability variance.
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
#'     \item{\code{bi} regression of genotype means on environmental means}
#'     \item{\code{t_value} t-values for gypothesis that bi=0}
#'     \item{\code{p_value} p-values for gypothesis that bi=0 }
#'     \item{\code{sigma} Shukla's stability variance value}
#'     \item{\code{pf_value} p-values for gypothesis that sigmai=0 }
#' }
#' }
#' @references
#' Shukla, G.K. 1972. Some statistical aspects of partitioning genotype-environmental components of variability. Heredity 29: 237-245. doi: 10.1038/hdy.1972.87
#' @examples
#' data(exp_data)
#' stability.shu(exp_data,"yield","gen","env","rep")
#' @import ggplot2
#' @importFrom stats aggregate anova lm pf qt sd
#' @importFrom rlang .data
#' @export

stability.shu <-
  function (dataf, res_var, gen_var, env_var, rep_var, plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes

    model1 <- lm(res_var ~ gen_var +env_var  +  gen_var:env_var, data = dataf)
    modav <- anova(model1)
    Df_error <- modav['Residuals','Df'] #Df residuals
    SK_error <- modav['Residuals','Sum Sq']/rps #Sum Sq residuals
    MS_error <- modav['Residuals','Mean Sq']/rps #Sum Sq residuals


    Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE)
    Yi. <- apply(Yij,1,mean,na.rm = TRUE)
    Y.j <- apply(Yij,2,mean,na.rm = TRUE)
    Y.. <- mean(dataf$res_var,na.rm = TRUE)
    SumKv1 <- sum((Y.j-Y..)^2)
    mat1 <- t((t(Yij)-Y.j)*(Y.j-Y..))/SumKv1
    Bi <- apply(mat1,1,sum)

    Ij <- Y.j-Y..                 # environment effect
    GEij <- t(t(Yij-Yi.)-Y.j)+Y.. #GxE effect

    corr_coefi <- apply(GEij,1, function(x) cor(Ij,x))
    T_Bi <- sqrt((en-2)*corr_coefi^2/(1-corr_coefi^2)) #t-value for Bi
    P_Bi <- (1 - pt(T_Bi,(en-2)))*2                   #p-value for Bi

    #-------------------------sigma
    Wij <- t(t(Yij-Yi.)-Y.j)+Y..
    Wi <-  apply(Wij^2,1,sum)
    SIGMAi <- (ge*(ge-1)*Wi - sum(Wi))/((en-1)*(ge-1)*(ge-2))/rps
    F_SIGMAi <- SIGMAi/MS_error
    pSIGMAi <- 1-pf(F_SIGMAi,en-1,Df_error)
    #..............  ANOVA for mean values

    meandf = data.frame(aggregate (res_var ~ gen_var + env_var, data = dataf, mean))
    model2 <- lm(res_var ~ gen_var*env_var , data = meandf)
    modav2 <- anova(model2)

    SK_env_gen <- modav2['gen_var:env_var','Sum Sq'] #Sum Sq GxE
    SK_heterog <- sum(Bi^2)*SumKv1  #Sum Sq heterogeneity
    SK_balance <- SK_env_gen - SK_heterog  #Sum Sq balance

    Df <-
      c(
        modav2['gen_var','Df'],
        modav2['env_var','Df'],
        modav2['gen_var:env_var','Df'],
        ge -1,
        (ge-1) *(en-2),
        rep(NA,ge),
        Df_error
        )
    SSS <-
      c(
        modav2['gen_var','Sum Sq'],
        modav2['env_var','Sum Sq'],
        modav2['gen_var:env_var','Sum Sq'],
        SK_heterog,
        SK_balance,
        rep(NA,ge),
        SK_error
        )
    MSSS <- SSS/Df
    MSSS[6:(5+ge)] <- SIGMAi

    FVAL <-
      c(
        MSSS[1]/MSSS[3], # MS_Gen/MS_GxE
        MSSS[2]/MSSS[6], # MS_Env/MS_Err
        MSSS[3]/MSSS[6], # MS_GxE/MS_Err
        MSSS[4]/MSSS[5], # MS_heterog/MS_balance
        MSSS[5]/MSSS[6], # MS_balance/MS_Err
        F_SIGMAi,
        NA
      )
    pval <-
      c(
        1- pf(FVAL[1], Df[1], Df[3]),
        1- pf(FVAL[2], Df[2], Df[6]),
        1- pf(FVAL[3], Df[3], Df[6]),
        1- pf(FVAL[4], Df[4], Df[5]),
        1- pf(FVAL[5], Df[5], Df[6]),
        pSIGMAi,
        NA
        )

    anovadf <- data.frame(Df, `Sum Sq`=SSS, `Mean Sq`=MSSS, `F value`= FVAL, `Pr(>F)`= pval, check.names=FALSE)
    rownames(anovadf) <- c("Genotypes","Environments", "Gen x Env","  Heterogeneity","  Balance",paste0('  ',rownames(Yij)),"Pooled error" )
    class(anovadf) <- c("anova","data.frame")

    outdat = data.frame(bi = Bi, t_value = T_Bi, p_value=P_Bi, sigma = SIGMAi*rps,  pf_value=pSIGMAi)

    results = list(ANOVA = anovadf, scores = outdat)
    if (plotIt){
      S_plot <-  ggplot(outdat, aes(x=.data$sigma, y=.data$bi)) +
        geom_point() +
        geom_text(aes(label=rownames(outdat)), size=4, vjust=1) +
        ylab('Bi') +
        xlab('Stability variance')
      print(S_plot)
    }
    return (results)

  }
