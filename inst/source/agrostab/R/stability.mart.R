#' Weighted homeostaticity index
#'
#' This function calculates the Martynov's weighted homeostaticity index.
#' @param dataf the name of the data frame containing the data to analyze.
#' @param res_var the response variable.
#' @param gen_var the genotypes variable.
#' @param env_var the environments variable.
#' @param rep_var the replications variable.
#' @param alpha alpha level of LSD; default is 0.05.
#' @param plotIt a logical value specifying if plot should be drawn; default is TRUE
#' @return A numeric vector with weighted homeostaticity index of genotypes.
#' @references
#' Martynov S.P. 1990. A Method for the Estimation of Crop Varieties Stability. Biom. J. 7: 887-893.
#' @examples
#' data(exp_data)
#' stability.mart(exp_data,"yield","gen","env","rep")
#' @export

stability.mart <-
  function (dataf, res_var, gen_var, env_var, rep_var,alpha = 0.05, plotIt=TRUE){
    dataf <- data.frame(env_var = dataf[,env_var], gen_var = dataf[,gen_var],  rep_var = dataf[, rep_var],res_var = dataf[,res_var])
    dataf$rep_var <- as.factor(dataf$rep_var)
    dataf$gen_var <- as.factor(dataf$gen_var)
    dataf$env_var <- as.factor(dataf$env_var)
    rps = length(levels(dataf$rep_var))  # number of reps
    en = length(levels(dataf$env_var))   # number of  environments
    ge = length(levels(dataf$gen_var))   # number of genotypes

    model1 <- lm(res_var ~ gen_var +env_var +  gen_var:env_var, data = dataf)
    modav <- anova(model1)
    MS_env_gen <- modav['gen_var:env_var','Mean Sq'] #Mean Sq GxE

    Yij <- tapply(dataf$res_var, list(dataf$gen_var, dataf$env_var), mean, na.rm = TRUE)
    Y.j <- apply(Yij,2,mean,na.rm = TRUE)
    Y.. <- mean(Yij,na.rm = TRUE)
    LSD <- qt(alpha/2,(en-1)*(ge-1),lower.tail =F)*sqrt(MS_env_gen*(en+1)/(ge*en))
    unfav <- Y.j[Y.j < Y.. - LSD]
    fav <- Y.j[Y.j > Y.. + LSD]
    med <- Y.j[(Y.j <= Y.. + LSD)&(Y.j >= Y.. - LSD)]
    C_groups <- 0
    w_unfav <- 0
    w_fav <- 0
    w_med <- 0
    if (length(unfav)>0 ){
      C_groups <- C_groups+1
      w_unfav <- en*length(unfav)
    }
    if (length(fav)>0 ){
      C_groups <- C_groups+1
      w_fav <- en*length(fav)
    }
    if (length(med)>0 ){
      C_groups <- C_groups+1
      w_med <- en*length(med)
    }
    w_unfav <- w_unfav/C_groups
    w_fav <- w_fav/C_groups
    w_med <- w_med/C_groups
    w_j <- rep(0,en)
    names(w_j) <- names(Y.j)
    w_j[names(unfav)] <- w_unfav
    w_j[names(fav)] <- w_fav
    w_j[names(med)] <- w_med
    S_j <- apply(Yij,2,sd)
    hhh <- t((t(Yij)-Y.j)*w_j/S_j)
    Hi <- apply(hhh,1,sum)
    if(plotIt){
      barplot(Hi,ylab="weighted homeostaticity index",las=3)
    }

    return (Hi)

  }
