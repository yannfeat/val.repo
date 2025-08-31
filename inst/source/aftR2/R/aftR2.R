# ----------------------------------------------------------------------------------
# Function for implementing the R-squared measure for AFT model
# ----------------------------------------------------------------------------------
#' @title R-squared measure under the accelerated failure time (AFT) models.
#' @description Compute the R-squared measure under the accelerated failure time (AFT) models proposed in the Chan et. al (2018) paper (DOI: 10.1080/03610918.2016.1177072).
#' @param fit.AFT The fitted AFT model from the survival::survreg function
#' @return The R-squared measure
#' @examples
#' library(survival)
#' fit.AFT = survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist='weibull')
#' aftR2(fit.AFT)
#' @export
#' @import survival
aftR2 <- function(fit.AFT){
    dist = fit.AFT$dist
    sigma = fit.AFT$scale

    var_Zbeta = var(fit.AFT$linear.predictors)

    if(dist == "logistic" | dist == "loglogistic"){
        var_epsilon = pi^2/3
    }else if(dist == "weibull" | dist == "exponential"){
        var_epsilon = pi^2/6
    }else if(dist == "gaussian" | dist == "lognormal"){
        var_epsilon = 1
    }

    R2 = 1 - sigma^2*var_epsilon/(var_Zbeta + sigma^2*var_epsilon)

    return(R2)
}


