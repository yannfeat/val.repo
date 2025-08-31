confint.QRS <- function(object, parm, level = .95, ...){
  se <- object$std_error
  betaHat <- object$coefficients
  parnames <- object$names[object$coefOrder]
  parnames[parnames=="y"] <- "Intercept"
  if (missing(parm))
        parm <- parnames
    else if (is.numeric(parm))
        parm <- parnames[parm]
  betaHat <- setNames(betaHat, parnames)
  se <- setNames(se, parnames)
  n <- nrow(object$y)
  p <- ncol(object$x)
  degf <- object$df.residual
  alpha <- (1-level)/2
  a <- c(alpha, 1 - alpha)
  fac <-  fac <- qt(a, object$df.residual)
  ci <- array(NA_real_, dim = c(length(parm), 2L), dimnames = list(parm,
        a))
  ci[] <- betaHat[parm] + se[parm] %o% fac
  ci
}
