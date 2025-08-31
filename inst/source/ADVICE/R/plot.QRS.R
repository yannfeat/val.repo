plot.QRS <- function(x, normqq = FALSE, scaleloc = FALSE, ...){
    res <- x$residuals
    fit <- x$fitted.values
    if (normqq) {
        qqnorm(res, ...)
        qqline(res)
    } else {
        if (scaleloc) {
            sigma <- sqrt(x$sigma2)
            stres <- res/sigma
            sqstres <- sqrt(abs(stres))
            plot(sqstres ~ fit, xlab = "Fitted values", ylab = expression(sqrt(abs("Standardized residuals"))), ...)
            lines(lowess(fit, sqstres), col=2)
        } else {
            plot(fit, res, type = 'p', main = "Residuals vs Fitted Values",
                xlab = "Fitted", ylab = "Residual", ...)
            abline(h = 0, lty = 2)
            lines(lowess(fit, res), col=2)
        }
    }
    invisible()
}
