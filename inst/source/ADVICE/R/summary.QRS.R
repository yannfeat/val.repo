summary.QRS <- function(object, ...){
   cat("Residuals:")
   cat("\n")
   res <- quantile(object$residuals, seq(0,1,.25))
   names(res)<- c(" Min"," 1Q", " Median", " 3Q",  " Max")
   print(res)
   cat("\n")
   cat("Coefficients:")
   cat("\n")
   Nrows<- ncol(object$x)
   coef_qrs <-matrix(0, nrow = Nrows, ncol = 4)
   colnames(coef_qrs)<- c(" Estimate " , " Std. Error ", " z score ", " Pr(>|z|) ")
   coefnames <- object$names[object$coefOrder]
   coefnames[coefnames == "y"] <- "Intercept"
   rownames(coef_qrs) <- coefnames
   indices <- 1:Nrows
   coef_qrs[,1] <- object$coefficients
   coef_qrs[,2] <- object$std_error
   coef_qrs[,3] <- object$coefficients/object$std_error
   abs_z<- abs(coef_qrs[,3])
   pr_greaterThan_z <-pnorm(abs_z, lower.tail = FALSE)
   coef_qrs[,4] <- 2*pr_greaterThan_z
   print(coef_qrs[indices,])
   cat("\n")
   df<-object$df.residual
   rse<- sqrt(object$sigma2)
#   rse <- sqrt(sum(object$residuals^2)/(length(y)-Nrows))
   cat("Residual standard error:", rse, "on", df, "degrees of freedom" )
   cat("\n")
}
