QRS <- function(x, y, Nsims = 100) {
    re_Order <- function(model_df) {
       xy.lm <- lm(y ~ ., data = model_df)
       coefOrder <- order(summary(xy.lm)$coefficients[,4])
       return(list(coefOrder = coefOrder, model = xy.lm))
    }
    Beta_Estimation = function(lm.object, coefOrder, Nsims){
        y <- model.frame(lm.object)$y
        X <- model.matrix(lm.object)[, coefOrder]
        n <- length(y)
        p <- length(coef(lm.object))
        X.qr <- qr(X)
        Q <- qr.Q(X.qr, complete = TRUE)
        Q1 <- Q[, (1 : p)]
        Q2 <- Q[, -(1 : p)]
        U <- qr.R(X.qr)
        Uinv<-backsolve(U, diag(p))
        uBhat <- t(Q1) %*% y
        Qres<- t(Q2) %*% y
        cutoff <- min(abs(quantile(Qres, c(.005, .995))))
        indices <- (abs(uBhat) > cutoff)
        UbHat_keep <- uBhat*indices
        betaHat <- as.vector(backsolve(U, UbHat_keep))
        if (p<n){     # if #feature are smaller than #samples
            sigma2hat <- sum(Qres^2)
            sigma2hat <- sigma2hat/(n-p)
        }
        eps <- matrix(rnorm(Nsims*n , sd = sqrt(sigma2hat)),nrow =n)
        temp<-t(Q1)%*%eps
        tempPlusUbhat <- matrix(uBhat,nrow = p,ncol = Nsims) +temp
        temp_indices <- abs(tempPlusUbhat)>cutoff
        Vhat<-tempPlusUbhat*temp_indices
        Vstar<-cov(t(Vhat))
        vUstar_temp<-Uinv%*%Vstar
        vUstar<-vUstar_temp%*%t(Uinv)
        SE <- sqrt(diag(vUstar))
        SE <- as.vector(SE)
        SE <- sqrt(diag(solve(t(U)%*%U))*sigma2hat)
        z <- betaHat/SE
        pvalue <- 2*(1 - pnorm(abs(z)))
        mylist = list(coefs = betaHat, SE = SE, z = z, pvalue = pvalue, sigma2 = sigma2hat, 
            modelmatrix = X, rank = sum(abs(betaHat)>1e-16), effects=c(UbHat_keep, Qres), 
            qr = X.qr, df.residuals=n-p)
        return(mylist)
    }
    xy <- data.frame(x[,-1], y)
    orderOut <- re_Order(xy)
    cOrder <- orderOut$coefOrder
    xy.lm <- orderOut$model
    fit <-Beta_Estimation(xy.lm, cOrder, Nsims)
    est_coef <- fit$coefs
    sigma2 <- fit$sigma2
    std_error <- fit$SE
    X <- fit$modelmatrix
    fitteds <- X%*%matrix(est_coef, ncol=1)
    residuals <- y - fitteds
    qrs<- list(coefficients = est_coef,
             residuals = residuals,
             effects = fit$effects,
             rank = fit$rank,
             fitted.values = fitteds,
             sigma2 = sigma2, 
             std_error = std_error,
             df.residual = fit$df.residuals,
             x = x, y = y, qr = fit$qr, coefOrder=cOrder)
    class(qrs)<- c("QRS", "lm")
    return(qrs)
}
