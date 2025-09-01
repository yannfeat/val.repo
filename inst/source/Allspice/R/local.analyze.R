analyze <- function(
    dat,
    bits,
    covariates=NULL,
    balance=FALSE) {

    # Check covariates.
    covnames <- NULL
    if(length(covariates) > 0) {
        keys <- intersect(rownames(dat), rownames(covariates))
        if(length(keys) < nrow(dat)) return("Incompatible covariates.")
        covariates <- covariates[rownames(dat),]

        # Check reserved variable names.
        reserved <- c("X","Y","B0","B1","MODEL")
        if(anyDuplicated(colnames(covariates)) > 0)
            return("Duplicated covariates.")
        if(anyDuplicated(c(colnames(covariates), reserved)) > 0)
            return("Reserved covariate names.")
	covnames <- colnames(covariates)
    }

    # Check classification.
    keys <- intersect(rownames(dat), rownames(bits))
    if(length(keys) < nrow(dat)) return("Incompatible bits.")
    bits <- bits[rownames(dat),]

    # Regression models.
    output <- list()
    for(v in colnames(dat)) {
        output[[v]] <- analyze_glm(xdat=dat[,v],
	    ydat=bits[,v], cdat=covariates, balance=balance)
    }

    # Set row and column names.
    output <- data.frame(output, stringsAsFactors=FALSE)
    rownames(output) <- c("B0", "B1", covnames)
    colnames(output) <- colnames(dat)

    # Convert to matrix.
    output <- makematrix(output)
    return(output)
}

#---------------------------------------------------------------------------

analyze_glm <- function(
    xdat,
    ydat,
    cdat=NULL,
    fam=binomial("probit"),
    balance=FALSE) {

    # Set up training data.
    ds <- data.frame(X=xdat, Y=ydat)
    if(length(cdat) > 0) ds <- data.frame(ds, cdat)

    # Add extra samples for numerical stability.
    mx <- mean(ds$X)
    xtra <- data.frame(Y=c(0,1), X=c(mx,mx))
    if(length(cdat) > 0) {
        mc <- colMeans(cdat)
        xtra <- data.frame(xtra, rbind(mc, mc))
    }
    ds <- rbind(ds, xtra)

    # Fit model.
    model <- list()
    if(balance) {
        n0 <- mean(ds$Y == 0)
        n1 <- mean(ds$Y == 1)
        w <- (n0*(ds$Y == 1) + n1*(ds$Y == 0))
        suppressWarnings(
            model <- glm(formula=Y~., data=ds, weights=w, family=fam))
    }
    else {
        suppressWarnings(
            model <- glm(formula=Y~., data=ds, family=fam))    
    }
    return(model$coefficients)
}