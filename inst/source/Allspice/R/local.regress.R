regress <- function(
    dat,
    coeff,
    covariates=NULL,
    intercept="B0",
    variable="B1") {

    # Check data.
    output <- makematrix(dat)
    if(is.character(output)) stop(output)

    # No covariates.
    if(length(covariates) < 1) {
         ds <- matrix(1, nrow=nrow(output), ncol=nrow(coeff))
	 colnames(ds) <- rownames(coeff)
         for(v in colnames(output)) {
	    ds[,variable] <- output[,v]
            b <- coeff[,v,drop=FALSE]
	    output[,v] <- (ds %*% b)
        }
        return(output)
    }

    # Check covariates.
    covariates <- makematrix(covariates)
    if(is.character(covariates)) stop(covariates)
    
    # Prepare regressors.
    vars <- intersect(rownames(coeff), colnames(covariates))
    covariates <- covariates[,vars,drop=FALSE]
    coeff <- coeff[c(intercept,variable,vars),]
    ds <- data.frame(B0=rep(1, nrow(dat)), B1=NA, covariates)
    ds <- as.matrix(ds)

    # Calculate scores.
    for(v in colnames(output)) {
	ds[,"B1"] <- output[,v]
        b <- coeff[,v,drop=FALSE]
	output[,v] <- (ds %*% b)
    }
    return(output)
}
