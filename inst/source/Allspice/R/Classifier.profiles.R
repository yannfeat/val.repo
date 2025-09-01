# Set RNA profiles.
setGeneric("profiles<-", function(obj, value)
    standardGeneric("profiles<-"))
setMethod("profiles<-", "Classifier",
function(
    obj,
    value) {

    # Make sure data are in a matrix.
    dat <- makematrix(value)
    if(is.character(dat)) stop(dat)

    # Classify samples.
    ok <- TRUE
    for(a in names(obj@assets)) {
        ass <- obj@assets[[a]]

        # Set default covariates.
	covars <- NULL
	if(length(ass@demographics) > 0) {
	    stats <- ass@demographics
            covars <- matrix(NA, nrow=ncol(dat), ncol=nrow(stats))
	    rownames(covars) <- colnames(dat)
	    colnames(covars) <- rownames(stats)
            for(j in 1:ncol(covars))
	        covars[,j] <- stats$MEAN[j] 
            
            # Replace default covariates with available values.
            ntotal <- 0
            keys <- intersect(rownames(obj@covardat), rownames(covars))
            vars <- intersect(colnames(obj@covardat), colnames(covars))
            for(v in vars) {
                x <- obj@covardat[keys,v]
                mask <- which(is.finite(x))
                covars[keys[mask],v] <- x[mask]
                ntotal <- (ntotal + length(mask))
            }
            
            # Check if all values were available.
	    if(ntotal < nrow(covars)*ncol(covars)) ok <- FALSE
        }

        # Assign categories.
        obj@results[[a]] <- classify(ass, dat=dat, covariates=covars)
    }

    # Check for incomplete covariates.
    if(!ok) warning("Classifier.profiles(): Incomplete covariates.",
        call.=FALSE, immediate.=TRUE)
    return(obj)
})
