# Set covariates.
setGeneric("covariates<-", function(obj, value)
    standardGeneric("covariates<-"))
setMethod("covariates<-", "Classifier",
function(
    obj,
    value) {

    # Check input.
    dat <- makematrix(value)
    if(is.character(dat)) stop(dat)

    # Check that row and column names are distinct.
    ovl <- intersect(rownames(dat), colnames(dat))
    if(length(ovl) > 0) stop("Row and column names overlap.")

    # Collect modelled covariates.
    vars <- character()
    for(a in obj@assets) {
       if(length(a@demographics) > 0) {
           prox <- a@coefficients[["prox"]]
           vars <- c(vars, rownames(prox))
       }
    }

    # Select columns.
    if(length(vars) < 1) {
        msg <- paste0("Classifier.covariates(): No covariates modelled.")
        warning(msg, call.=FALSE, immediate.=TRUE)
    }
    else {

        # Check if covariates need to be transposed.
        cnames <- intersect(vars, colnames(dat))
        if(length(cnames) < 1) dat <- t(dat)

        # Update object.
        vars <- intersect(vars, colnames(dat))
        if(length(vars) < 1) stop("Incompatible covariates.")
        obj@covardat <- dat[,vars,drop=FALSE]
    }
    return(obj)
})
