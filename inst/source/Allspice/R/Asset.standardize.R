# Samples in columns, variables in rows.
setGeneric("standardize", function(obj, dat, trim=FALSE)
    standardGeneric("standardize"))
setMethod("standardize", "Asset",
function(
    obj,
    dat,
    trim=FALSE) {

    # Make sure data are in a matrix.
    output <- makematrix(dat)
    if(is.character(output)) stop(output)

    # Check if anything to do.
    if(obj@parameters["standard"] == 0) return(output)

    # Reference profile.
    stats <- obj@reference
    if(length(stats) < 1) stop("No population reference.")
    if(length(stats$MEAN) < 1) stop("No population statistics.")
    if(length(stats$SD) < 1) stop("No population statistics.")

    # Select variables.
    vars <- intersect(rownames(stats), rownames(output))
    if(trim && (length(obj@centroids) > 0))
        vars <- intersect(rownames(obj@centroids), vars)
    if(length(vars) < 1) stop("Incompatible data.")
    output <- output[vars,,drop=FALSE]
    stats <- stats[vars,,drop=FALSE]

    # Logarithm.
    if(obj@parameters["logarithm"]) output <- log2(output + 1)

    # Impute missing values.
    nimput <- 0
    for(k in 1:ncol(output)) {
        mask <- which(!is.finite(output[,k]))
	if(length(mask) > 0) output[mask,k] <- stats$MEAN[mask]
	nimput <- (nimput + length(mask))
    }
    if(nimput > 0) {
        msg <- paste0("Asset.standardize(): ", nimput,
            " / ", nrow(output)*ncol(output), " values imputed.")
        warning(msg, call.=FALSE, immediate.=TRUE)
    }
    
    # Center and scale with tapering by t-distribution.
    if(obj@parameters["standard"]) {
        e <- .Machine$double.eps
        for(k in 1:ncol(output)) {
            x <- (output[,k] - stats$MEAN)/(stats$SD + e)
	    x <- pmax(pt(x, df=20), e)
	    x <- pmin(x, (1 - e))
            output[,k] <- 1.047*qnorm(x)
        }
    }

    # Check variances.
    n <- sum(apply(output, 1, sd) > 0)
    if(n < nrow(output)) {
        msg <- paste0("Asset.standardize(): ", n,
            " / ", nrow(output), " usable variables.")
        warning(msg, call.=FALSE, immediate.=TRUE)
    }
    return(output)
})