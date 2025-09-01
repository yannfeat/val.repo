# Samples in columns, variables in rows.
setGeneric("normalize", function(obj, dat)
    standardGeneric("normalize"))
setMethod("normalize", "Asset",
function(
    obj,
    dat) {

    # Make sure data are in a matrix.
    output <- makematrix(dat)
    if(is.character(output)) stop(output)

    # Check if anything to do.
    if(obj@parameters["norm"] == 0) return(output)

    # Reference profile.
    stats <- obj@reference
    if(length(stats) < 1) stop("No population reference.")
    if(length(stats$VALUE) < 1) stop("No reference profile.")
    refvals <- as.double(obj@reference$VALUE)
    names(refvals) <- rownames(obj@reference)

    # Nomenclature conversion.
    if(length(obj@naming) > 0) {
        keys <- translate_names(dat=output, naming=obj@naming)
        ndiff <- sum(keys != rownames(output))
	if(ndiff > 0) {
            rownames(output) <- keys
	    if(ndiff < nrow(output)) {
                msg <- paste0("Asset.normalize(): ", ndiff, " / ",
                    nrow(output), " variables renamed.")
                warning(msg, call.=FALSE, immediate.=TRUE)
	    }
        }
    }

    # Select variables for normalization.
    alerts <- character()
    vars <- intersect(names(refvals), rownames(output))
    if(length(vars) < 1) stop("Incompatible data.")
    if(length(vars) < length(refvals)) {

        # Expand data matrix to include more variables.
        extra <- setdiff(names(refvals), rownames(output))
        newdat <- rep(refvals[extra], times=ncol(output))
        newdat <- matrix(newdat, nrow=length(extra), ncol=ncol(output))
        rownames(newdat) <- extra
        colnames(newdat) <- colnames(output)
        output <- rbind(output, newdat); newdat <- NULL; gc()

        # Warning message.
        msg <- paste0("Asset.normalize(): ", length(extra), " / ",
            length(refvals), " variables imputed from reference.")
        warning(msg, call.=FALSE, immediate.=TRUE)
    }
    output <- output[names(refvals),,drop=FALSE]

    # Median ratio to pseudo-reference for each sample.
    med <- apply(output, 2, "/", refvals)
    med <- apply(med, 2, median)
    med <- pmax(med, 1e-6)

    # Remove unusable samples.
    mask <- which(!is.finite(med))
    if(length(mask) > 0) {
        msg <- paste0("Asset.normalize(): ", length(mask), " / ",
            length(med), " unusable samples excluded.")
        output <- output[,-mask,drop=FALSE]
        med <- med[-mask]
	warning(msg, call.=FALSE, immediate.=TRUE)
    }

    # Normalize by median ratio (DESeq2).
    for(k in 1:ncol(output))
        output[,k] <- (output[,k])/(med[k])
    return(output)
})

#---------------------------------------------------------------------------

translate_names <- function(
    dat,
    naming) {   
    output <- rownames(dat)
    symbols <- rownames(naming)
    for(v in colnames(naming)) {

        # Find matching keys.
        pos <- match(rownames(dat), naming[,v])
        mask <- which(pos > 0)
        if(length(mask) < 1) next

        # Check if any changes.
        keys <- rownames(dat)
        keys[mask] <- symbols[pos[mask]]
        if(sum(keys != rownames(dat)) < 1) next

        # Update output.
	output[mask] <- keys[mask]
    }
    return(output)
}
