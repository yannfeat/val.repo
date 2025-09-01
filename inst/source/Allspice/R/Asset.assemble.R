# Train a classification asset.
setGeneric("assemble<-", function(obj, value)
    standardGeneric("assemble<-"))
setMethod("assemble<-", "Asset",
function(
    obj,
    value) {

    # Check if locked.
    if(length(obj@centroids) > 0)
        stop("Asset is already assembled.")

    # Set title.
    if(length(value$title) < 1) stop("No title.")
    if(!is.character(value$title)) stop("Unusable title.")
    if(nchar(value$title) < 1) stop("Empty title.")
    if(nchar(value$title) > 16) stop("Title is over 16 characters.")
    obj@title <- as.character(value$title[1])
    
    # Check data.
    if(length(value$dat) < 1) stop("No data.")
    dat <- value$dat; value$dat <- NULL # reduce memory footprint
    dat <- makematrix(dat, finite=TRUE)
    if(is.character(dat)) {
        warning("Problem with input data.", call.=F, immediate.=T)
        stop(dat)
    }

    # Select final training features.
    predictors <- value$predictors
    if(length(predictors) > 0) {
        predictors <- intersect(predictors, rownames(dat))
        if(length(predictors) < length(value$predictors))
           warning("Missing predictors.", immediate.=T)
        if(length(predictors) < 3) stop("Too few predictors.")
    }

    # Check bitmap.
    if(length(value$bits) < 1) stop("No category bits.")
    bits <- make_bits(value$bits)
    if(is.character(bits)) {
        warning("Problem with category bits.", call.=F, immediate.=T)
        stop(bits)
    }
    
    # Check reserved variable names.
    reserved <- c("Ambiguous","Unclassified","")
    cnames <- setdiff(colnames(bits), reserved)
    if(length(cnames) < 1) stop("No usable category labels.")
    if(length(cnames) < ncol(bits)) {
        reserved <- intersect(reserved, colnames(bits))
	for(m in reserved) {
	    msg <- paste0("Category label excluded: '", m, "'.")
	    warning(msg, call.=F, immediate.=T)
        }
        bits <- bits[,cnames,drop=FALSE]
    }
    
    # Check covariates.
    covars <- value$covariates
    if(length(covars) > 0) {
        covars <- makematrix(covars, finite=TRUE)
        if(is.character(covars)) {
            warning("Problem with covariates.", call.=F, immediate.=T)
            stop(covars)
        }

        # Check reserved variable names.
        reserved <- c("CATEG", "")    
        if(anyDuplicated(colnames(covars)) > 0)
            stop("Duplicated covariates.")
        if(anyDuplicated(c(colnames(covars), reserved)) > 0)
            stop("Reserved covariate names.")
    }

    # Check sample identities.
    keys <- intersect(colnames(dat), rownames(bits))
    if(length(covars) > 0) keys <- intersect(keys, rownames(covars))
    if(length(keys) < ncol(dat)) {
        msg <- paste0("Asset.assemble(): ", length(keys),
            " / ", ncol(dat), " samples included.")
        warning(msg, call.=FALSE, immediate.=TRUE)
        if(length(keys) < 10) stop("Too few training samples.")
    }
    dat <- dat[,keys,drop=FALSE]
    bits <- bits[keys,,drop=FALSE]
    if(length(covars) > 0) covars <- covars[keys,,drop=FALSE]

    # Check visuals.
    if(length(obj@categories) < 1) visuals(obj) <- colnames(bits)
    categ <- c(colnames(bits), "Unclassified", "Ambiguous")
    vars <- intersect(categ, rownames(obj@categories))
    if(length(vars) < length(categ)) {
        missed <- setdiff(categ, rownames(obj@categories))
	for(m in missed) {
	    msg <- paste0("Missing visuals: ", m, ".")
	    warning(msg, call.=F, immediate.=T)
	}
        stop("Incompatible visuals.")
    }
    obj@categories <- obj@categories[vars,,drop=FALSE]

    # Apply nomenclature.
    vars <- translate(dat, obj@naming)
    if(anyDuplicated(vars) > 0)
       stop("Nomenclature caused duplicated row names.")
    if(length(predictors) > 0)
        predictors <- vars[match(predictors, rownames(dat))]
    rownames(dat) <- vars

    # Population reference.
    ref <- reference(dat=dat, cfg=obj@parameters)
    if(is.character(ref)) stop(ref)
    obj@reference <- ref

    # Trim nomenclature.
    if(length(obj@naming) > 0) {
        keys <- intersect(rownames(ref), rownames(obj@naming))
        obj@naming <- obj@naming[keys,,drop=F]
    }

    # Population covariates.
    if(length(covars) > 0) {
        obj@demographics <- data.frame(
	    N=apply(is.finite(covars), 2, sum),
	    MEAN=apply(covars, 2, mean, na.rm=TRUE),
	    SD=apply(covars, 2, sd, na.rm=TRUE))
    }

    # Normalize and standardize training data.
    dat <- normalize(obj, dat=dat); gc()
    dat <- standardize(obj, dat=dat); gc()

    # Exclude redundant variables.
    if(length(predictors) > 0) {
        dat <- dat[predictors,,drop=FALSE]
    }
    else {
        dat <- prune(dat=dat, bits=bits,
            nmax=obj@parameters["ninput.max"],
            rrmax=obj@parameters["rrinput.max"])
        if(is.character(dat)) stop(dat)
    }

    # Summarize subset patterns.
    cents <- summarize(dat=dat, bits=bits)
    if(is.character(cents)) stop(cents)
    if(ncol(cents) < ncol(bits)) {
        msg <- paste0("Asset.assemble(): ", ncol(cents),
            " / ", ncol(bits), " categories included.")
        warning(msg, call.=FALSE, immediate.=TRUE)
    }
    obj@centroids <- cents

    # Distances to centroids.
    delta <- distances(dat=dat, centroids=obj@centroids)
    if(is.character(delta)) stop(delta)

    # Regression models of proximity.
    beta <- analyze(dat=delta, covariates=covars, bits=bits, balance=TRUE)
    if(is.character(beta)) stop(beta)
    obj@coefficients[["prox"]] <- beta

    # Regression models of frequency.
    beta <- analyze(dat=delta, covariates=covars, bits=bits, balance=FALSE)
    if(is.character(beta)) stop(beta)
    obj@coefficients[["freq"]] <- beta
    return(obj)
})

#-------------------------------------------------------------------------

# Prepare category data.
make_bits <- function(dat) {
    output <- NULL

    # Check if any data.
    if(length(dat) < 1) return("Empty input.")

    # Convert a vector into a matrix.
    if(!is.matrix(dat) && !is.data.frame(dat)) {
         keys <- names(dat)
         if(length(keys) < 1) return("No identities for category bits.")
         dat <- data.frame(X=as.vector(dat), stringsAsFactors=FALSE)
         rownames(dat) <- keys
    }

    # Split into binary columns.
    output <- NULL
    if(ncol(dat) == 1) {
        keys <- rownames(dat)
        if(length(keys) < 1) return("No identities for category bits.")
        dat <- as.factor(dat[,1])
        levs <- as.character(levels(dat))
        if(sum(is.na(levs)) > 0) return("Unusable category name.")
        if(sum(nchar(levs) < 1) > 0) return("Empty category name.")
        dat <- as.integer(dat)
        output <- matrix(0, nrow=length(keys), ncol=length(levs))
        rownames(output) <- as.character(keys)
        colnames(output) <- as.character(levs)
        for(j in 1:ncol(output))
            output[,j] <- as.integer(dat == j)
    }
    else {
        output <- dat
        for(j in 1:ncol(output))
            output[,j] <- as.integer(dat[,j] != 0)
    }
    return(makematrix(output, finite=TRUE))
}
