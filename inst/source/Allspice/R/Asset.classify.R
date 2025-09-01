# Samples as columns, genes as rows.
setGeneric("classify", function(obj, dat, covariates)
    standardGeneric("classify"))
setMethod("classify", "Asset",
function(
    obj,
    dat,
    covariates) {
    
    # Check if ready.
    if(length(obj@centroids) < 1)
        stop("Asset is not assembled.")

    # Make sure data are in a matrix.
    dat <- makematrix(dat)
    if(is.character(dat)) stop(dat)

    # Normalize and standardize profiles.
    dat <- normalize(obj, dat=dat)
    dat <- standardize(obj, dat=dat, trim=TRUE)

    # Select covariates.
    if(length(obj@demographics) > 0) {

        # Make sure covariates are in a matrix.
        covariates <- makematrix(covariates)
        if(is.character(covariates)) stop(covariates)

        # Check compatibility.
        keys <- intersect(colnames(dat), rownames(covariates))
        if(length(keys) < ncol(dat)) stop("Incompatible covariates.")
        covariates <- covariates[colnames(dat),,drop=FALSE]
    
        # Select variables.
	prox <- obj@coefficients[["prox"]]
        vars <- intersect(rownames(prox), colnames(covariates))
        vars <- setdiff(vars, c("B0","B1","MODEL"))
        if((length(vars) + 3) < nrow(prox))
            stop("Insufficient covariates.")
        covariates <- covariates[,vars,drop=FALSE]
    }

    # Distances to centroids.
    delta <- distances(dat=dat, centroids=obj@centroids)
    if(is.character(delta)) stop(delta)

    # Standardized category scores.
    biomrk <- regress(dat=delta, covariates=covariates,
	coeff=obj@coefficients[["prox"]])
    fscores <- regress(dat=delta, covariates=covariates,
	coeff=obj@coefficients[["freq"]])

    # Include only within-group best matches.
    dscores <- biomrk
    if(length(obj@categories$GROUP) > 0) {
        res <- merge_groups(pdat=dscores, fdat=fscores,
	    grouping=obj@categories)
        dscores <- res$pdat
        fscores <- res$fdat
    }

    # Find best matches between groups.
    topscor <- matrix(0, nrow=nrow(dscores), ncol=4)
    matched <- matrix(0, nrow=nrow(dscores), ncol=2)
    for(k in 1:nrow(dscores)) {
        sorted <- order(dscores[k,], decreasing=TRUE)
	topscor[k,1:2] <- dscores[k,sorted[1:2]]
	topscor[k,3:4] <- fscores[k,sorted[1:2]]
	matched[k,] <- sorted[1:2]
    }

    # Calculate quality metrics.
    x <- (topscor[,1] - topscor[,2])
    prox <- cbind(pnorm(topscor[,1]), pnorm(topscor[,2]))
    freq <- cbind(pnorm(topscor[,3]), pnorm(topscor[,4]))
    excl <- pchisq((x > 0)*(x^2), df=1)

    # Convert to data frame.
    categ <- colnames(fscores)
    output <- data.frame(CATEG=categ[matched[,1]],
        MATCH=categ[matched[,1]], MATCH.2nd=categ[matched[,2]],
	FREQ=freq[,1], FREQ.2nd=freq[,2],
	PROX=prox[,1], PROX.2nd=prox[,2],
	EXCL=excl, stringsAsFactors=FALSE)
    rownames(output) <- rownames(fscores)

    # Check proximity.
    prox.min <- obj@parameters["proximity.min"]
    unclass <- which(output$PROX < prox.min)
    output[unclass,"CATEG"] <- "Unclassified"

    # Check exclusivity.
    excl.min <- obj@parameters["exclusivity.min"]
    ambig <- setdiff(which(output$EXCL < excl.min), unclass)
    output[ambig,"CATEG"] <- "Ambiguous"

    # Add visual parameters.
    output$LABEL <- obj@categories[output$CATEG,"LABEL"]
    output$COLOR <- obj@categories[output$CATEG,"COLOR"]
    output$COLOR.light <- obj@categories[output$CATEG,"COLOR.light"]
    output$COLOR.dark <- obj@categories[output$CATEG,"COLOR.dark"]

    # Return results.
    attr(output, "biomarkers") <- biomrk
    attr(output, "biomarkers.grouped") <- dscores
    return(output)
})

# Merge category scores into superscores.
merge_groups <- function(
    pdat,
    fdat,
    grouping) {

    # Put member categories together.
    grp <- grouping[colnames(pdat),"GROUP"]
    grp <- split(x=colnames(pdat), f=grp)
    nums <- sapply(grp, length)

    # Merge scores within supergroups.
    output <- fdat
    for(g in grp[which(nums > 1)]) {
        p <- pdat[,g,drop=FALSE]
        f <- fdat[,g,drop=FALSE]
	w <- length(g)/ncol(pdat)
	for(k in 1:nrow(p)) {
            ind <- which.max(p[k,])
            p[k,-ind] <- NA
	}
        pdat[,g] <- p
        fdat[,g] <- ((1 - w)*f + w*p)
    }

    # Return result.
    output <- list()
    output$pdat <- pdat
    output$fdat <- fdat
    return(output)
}
