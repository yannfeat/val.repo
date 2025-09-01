# Class definition.
setClass("Asset",
    representation(
        title="character",
	parameters="numeric", # processing and other parameters
	reference="data.frame", # population features
	demographics="data.frame", # population covariates
	naming="data.frame", # conversions for variable names
	categories="data.frame", # category attributes
	centroids="matrix", # classification model
	coefficients="list")) # regression models

# Import asset from disk.
asset <- function(
    folder=NULL,
    verbose=TRUE) {

    # Check if anything to do.
    output <- new("Asset"); configuration(output) <- NULL
    if(length(folder) < 1) return(output)

    # Check folder.
    if(!dir.exists(folder)) stop("Folder does not exist.")

    # Asset identifier.
    dat <- import_data(folder, "configuration", verbose=F)
    rownames(dat) <- dat$PARAM
    output@title <- dat["title","VALUE"]
    if(verbose) {
        cat("\n", output@title, "\n", sep="")
	cat(attr(dat, "message"))
    }

    # Configuration parameters.
    dat <- dat[setdiff(rownames(dat),"title"),,drop=FALSE]
    output@parameters <- as.double(dat$VALUE)
    names(output@parameters) <- dat$PARAM

    # Visual attributes.
    dat <- import_data(folder, "categories", verbose=verbose)
    rownames(dat) <- dat$CATEG; dat$CATEG <- NULL

    # Check if duplicated labels.
    if(anyDuplicated(dat$LABEL) > 0) {
        f <- as.factor(dat$LABEL)
	if(length(levels(f)) < 2) stop("Less than two category groups.")
        dat$GROUP <- as.integer(f)
    }
    output@categories <- dat

    # Population features.
    dat <- import_data(folder, "reference", verbose=verbose)
    rownames(dat) <- dat$VAR; dat$VAR <- NULL
    output@reference <- dat

    # Population covariates.
    dat <- import_data(folder, "covariates", verbose=verbose)
    if(length(dat) > 0) {
	rownames(dat) <- dat$VAR; dat$VAR <- NULL
	output@demographics <- dat
    }

    # Variable name conversions.
    dat <- import_data(folder, "nomenclature", verbose=verbose)
    if(length(dat) > 0) {
        rownames(dat) <- dat$VAR; dat$VAR <- NULL
	output@naming <- dat
    }

    # Centroids.
    dat <- import_data(folder, "centroids", verbose=verbose)
    rownames(dat) <- dat$VAR; dat$VAR <- NULL
    output@centroids <- as.matrix(dat)

    # Regression models.
    dat <- import_data(folder, "coefficients", verbose=verbose)
    models <- split(x=dat, f=dat$MODEL)
    output@coefficients <- lapply(models, function(x) {
        rownames(x) <- x$CATEG; x$CATEG <- NULL; x$MODEL <- NULL
	return(t(as.matrix(x)))
    })
    return(output)
}

# Import asset component from file.
import_data <- function(
    folder,
    fname,
    verbose) {
    output <- NULL
    fpath <- file.path(folder, paste0(fname, ".txt"))
    if(file.exists(fpath)) {
        output <- read.delim(file=fpath, sep="\t",
	    stringsAsFactors=FALSE, check.names=FALSE)
	nbytes <- prettyNum(file.size(fpath), big.mark=",")
	msg <- paste0("    ", fname, " -> ", nbytes, " bytes\n")
	if(verbose) cat(msg)
        attr(output, "message") <- msg
    }
    return(output)
}
