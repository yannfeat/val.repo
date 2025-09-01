# Class definition.
setClass("Classifier",
    representation(
	assets="list", # classification pipeline
        covardat="matrix", # covariates
        results="list"))

# Create classifier from disk asset.
classifier <- function(
    ...,
    verbose=TRUE) {

    # New Classifier object.
    output <- new("Classifier")
    output@covardat <- matrix(nrow=0, ncol=0)
    folders <- list(...)

    # Assets for B-cell acute lymphoblast leukemia.
    if(length(folders) < 1) {
        basepath <- system.file(package="Allspice")
        folders[["subtypes"]] <- file.path(basepath, "subtypes")
        folders[["drivers"]] <- file.path(basepath, "drivers")
        folders[["tissues"]] <- file.path(basepath, "tissues")
    }

    # Import assets.
    if(anyDuplicated(folders) > 0) stop("Duplicated inputs.")
    for(f in folders) {
        fpath <- as.character(f)
        if(!dir.exists(fpath)) stop("Folder does not exist.")
        output@assets[[fpath]] <- asset(fpath, verbose=verbose)
    }
    return(output)
}
