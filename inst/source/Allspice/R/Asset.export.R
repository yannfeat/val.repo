# Save resources to disk.
setGeneric("export", function(obj, folder)
    standardGeneric("export"))
setMethod("export", "Asset",
function(
    obj,
    folder) {
    output <- character()

    # Check if ready.
    if(length(obj@centroids) < 1) stop("Incomplete asset.")

    # Check folder name.
    if(!is.character(folder)) stop("Unusable folder name.")
    if(!dir.exists(folder)) dir.create(folder[1])
    if(!dir.exists(folder)) stop("Folder cannot be used.")

    # Save configuration.
    fn <- file.path(folder, "configuration.txt")
    dat <- data.frame(PARAM=c("title", names(obj@parameters)),
        VALUE=c(obj@title, obj@parameters), stringsAsFactors=FALSE)
    write.table(dat, file=fn, quote=FALSE, row.names=FALSE, sep="\t")
    output[length(output)+1] <- fn

    # Save visuals.
    fn <- file.path(folder, "categories.txt")
    dat <- data.frame(CATEG=rownames(obj@categories),
        obj@categories, stringsAsFactors=FALSE)
    write.table(dat, file=fn, quote=FALSE, row.names=FALSE, sep="\t")
    output[length(output)+1] <- fn

    # Save data statistics.
    fn <- file.path(folder, "reference.txt")
    dat <- data.frame(VAR=rownames(obj@reference),
        obj@reference, stringsAsFactors=FALSE)
    write.table(dat, file=fn, quote=FALSE,
        row.names=FALSE, sep="\t")
    output[length(output)+1] <- fn

    # Save covariate statistics.
    if(length(obj@demographics) > 0) {
        fn <- file.path(folder, "covariates.txt")
	dat <- obj@demographics
	dat <- data.frame(VAR=rownames(dat), dat)
        write.table(dat, file=fn, quote=FALSE,
            row.names=FALSE, sep="\t")
        output[length(output)+1] <- fn
    }

    # Save nomenclature if available.
    if(length(obj@naming) > 0) {
        fn <- file.path(folder, "nomenclature.txt")
        dat <- data.frame(VAR=rownames(obj@naming),
            obj@naming, stringsAsFactors=FALSE)
        write.table(dat, file=fn, quote=FALSE,
            row.names=FALSE, sep="\t")
        output[length(output)+1] <- fn
    }

    # Save centroids.
    fn <- file.path(folder, "centroids.txt")
    dat <- data.frame(VAR=rownames(obj@centroids), obj@centroids,
        check.names=FALSE, stringsAsFactors=FALSE)
    write.table(dat, file=fn, quote=FALSE, row.names=FALSE, sep="\t")
    output[length(output)+1] <- fn

    # Save regression coefficients.
    fn <- file.path(folder, "coefficients.txt")
    prox <- obj@coefficients[["prox"]]
    prox <- data.frame(CATEG=colnames(prox), MODEL="prox",
        t(prox), stringsAsFactors=F)
    freq <- obj@coefficients[["freq"]]
    freq <- data.frame(CATEG=colnames(freq), MODEL="freq",
        t(freq), stringsAsFactors=F)
    write.table(rbind(prox, freq), file=fn, quote=FALSE,
        row.names=FALSE, sep="\t")
    output[length(output)+1] <- fn
    return(output)
})
