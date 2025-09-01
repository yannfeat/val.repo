# Return useful information about assets.
setGeneric("information", function(obj)
    standardGeneric("information"))
setMethod("information", "Classifier",
function(obj) {

    # Check if any contents.
    if(length(obj@assets) < 1) stop("Empty classifier.")

    # Collect modelled covariates.
    assnames <- names(obj@assets)
    covars <- list(ASSET=integer(), TITLE=character(),
        COVAR=character(), SOURCE=character())
    for(k in 1:length(assnames)) {
       a <- obj@assets[[k]]
       if(length(a@demographics) > 0) {
           beta <- a@coefficients[["prox"]]
           vars <- setdiff(rownames(beta), c("B0","B1"))
 	   covars$ASSET <- c(covars$ASSET, rep(k, length(vars)))
 	   covars$TITLE <- c(covars$TITLE, rep(a@title, length(vars)))
 	   covars$COVAR <- c(covars$COVAR, vars)
	   covars$SOURCE <- c(covars$SOURCE, rep(assnames[k], length(vars)))
       }
       else {
 	   covars$ASSET <- c(covars$ASSET, k)
 	   covars$TITLE <- c(covars$TITLE, a@title)
 	   covars$COVAR <- c(covars$COVAR, NA)
	   covars$SOURCE <- c(covars$SOURCE, assnames[k])
       }
    }
    covars <- data.frame(covars, stringsAsFactors=FALSE)

    # Collect configuration parameters.
    config <- NULL; titles <- character()
    for(k in 1:length(assnames)) {
       a <- obj@assets[[k]]
       if(length(config) < 1) {
           config <- a@parameters
       }
       else {
           config <- rbind(config, a@parameters)
	   colnames(config) <- names(a@parameters)
       }
       titles[k] <- a@title
    }
    config <- data.frame(ASSET=(1:length(assnames)),
        TITLE=titles, config, SOURCE=assnames,
	stringsAsFactors=FALSE)
    rownames(config) <- NULL

    # Collect category labels.
    categ <- NULL
    for(k in 1:length(assnames)) {
        a <- obj@assets[[k]]
	dat <- a@categories
        dat <- data.frame(stringsAsFactors=FALSE,
	    ASSET=rep(k, nrow(dat)),
	    TITLE=rep(a@title, nrow(dat)),
	    CATEG=rownames(a@categories), a@categories,
	    SOURCE=rep(assnames[k], nrow(dat)))
        rownames(dat) <- NULL
        if(length(categ) < 1) {
	    categ <- dat
	}
	else {
	    vars <- intersect(colnames(categ), colnames(dat))
	    categ <- rbind(categ[,vars,drop=F], dat[,vars,drop=F])
	}
    }

    # Return results.
    output <- list()
    output$covariates <- covars
    output$configuration <- config
    output$categories <- categ
    return(output)
})
