makematrix <- function(
    dat,
    finite=FALSE) {

    # Check if numeric.
    if(length(dat) < 1) return("Empty data.")
    if(is.logical(dat)) dat <- (dat + 0)
    if(is.vector(dat) && is.numeric(dat)) {
        dat <- as.matrix(dat)
	colnames(dat) <- "(empty)"
    }
    if(is.data.frame(dat)) {
        for(j in 1:ncol(dat))
	    suppressWarnings(dat[,j] <- as.numeric(dat[,j]))
        dat <- as.matrix(dat)
    }
    
    # Check if usable.
    if(!is.matrix(dat)) return("Unusable matrix.")
    if(!is.numeric(dat)) return("Non-numeric matrix.")
    if(ncol(dat)*nrow(dat) < 1) return("Empty matrix.")
    if(length(rownames(dat)) < 1) return("No row names.")
    if(length(colnames(dat)) < 1) return("No column names.")
    rownames(dat) <- as.character(rownames(dat))
    colnames(dat) <- as.character(colnames(dat))

    # Check for missing values.
    if(finite) {
        for(j in 1:ncol(dat)) {
            if(sum(is.finite(dat[,j])) < nrow(dat))
                return("Unusable values.")
        }
    }
    return(dat)
}
