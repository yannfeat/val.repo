statistics <- function(
    dat,
    bits) {

    # Make sure data are in a matrix.
    dat <- makematrix(dat)
    bits <- makematrix(bits)
    if(is.character(dat)) return(dat) 
    if(is.character(bits)) return(bits)

    # Calculate subgroup statistics.
    output <- list()
     for(v in colnames(bits)) {
        mask <- which(bits[,v] == 1)
	x <- dat[mask,,drop=FALSE]
        stats <- data.frame(stringsAsFactors=FALSE,
	    CATEG=rep(v, ncol(x)),
	    N=apply(is.finite(x), 2, sum),
	    MEAN=apply(x, 2, mean, na.rm=TRUE),
            SD=apply(x, 2, sd, na.rm=TRUE),
	    MIN=apply(x, 2, min, na.rm=TRUE),
            MAX=apply(x, 2, max, na.rm=TRUE))
        rownames(stats) <- colnames(dat)
        output[[v]] <- stats
    }

    # Calculate total statistics.
    totals <- data.frame(stringsAsFactors=FALSE,
	N=apply(is.finite(dat), 2, sum),
	MEAN=apply(dat, 2, mean, na.rm=TRUE),
        SD=apply(dat, 2, sd, na.rm=TRUE),
	MIN=apply(dat, 2, min, na.rm=TRUE),
        MAX=apply(dat, 2, max, na.rm=TRUE))
    rownames(totals) <- colnames(dat)

    # Return results.
    attr(output, "total") <- totals
    return(output)
}
