reference <- function(
    dat,
    cfg) {

    # Make sure data are in a matrix.
    dat <- makematrix(dat)
    if(is.character(dat)) return(dat) 
    
    # Frequencies of non-zero values per variable across samples.
    fcutoff <- (cfg["nonzero.ratio"])*ncol(dat)
    freq <- apply((dat >= cfg["nonzero.min"]), 1, sum, na.rm=TRUE)

    # Remove variables with low or sparse expression.
    dat <- dat[which(freq >= fcutoff),,drop=FALSE]

    # Apply logarithm.
    if(cfg["logarithm"]) dat <- log2(dat + 1)
    
    # Mean for each variable across samples.
    refvals <- rowMeans(dat, na.rm=TRUE)
    names(refvals) <- rownames(dat)

    # Undo logarithm for reference values.
    if(cfg["logarithm"]) refvals <- (2^refvals - 1)

    # Robust descriptive statistics.
    mu <- apply(dat, 1, mean, trim=0.01)
    mom2 <- apply(dat^2, 1, mean, trim=0.01)
    sigma <- sqrt(pmax((mom2 - mu^2), 0))

    # Collect statistics.
    output <- data.frame(VALUE=refvals, MEAN=mu,
        SD=sigma, stringsAsFactors=FALSE)
    rownames(output) <- rownames(dat)

    # Exclude unsusable variables.
    mask <- which(output$SD > 0)
    if(length(mask) < nrow(output))
        output <- output[mask,,drop=FALSE]
    return(output)
}
