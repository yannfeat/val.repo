# Samples in columns, variables in rows.
distances <- function(
    dat,
    centroids,
    closest=FALSE) {
    
    # Select inputs.
    vars <- intersect(rownames(centroids), rownames(dat))
    if(length(vars) < nrow(centroids)) return("Incompatible data.")
    else dat <- dat[rownames(centroids),,drop=FALSE]

   # Calculate Euclidean distances.
    delta <- apply(dat, 2, function(x, centroids) {
        d <- colMeans((centroids - x)^2)
	return(sqrt(d))
    }, centroids=centroids)
    colnames(delta) <- colnames(dat)

    # Return the closest matches.
    if(closest) {
        ind <- apply(delta, 2, which.min)
        delta <- apply(delta, 2, min)
        output <- data.frame(MATCH=colnames(centroids)[ind],
	    D=delta, stringsAsFactors=FALSE)
        rownames(output) <- colnames(dat)
        return(output)
    }

    # Return all distances.
    return(t(delta))
}
