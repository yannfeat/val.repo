summarize <- function(
    dat,
    bits) {

    # Make sure data are in a matrix.
    dat <- makematrix(dat)
    if(is.character(dat)) return(dat)
    
    # Make sure bitmap is a matrix.
    bits <- makematrix(bits)
    if(is.character(bits)) return(bits)

    # Check reserved categories.
    if(anyDuplicated(colnames(bits)) > 0)
        return("Duplicated category labels.")
    if(anyDuplicated(c(colnames(bits), "Unclassified")) > 0)
        return("Category cannot be named 'Unclassified'.")
    if(anyDuplicated(c(colnames(bits), "Ambiguous")) > 0)
        return("Category cannot be named 'Ambiguous'.")
    if(anyDuplicated(c(colnames(bits), "VAR")) > 0)
        return("Category cannot be named 'VAR'.")

    # Exclude unusable categories.
    freq <- colSums(bits == 1)
    cols <- which(freq >= 2)
    if(length(cols) < 1) return("Unusable category data.")
    bits <- bits[,cols,drop=FALSE]

    # Calculate subset centroids.
    output <- matrix(NA, nrow=nrow(dat), ncol=ncol(bits))
    rownames(output) <- rownames(dat)
    colnames(output) <- colnames(bits)
    for(cname in colnames(bits)) {
        mask <- which(bits[,cname] == 1)
        output[,cname] <- rowMeans(dat[,mask,drop=FALSE])
    }
    return(output)
}
