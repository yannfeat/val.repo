translate <- function(
    dat,
    naming) {

    # Check if anything to do.
    output <- rownames(dat)
    if(length(naming) < 1) return(output)

    # Translate row names.
    symbols <- rownames(naming)
    for(v in colnames(naming)) {

        # Find matching keys.
        pos <- match(rownames(dat), naming[,v])
        mask <- which(pos > 0)
        if(length(mask) < 1) next

        # Check if any changes.
        keys <- rownames(dat)
        keys[mask] <- symbols[pos[mask]]
        if(sum(keys != rownames(dat)) < 1) next

        # Update output.
	output[mask] <- keys[mask]
    }
    return(output)
}