# Samples in columns, variables in rows.
prune <- function(
    dat,
    bits,
    nmax,
    rrmax) {

    # Make sure data are in a matrix.
    dat <- makematrix(dat)
    if(is.character(dat)) return(dat)

    # Make sure bitmap is a matrix.
    bits <- makematrix(bits)
    if(is.character(bits)) return(bits)
    
    # Check inputs.
    keys <- intersect(colnames(dat), rownames(bits))
    if(length(keys) < ncol(dat)) return("Incompatible bits.")
    bits <- bits[colnames(dat),,drop=FALSE]
    nmax <- min(c(nmax, nrow(dat)))

    # Check if anything to do.
    if(nrow(dat) <= nmax) return(dat)

    # Welch t statistic converted into Z-scores.
    z <- matrix(0, nrow=nrow(dat), ncol=ncol(bits))
    rownames(z) <- rownames(dat)
    colnames(z) <- colnames(bits)
    for(v in colnames(bits)) {
        contr <- which(bits[,v] == 0)
        cases <- which(bits[,v] == 1)
        n0 <- length(contr)
        n1 <- length(cases)
        if((n1 >= 2) && (n0 >= 2)) {
            x0 <- dat[,contr]
            x1 <- dat[,cases]
            mu0 <- rowMeans(x0)
            mu1 <- rowMeans(x1)
            sigma0 <- rowMeans(x0^2)
            sigma1 <- rowMeans(x1^2)
            sigma0 <- (sigma0 - mu0*mu0)
            sigma1 <- (sigma1 - mu1*mu1)
            sigma0 <- pmax(sigma0, 1e-9)
            sigma1 <- pmax(sigma1, 1e-9)
            sq0 <- sigma0*sigma0/n0
            sq1 <- sigma1*sigma1/n1
            x <- (mu1 - mu0)/sqrt(sq1 + sq0)
            p <- 2*pt(q=-abs(x), df=n1)
            p <- pmin(p, (1 - 1e-9), na.rm=TRUE)
            p <- pmax(p, 1e-99, na.rm=TRUE)
            z[,v] <- sign(x)*abs(qnorm(p=p))
	}
    }

    # Standardize to remove differences in category sizes.
    for(j in 1:ncol(z)) {
        mu <- mean(z[,j], na.rm=TRUE)
	sigma <- sd(z[,j], na.rm=TRUE)
	z[,j] <- (z[,j] - mu)/(sigma + 1e-9)
    }

    # Aggregate scores.
    scores <- apply(z, 1, sd, na.rm=TRUE)
    scores <- sort(scores, decreasing=TRUE)

    # Select non-redundant variables.
    starter <- names(scores[1])
    output <- matrix(dat[starter,], nrow=ncol(dat), ncol=1)
    rownames(output) <- colnames(dat)
    colnames(output) <- starter
    for(g in names(scores)) {
        if(ncol(output) >= nmax) break
	cc <- 0 # looping is slow but stops early
        for(j in 1:ncol(output)) {
	    cc <- cor(output[,j], dat[g,])
	    if(cc^2 >= rrmax) break
        }
        if(cc^2 < rrmax) {
            cnames <- colnames(output)
            output <- cbind(output, dat[g,])
            colnames(output) <- c(cnames, g)
	}
    }

    # Return results.
    output <- t(output)
    attr(output, "scores") <- scores
    return(output)
}
