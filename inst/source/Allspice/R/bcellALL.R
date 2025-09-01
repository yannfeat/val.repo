bcellALL <- function(
    n=200,
    contamination=0.05) {

    # Check input.
    if(!is.numeric(n)) stop("Unusable input.")
    if(!is.finite(n)) stop("Unusable input.")
    if(n < 1) stop("Unusable input.")

    # Check input.
    if(!is.numeric(contamination)) stop("Unusable input.")
    if(!is.finite(contamination)) stop("Unusable input.")
    if(contamination >= 1) stop("Unusable input.")

    # Import expression statistics.
    base <- system.file(package="Allspice")
    fn <- file.path(base, "simulation", "statistics.txt")
    stats <- read.delim(fn, stringsAsFactors=FALSE)
    rownames(stats) <- stats$VAR; stats$VAR <- NULL

    # Import subgroup centroids.
    base <- system.file(package="Allspice")
    fn <- file.path(base, "simulation", "centroids.txt")
    cents <- read.delim(fn, stringsAsFactors=FALSE)
    rownames(cents) <- cents$VAR; cents$VAR <- NULL

    # Import subgroup demographics.
    base <- system.file(package="Allspice")
    fn <- file.path(base, "simulation", "covariates.txt")
    covars <- read.delim(fn, stringsAsFactors=FALSE)
    rownames(covars) <- covars$VAR; covars$VAR <- NULL

    # Pick subtype labels.
    catnames <- sample(colnames(covars), size=n, prob=covars["N",],
        replace=(n > ncol(covars)))

    # Reference stats.
    mu0 <- log2(stats$MEDIAN + 1)
    sigma0 <- (log2(stats$Q691 + 1) - log2(stats$Q309 + 1))

    # Set up profile matrix.
    profs <- matrix(NA, nrow=nrow(cents), ncol=n)
    rownames(profs) <- rownames(cents)
    colnames(profs) <- paste0("patient", 1:n)
    
    # Simulate subtype profiles.
    for(k in 1:n) {
        mu <- log2(cents[,catnames[k]] + 1)
        x <- rnorm(length(mu), mean=mu, sd=0.5*sigma0)
	profs[,k] <- pmax(round(2^x - 1), 0)
    }

    # Trim extreme values.
    f <- sqrt(n)*sqrt(nrow(profs))
    q <- -quantile(as.vector(-profs), 1/f)
    for(j in 1:ncol(profs)) profs[,j] <- pmin(profs[,j], q)

    # Simulate covariates.
    meta <- data.frame(stringsAsFactors=FALSE,
        MALE=as.integer(runif(n) < covars["MALE",catnames]),
        AGE=as.integer(0.5*covars["AGE",catnames] + runif(n)*40),
	SUBTYPE=catnames)
    rownames(meta) <- colnames(profs)

    # Perturb a proportion of profiles.
    nfail <- floor(contamination*n)
    if(nfail > 0) {
        mask <- sample.int(n, nfail)
        for(k in mask) {
            profs[,k] <- sample(profs[,k])
            meta[k,"SUBTYPE"] <- "Contaminated"
        }
    }
    
    # Return results.
    output <- list()
    output$counts <- profs
    output$metadata <- meta
    return(output)
}
