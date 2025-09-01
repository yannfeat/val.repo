# Set internal parameters.
setGeneric("configuration<-", function(obj, value)
    standardGeneric("configuration<-"))
setMethod("configuration<-", "Asset",
function(
    obj,
    value) {

    # Check if locked.
    if(length(obj@centroids) > 0)
        stop("Asset is already assembled.")

    # Default values.
    cfg <- obj@parameters
    if(length(cfg) < 1) {
	cfg["norm"] <- 1
        cfg["nonzero.min"] <- 100
        cfg["nonzero.ratio"] <- 0.01
	cfg["standard"] <- 1
	cfg["logarithm"] <- 1
	cfg["ninput.max"] <- 20
	cfg["rrinput.max"] <- 0.36
        cfg["proximity.min"] <- 0.5
        cfg["exclusivity.min"] <- 0.5
    }

    # Check if unknown parameters.
    unkn <- setdiff(names(value), names(cfg))
    if(length(unkn) > 0) stop("Unknown parameter.")

    # Update values.
    prm <- intersect(names(cfg), names(value))
    cfg[prm] <- value[prm]

    # Check values.
    if(sum(!is.finite(cfg)) > 0) stop("Unusable parameter value.")
    if(sum(cfg < 0) > 0) stop("Negative parameter value.")
    obj@parameters <- cfg
    return(obj)
})

# Get internal parameters.
setGeneric("configuration", function(obj)
    standardGeneric("configuration"))
setMethod("configuration", "Asset",
function(obj) {
    return(obj@parameters)
})
