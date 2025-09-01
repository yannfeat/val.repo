# Save resources to disk.
setGeneric("nomenclature<-", function(obj, value)
    standardGeneric("nomenclature<-"))
setMethod("nomenclature<-", "Asset",
function(
    obj,
    value) {

    # Check if locked.
    if(length(obj@centroids) > 0)
        stop("Asset is already assembled.")

    # Check input.
    if(!is.data.frame(value)) stop("Value must be a data frame.")
    if(nrow(value)*ncol(value) < 1) stop("Empty value.")
    if(length(rownames(value)) < 1) stop("No row names.")
    if(length(colnames(value)) < 1) stop("No column names.")

    # Check for duplicates.
    for(j in 1:ncol(value)) {
        if(anyDuplicated(value[,j]))
            stop("Duplicated entries in nomenclature.")
    }

    # Update object.
    obj@naming <- value
    return(obj)
})
