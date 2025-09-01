# Predicted category labels.
setGeneric("predictions", function(obj)
    standardGeneric("predictions"))
setMethod("predictions", "Classifier", function(obj) {
    output <- NULL

    # Check if results are available.
    if(length(obj@results) < 1) stop("No profiles.")

    # Strip any extra attributes from data frames.
    output <- list()
    for(r in names(obj@results)) {
        output[[r]] <- data.frame(obj@results[[r]],
	    stringsAsFactors=FALSE)
    }
    return(output)
})
