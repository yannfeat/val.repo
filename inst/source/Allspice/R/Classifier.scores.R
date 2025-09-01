# Predicted category labels.
setGeneric("scores", function(obj)
    standardGeneric("scores"))
setMethod("scores", "Classifier", function(obj) {
    output <- NULL

    # Check if results are available.
    if(length(obj@results) < 1) stop("No profiles.")

    # Collect biomarker scores.
    output <- list()
    for(r in names(obj@results))
        output[[r]] <- attr(obj@results[[r]], "biomarkers")
    return(output)
})
