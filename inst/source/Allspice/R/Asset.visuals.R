setGeneric("visuals<-", function(obj, value)
    standardGeneric("visuals<-"))
setMethod("visuals<-", "Asset",
function(
    obj,
    value) {

    # Check if locked.
    if(length(obj@centroids) > 0)
        stop("Asset is already assembled.")

    # Check input.
    if(is.character(value)) {
        vnames <- names(value)
        value <- data.frame(LABEL=value)
	if(length(vnames) > 0) rownames(value) <- vnames
	else rownames(value) <- value$LABEL
    }
    if(!is.data.frame(value)) stop("Unusable value.")
    if(nrow(value)*ncol(value) < 1) stop("Empty value.")
    if(length(rownames(value)) < 1) stop("No row names.")
    if(length(value$LABEL) < 1) stop("No labels.")
    
    # Default visuals.
    categ <- list(LABEL=as.character(value$LABEL))
    if(length(value$COLOR) > 0) categ$COLOR <- value$COLOR
    else categ$COLOR <- colormap(1:nrow(value))
    if(length(value$COLOR.light) > 0) categ$COLOR.light <- value$COLOR.light
    else categ$COLOR.light <- colormap(1:nrow(value), adjustment="light")
    if(length(value$COLOR.dark) > 0) categ$COLOR.dark <- value$COLOR.dark
    else categ$COLOR.dark <- colormap(1:nrow(value), adjustment="dark")

    # Add reserved categories.
    vnames <- rownames(value)
    if(sum(vnames == "Unclassified") < 1) {
        categ$LABEL <- c(categ$LABEL, "Unclassified")  
        categ$COLOR <- c(categ$COLOR, "#888888")  
        categ$COLOR.light <- c(categ$COLOR.light, "#b0b0b0")
        categ$COLOR.dark <- c(categ$COLOR.dark, "#404040")
	vnames <- c(vnames, "Unclassified")
    }
    if(sum(vnames == "Ambiguous") < 1) {
        categ$LABEL <- c(categ$LABEL, "Ambiguous")  
        categ$COLOR <- c(categ$COLOR, "#888888")
        categ$COLOR.light <- c(categ$COLOR.light, "#b0b0b0")
        categ$COLOR.dark <- c(categ$COLOR.dark, "#404040")
	vnames <- c(vnames, "Ambiguous")
    }

    # Convert to data frame.
    categ <- data.frame(categ, stringsAsFactors=FALSE)
    rownames(categ) <- vnames

    # Update object.
    obj@categories <- categ
    return(obj)
})