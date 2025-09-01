# Predicted category labels.
setGeneric("report", function(obj, name, file=NULL)
    standardGeneric("report"))
setMethod("report", "Classifier",
function(
    obj,
    name,
    file=NULL) {

    # Check if any results.
    if(length(obj@results) < 1) stop("No profiles.")

    # Check sample name.
    if(is.numeric(name)) {
         name <- round(name)[1]
         keys <- rownames(obj@results[[1]])
	 if(!is.finite(name)) stop("Unusable name.")
	 if(name < 1) stop("Unusable name.")
	 if(name > length(keys)) stop("Unusable name.")
         name <- keys[name]
    }
    if(!is.character(name)) stop("Unusable name.")
    if(length(name) < 1) stop("Unusable name.")
    name <- as.character(name)[1]

    # Check file name.
    if(length(file) > 0) {
        if(!is.character(file)) stop("Unusable file name.")
        file <- as.character(file[1])
    }

    # Visualize classification results.
    for(j in 1:length(obj@results)) {
        res <- report_bars(fname=file, key=name, column=j,
	    assets=obj@assets, results=obj@results)
        if(is.character(res)) stop(res)
    }

    # Return file name.
    invisible(file)
})

#---------------------------------------------------------------------------

report_bars <- function(
    fname,
    key,
    results,
    assets,
    column,
    block=9,
    capacity=18) {

    # Find prediction.
    pred <- results[[column]]
    ind <- which(rownames(pred) == key)
    if(length(ind) < 1) return("Unknown name.")
    output <- pred[key,,drop=FALSE]

    # Set canvas size.
    primary <- assets[[1]]
    nrows <- max(ncol(primary@centroids), capacity)
    ncols <- length(results)
    xcanvas <- c(0.5, (block*max(ncols, 2) + 3))
    ycanvas <- c(0.5, (nrows + 3.2))
    wcanvas <- (xcanvas[2] - xcanvas[1])
    hcanvas <- (ycanvas[2] - ycanvas[1])
    
    # Set up figure.
    if(column == 1) {
        if(length(fname) > 0)
            svg(filename=fname, width=0.3*wcanvas, height=0.3*hcanvas)

        # Set canvas parameters.
        prev <- par(pty="m", oma=c(0,0,0,0), mar=c(0,0,0,0))
        on.exit(par(prev))

        # Prepare plot area.
        plot(bty="n", axes=FALSE, asp=(length(fname) > 0),
            x=NA, y=NA, xlim=xcanvas, ylim=ycanvas,
            xlab=NA, ylab=NA, main=NA)

        # Sample name.
        text(x=xcanvas[1], y=(ycanvas[2] - 0.55),
            labels=key, pos=4, cex=1.2, col="#303030")

        # Best matching category and frequency.
        clabel <- output$LABEL
        best <- primary@categories[output$MATCH,"LABEL"]
	if(clabel == best)
	    clabel <- paste(clabel, percent_label(output$FREQ))

        # Category label.
        colr <- output$COLOR.dark
        text(x=xcanvas[1], y=(ycanvas[2] - 1.38),
            labels=clabel, pos=4, cex=1.0, col=colr, font=2)

        # Proximity.
        proxmin <- primary@parameters["proximity.min"]
        if(output$PROX < proxmin) {
            txt <- sprintf("Proximity: %s", percent_label(output$PROX))
 	    txt <- sprintf("%s < %.0f%%", txt, 100*proxmin)
            text(x=(xcanvas[1] + block), y=(ycanvas[2] - 0.55),
                labels=txt, pos=4, cex=1.0, col="#000000")
        }

        # Exclusivity.
        exclmin <- primary@parameters["proximity.min"]
        if(output$EXCL < exclmin) {
            txt <- sprintf("Exclusivity: %s", percent_label(output$EXCL))
 	    txt <- sprintf("%s < %.0f%%", txt, 100*exclmin)
            text(x=(xcanvas[1] + block), y=(ycanvas[2] - 1.38),
                labels=txt, pos=4, cex=1.0, col="#000000")
        }
    }

    # Title for category score.
    ass <- assets[[column]]
    xdiv <- (xcanvas[1] + (column - 0.5)*block + 1.5)
    if(column == 1) {
        text(x=xdiv, y=(nrows + 0.4), labels=ass@title,
            pos=3, cex=1.0, font=2, col="#000000")
    }
    else {
        text(x=xdiv, y=(nrows + 0.4), labels=ass@title,
            pos=3, cex=1.0, col="#606060")
    }

    # Sort entries by biomarker value.
    biomrk <- attr(pred, "biomarkers.grouped")
    mask <- order(biomrk[key,], decreasing=TRUE)
    bnames <- colnames(biomrk)[mask]
    
    # Exclude duplicated labels.
    visuals <- ass@categories
    mask <- which(duplicated(visuals[bnames,"LABEL"]) == FALSE)
    if(length(mask) > nrows) mask <- mask[1:nrows]
    bnames <- bnames[mask]

    # Sort entries alphabetically.
    mask <- order(visuals[bnames,"LABEL"])
    bnames <- bnames[mask]

    # Category labels.
    labels <- visuals[bnames,"LABEL"]
    ydivs <- (nrows:1); ydivs <- ydivs[1:length(labels)]
    text(x=xdiv, y=ydivs, labels=labels,
        pos=2, cex=1.0, col="#000000")

    # Set colors.
    fills <- rep("#a0a0a0", length.out=length(labels))
    strokes <- rep("#505050", length.out=length(labels))
    if(column == 1) {
        fills <- visuals[bnames,"COLOR"]
        strokes <- visuals[bnames,"COLOR.dark"]
    }

    # Category scores.
    bvals <- biomrk[key,bnames]
    dx <- bvals/max(max(bvals), 3)
    dx <- pmax(dx, 0.03)*(block - 5)
    rect(xleft=xdiv, ybottom=(ydivs - 0.35),
        xright=(xdiv + dx), ytop=(ydivs + 0.48),
	border=NA, col=fills)

    # Save figure.
    if((length(fname) > 0) && (column == length(results))) dev.off()
    return(NULL)
}

#---------------------------------------------------------------------------

percent_label <- function(x) {
   if(x < 0.01) return("<1%")
   if(x > 0.99) return(">99%")
   return(sprintf("%.0f%%", 100*x))
}
