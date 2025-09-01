if(readline("Clear workspace? [y/N] ") != "y")
  stop("Examples not run.")
library("Allspice")
prevopt <- options(); options(digits=3)
set.seed(1)

cat("\nAsset.assemble.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Prepare training data.
    simu <- bcellALL(200)
    materials <- list(title="Simutypes")
    materials$dat <- simu$counts
    materials$covariates <- simu$metadata[,c("MALE","AGE")]
    materials$bits <- simu$metadata[,"SUBTYPE",drop=FALSE]
    
    # Assemble classification asset.
    bALL <- asset()
    assemble(bALL) <- materials
    
    # Export asset into a new folder.
    tpath <- tempfile()
    export(bALL, folder = tpath)
    
    # Create a classifier.
    cls <- classifier(tpath, verbose = FALSE)
    
    # Classify new samples.
    simu <- bcellALL(5)
    covariates(cls) <- simu$metadata
    profiles(cls) <- simu$counts
    primary <- predictions(cls)[[1]]
    print(primary[,c("LABEL","PROX","EXCL")])
Sys.sleep(1)

cat("\nAsset.classify.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Import ALL subtyping asset.
    base <- system.file(package = "Allspice")
    folder <- file.path(base, "subtypes")
    a <- asset(folder)
    
    # Simulated data.
    simu <- bcellALL(5)
    
    # Standardize RNA read counts.
    expres <- normalize(a, dat = simu$counts)
    expres <- standardize(a, dat = expres)
    
    # Predict categories.
    res <- classify(a, dat = expres, covariates = simu$metadata)
    print(res[,c("LABEL","PROX","EXCL")])
Sys.sleep(1)

cat("\nAsset.configuration.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Change asset configuration.
    a <- asset()
    print(configuration(a))
    configuration(a) <- c(nonzero.min=0, nonzero.ratio=0)
    print(configuration(a))
Sys.sleep(1)

cat("\nAsset.export.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Import ALL subtyping asset.
    base <- system.file(package = "Allspice")
    folder <- file.path(base, "subtypes")
    a <- asset(folder)
    
    # Export asset into a new folder.
    tpath <- tempfile()
    fnames <- export(a, folder = tpath)
    print(dir(tpath))
Sys.sleep(1)

cat("\nAsset.nomenclature.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Import nomenclature from a system file.
    base <- system.file(package = "Allspice")
    fname <- file.path(base, "subtypes", "nomenclature.txt")
    info <- read.delim(fname, stringsAsFactors = FALSE)
    
    # Set ENSEMBLE identities as row names.
    rownames(info) <- info$ENSEMBL
    info$ENSEMBL <- NULL
    print(head(info))
    
    # Create a new asset and set nomenclature.
    a <- asset()
    nomenclature(a) <- info
    
    # Prepare training data.
    simu <- bcellALL(200)
    materials <- list(title="Simutypes")
    materials$dat <- simu$counts
    materials$covariates <- simu$metadata[,c("MALE","AGE")]
    materials$bits <- simu$metadata[,"SUBTYPE",drop=FALSE]
    
    # Assemble classification asset.
    assemble(a) <- materials
    
    # Check that nomenclature was set.
    simu <- bcellALL(5)
    expres <- normalize(a, dat = simu$counts)
    print(head(simu$counts))
    print(head(expres))
Sys.sleep(1)

cat("\nAsset.normalize.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    
    # Import ALL subtyping asset.
    base <- system.file(package = "Allspice")
    folder <- file.path(base, "subtypes")
    a <- asset(folder)
    
    # Simulated data.
    simu <- bcellALL(5)
    
    # Normalize RNA read counts.
    expres <- normalize(a, dat = simu$counts)
    print(head(simu$counts))
    print(head(expres))
Sys.sleep(1)

cat("\nAsset.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Set up an ALL subtyping asset.
    folder <- system.file("subtypes", package="Allspice")
    a <- asset(folder)
Sys.sleep(1)

cat("\nAsset.standardize.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Import ALL subtyping asset.
    base <- system.file(package = "Allspice")
    folder <- file.path(base, "subtypes")
    a <- asset(folder)
    
    # Simulated data.
    simu <- bcellALL(5)
    
    # Standardize RNA read counts.
    expres <- normalize(a, dat = simu$counts)
    zscores <- standardize(a, dat = expres)
    print(head(simu$counts))
    print(head(expres))
    print(head(zscores))
Sys.sleep(1)

cat("\nAsset.visuals.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Create a new asset and set nomenclature.
    a <- asset()
    
    # Set category labels with automatic colors.
    labels <- paste("Category", 1:8)
    names(labels) <- paste0("cat", 1:8)
    visuals(a) <- labels
    print(a@categories)
    
    # Add color information.
    info <- data.frame(stringsAsFactors = FALSE,
        LABEL = labels, COLOR = "red")
    rownames(info) <- names(labels)
    visuals(a) <- info
    print(a@categories)
Sys.sleep(1)

cat("\nbcellALL.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Simulate B-cell ALL samples.
    simu <- bcellALL(5)
    print(head(simu$counts))
    print(simu$metadata)
Sys.sleep(1)

cat("\nClassifier.covariates.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Simulated data.
    simu <- bcellALL(5)
    
    # Predict subtypes without covariates.
    cls <- classifier(verbose = FALSE)
    profiles(cls) <- simu$counts
    primary <- predictions(cls)[[1]]
    print(primary[,c("LABEL","PROX","EXCL")])
    
    # Predict subtypes with covariates.
    cls <- classifier(verbose = FALSE)
    covariates(cls) <- simu$metadata
    profiles(cls) <- simu$counts
    primary <- predictions(cls)[[1]]
    print(primary[,c("LABEL","PROX","EXCL")])
Sys.sleep(1)

cat("\nClassifier.information.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Show the contents of the b-cell ALL classifier.
    cls <- classifier(verbose=FALSE)
    info <- information(cls)
    print(info$covariates)
    print(info$configuration)
    print(head(info$categories))
    print(tail(info$categories))
Sys.sleep(1)

cat("\nClassifier.predictions.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Simulated data.
    simu <- bcellALL(5)
    
    # Predict subtypes.
    cls <- classifier(verbose = FALSE)
    covariates(cls) <- simu$metadata
    profiles(cls) <- simu$counts
    pred <- predictions(cls)
    print(pred[[1]][,c("LABEL","PROX","EXCL")])
    print(pred[[2]][,c("LABEL","PROX","EXCL")])
    print(pred[[3]][,c("LABEL","PROX","EXCL")])
Sys.sleep(1)

cat("\nClassifier.profiles.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Simulated data.
    simu <- bcellALL(5)
    
    # Predict subtypes.
    cls <- classifier(verbose = FALSE)
    covariates(cls) <- simu$metadata
    profiles(cls) <- simu$counts
    primary <- predictions(cls)[[1]]
    print(primary[,c("LABEL","PROX","EXCL")])
Sys.sleep(1)

cat("\nClassifier.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Set up an ALL classifier object.
    cls <- classifier()
Sys.sleep(1)

cat("\nClassifier.report.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Simulated data.
    simu <- bcellALL(5)
    keys <- colnames(simu$counts)
    
    # Predict subtypes.
    cls <- classifier(verbose = FALSE)
    covariates(cls) <- simu$metadata
    profiles(cls) <- simu$counts
    
    # Show visual report by name.
    dev.new()
    report(cls, name = keys[3])
    
    # Show visual report by sample index.
    dev.new()
    report(cls, name = 3)
Sys.sleep(1)

cat("\nClassifier.scores.Rd\n")
rm(list=setdiff(ls(),"prevopt"))

    # Simulated data.
    simu <- bcellALL(5)
    
    # Predict subtypes.
    cls <- classifier(verbose = FALSE)
    covariates(cls) <- simu$metadata
    profiles(cls) <- simu$counts
    z <- scores(cls)
    print(z[[1]][,1:5])
    print(z[[2]][,1:5])
    print(z[[3]][,1:5])
Sys.sleep(1)

options(prevopt)
cat("\nAll examples completed.\n\n")
