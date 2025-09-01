## ----echo=FALSE, results="hide"-----------------------------------------------
opt <- options(digits=3)
set.seed(1)

## ----eval=FALSE---------------------------------------------------------------
#  # Install the package from a remote repository.
#  install.packages("Allspice")

## -----------------------------------------------------------------------------
# Activate the library.
library("Allspice")
packageVersion("Allspice")
ls("package:Allspice")

## ----eval=FALSE---------------------------------------------------------------
#  # Access function documentation (not shown in vignette).
#  ? Allspice::classifier

## ----eval=FALSE---------------------------------------------------------------
#  # Run all code examples (not shown in vignette).
#  fn <- system.file("examples.R", package = "Allspice")
#  source(fn)

## -----------------------------------------------------------------------------
# Generate gene RNA read counts.
simu <- bcellALL(300)
print(simu$counts[1:5,1:6])
print(simu$metadata[1:6,])

## -----------------------------------------------------------------------------
# Set up a classifier for genetic B-cell ALL subtypes.
cls <- classifier()

## -----------------------------------------------------------------------------
# List covariates.
info <- information(cls)
print(info$covariates[,c("ASSET","TITLE","COVAR")])

## -----------------------------------------------------------------------------
# Set covariates.
covariates(cls) <- simu$metadata

## -----------------------------------------------------------------------------
# Load RNA-seq profiles.
profiles(cls) <- simu$counts

## -----------------------------------------------------------------------------
# Prediction results.
pred <- predictions(cls)
primary <- pred[[1]]
print(primary[1:6,c("LABEL","FREQ","PROX","EXCL")])

## -----------------------------------------------------------------------------
# Prediction results.
ambig <- which(primary$CATEG == "Ambiguous")
uncla <- which(primary$CATEG == "Unclassified")
rows <- unique(c(1:5, ambig[1], uncla[1]))
print(primary[rows,c("LABEL","MATCH","FREQ","PROX","EXCL")])

## -----------------------------------------------------------------------------
# Access subtype labelling information.
info <- information(cls)
print(info$categories[1:5,c("ASSET","TITLE","CATEG","LABEL")])

## -----------------------------------------------------------------------------
# Select successful classification.
rows <- which((primary$CATEG != "Ambiguous") & (primary$FREQ > 0.9))
print(primary[rows[1],c("LABEL","MATCH","FREQ","PROX","EXCL")])

## ----results="hide", fig.width=8, fig.height=6, fig.align="center", fig.cap="Figure: Classification results. The predicted subtype label is written on the top left corner. The expected frequency of the subtype given the data profile is written as a percentage after the predicted label. RNA biomarker scores were also calculated for the presence of alterations in specific genes (center bar chart), note that a patient may have multiple driver genes in parallel. The right-most chart shows RNA biomarker scores for the predicted source tissue of the sample (in this simulated dataset, all samples were generated from B-cell ALL profiles). "----
# Show patient report.
report(cls, name = rows[1], file = NULL)

## -----------------------------------------------------------------------------
# Select ambiguous classification.
rows <- which((primary$CATEG == "Ambiguous") & (primary$PROX > 0.9))
print(primary[rows[1],c("LABEL","MATCH","FREQ","PROX","EXCL")])

## ----results="hide", fig.width=8, fig.height=6, fig.align="center", fig.cap="Figure: Classification results for a case with mixed transcriptional characteristics."----
# Show patient report.
report(cls, name = rows[1], file = NULL)

## -----------------------------------------------------------------------------
# Select poor quality samples.
rows <- which(primary$CATEG == "Unclassified")
print(primary[rows[1],c("LABEL","MATCH","FREQ","PROX","EXCL")])

## ----results="hide", fig.width=8, fig.height=6, fig.align="center", fig.cap="Figure: Classification results for a case with atypical data."----
# Show patient report.
report(cls, name = rows[1], file = NULL)

## -----------------------------------------------------------------------------
# Create a new empty asset.
bALL <- asset()
print(configuration(bALL))

## -----------------------------------------------------------------------------
# Re-configure asset.
configuration(bALL) <- c(ninput.max=30, nonzero.min=90)
print(configuration(bALL)[c("ninput.max","nonzero.min")])

## -----------------------------------------------------------------------------
# Prepare asset title.
materials <- list(title="Simutypes")

## -----------------------------------------------------------------------------
# Prepare RNA-seq read counts.
materials$dat <- simu$counts

## -----------------------------------------------------------------------------
# Prepare covariate data.
materials$covariates <- simu$metadata[,c("MALE","AGE")]

## -----------------------------------------------------------------------------
# Prepare subtype information.
categ <- simu$metadata[,"SUBTYPE",drop=FALSE]
rows <- which(categ != "Contaminated")
materials$bits <- categ[rows,,drop=FALSE]

## -----------------------------------------------------------------------------
# Assemble the classification asset.
bALL <- asset()
assemble(bALL) <- materials

## -----------------------------------------------------------------------------
# Save asset to disk.
tpath <- tempfile()
export(bALL, folder = tpath)

## -----------------------------------------------------------------------------
# Create a classifier.
clstest <- classifier(tpath)

## -----------------------------------------------------------------------------
# Classify samples.
simutest <- bcellALL(5)
covariates(clstest) <- simutest$metadata
profiles(clstest) <- simutest$counts
primtest <- predictions(clstest)[[1]]
print(primtest[,c("LABEL","MATCH","FREQ","PROX","EXCL")])

## -----------------------------------------------------------------------------
# Show correct subtypes.
print(simutest$metadata)

## -----------------------------------------------------------------------------
# Iris flower dataset.
print(head(iris))

## -----------------------------------------------------------------------------
# Set row names.
flowers <- iris
rownames(flowers) <- paste0("flower", 1:nrow(flowers))
print(flowers[c(1,80,150),])

## -----------------------------------------------------------------------------
# Prepare training set.
materials <- list(title="Iris species")
materials$dat <- t(flowers[,1:4]) # vars on rows, samples on columns
materials$bits <- flowers[,"Species",drop=FALSE]

## -----------------------------------------------------------------------------
# Set human-readable category labels.
model <- asset()
labels <- c("Iris Setosa", "Iris Virginica", "Iris Versicolor")
names(labels) <- c("setosa", "virginica", "versicolor")
visuals(model) <- labels

## -----------------------------------------------------------------------------
# Configure a new asset.
configuration(model) <- c(norm=FALSE, logarithm=FALSE)
configuration(model) <- c(nonzero.min=0, nonzero.ratio=0)
print(configuration(model))

## -----------------------------------------------------------------------------
# Assemble the classification asset.
assemble(model) <- materials
tpath <- tempfile()
export(model, folder = tpath)

## -----------------------------------------------------------------------------
# Classify samples.
clsiris <- classifier(tpath)
profiles(clsiris) <- t(flowers[,1:4])
iristest <- predictions(clsiris)[[1]]
print(iristest[c(1,80,150),c("LABEL","MATCH","PROX","EXCL")])

## -----------------------------------------------------------------------------
# Summary of results.
print(table(iristest$LABEL, flowers$Species))

## ----results="hide"-----------------------------------------------------------
# Default ALL classifier.
cls <- classifier()

## -----------------------------------------------------------------------------
# Predict source tissue.
simu <- bcellALL(5)
covariates(cls) <- simu$metadata
profiles(cls) <- simu$counts
tissues <- predictions(cls)[[3]]
print(tissues[,c("LABEL","CATEG","MATCH","MATCH.2nd")])

## -----------------------------------------------------------------------------
# Show asset contents.
base <- system.file(package = "Allspice")
folder <- file.path(base, "subtypes")
print(dir(folder))

## -----------------------------------------------------------------------------
# Category information.
dat <- read.delim(file.path(folder, "categories.txt"))
print(dat)

## -----------------------------------------------------------------------------
# Standardized subtype profiles.
dat <- read.delim(file.path(folder, "centroids.txt"))
print(dat[1:5,1:6])
cat(nrow(dat), " genes, ", ncol(dat), " subtypes\n", sep="")

## -----------------------------------------------------------------------------
# Regression coefficients.
dat <- read.delim(file.path(folder, "coefficients.txt"))
print(dat[1:5,])

## -----------------------------------------------------------------------------
# Asset settings.
dat <- read.delim(file.path(folder, "configuration.txt"))
print(dat)

## -----------------------------------------------------------------------------
# Covariate statistics.
dat <- read.delim(file.path(folder, "covariates.txt"))
print(dat)

## -----------------------------------------------------------------------------
# Gene names.
dat <- read.delim(file.path(folder, "nomenclature.txt"))
print(dat[1:5,])

## -----------------------------------------------------------------------------
# RNA reference profile.
dat <- read.delim(file.path(folder, "reference.txt"))
print(dat[1:5,])
cat(nrow(dat), " genes\n", sep="")

## ----echo=FALSE---------------------------------------------------------------
sessionInfo()
Sys.time()

## ----echo=FALSE, results="hide"-----------------------------------------------
options(opt)

