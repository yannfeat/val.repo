# Version 0.9.2 [2025-08-19]

## Documentation

 * Add missing package anchors for a few Rd links.


# Version 0.9.1 [2024-02-17]

## Documentation

 * Add `citation(package = "ACNE")`.
 
 * Fix minor Rd help-page issues.


# Version 0.9.0 [2023-06-25]

## Miscellaneous

 * Made NMF initialization a bit faster by calling **matrixStats**
   functions with optimized subsetting of rows and columns.

 * Importing `throw()` from **R.oo** instead of **R.methodsS3**.


# Version 0.8.1 [2015-10-24]

## Significant Changes

 * ROBUSTNESS: Importing core R functions.


# Version 0.8.0 [2015-02-23]

## Bug Fixes

 * `plot()` for `SnpNmfFit` used undefined variable `fit`.

## Miscellaneous

 * ROBUSTNESS: Package test coverage is 49%.


# Version 0.7.1 [2014-04-27]

## Documentation

 * Rebuilt help pages which drop references to a few
   deprecated/private methods.

 * Clarified the `example(fitSnpNmfArray)`.

## Miscellaneous

 * Added package system tests.

 * Bumped package dependencies.


# Version 0.7.0 [2013-10-17]

## Significant Changes

 * Now importing only what needs to be imported and formally declaring
   all S3 methods in NAMESPACE.

## New Features

 * Added argument `drop` to `doACNE()` to be more consistent with
   similar methods in **aroma.affymetrix**, e.g. `doRMA()` and
   `doCRMAv2()`.

## Documentation

 * Added a help page for `doACNE()`.

 * Added references to ACNE article.

## Miscellaneous

 * CLEANUP: Removed all explicit calls to `gc()` and replaced all
   `rm()` with faster `NULL` assignments.

 * Bumped the package dependencies.

 * Package now requires R (>= 2.15.0) and Bioconductor (>= 2.10.0).


# Version 0.6.2 [2013-09-16]

## Miscellaneous

 * Bumped the package dependencies.


# Version 0.6.1 [2013-08-23]

## Significant Changes

 * Now formally declaring S3 methods in NAMESPACE.


# Version 0.6.0 [2013-08-21]

## Significant Changes

 * Now package no longer attaches ("loads") several of the dependent
   packages, e.g. **R.methods**, **R.oo**, **matrixStats*, and
   **MASS**.

## Miscellaneous

 * Added `.Rbuildignore` to package so that `incl/` and other
   directories/files are not included in the source build.

 * Bumped the package dependencies.


# Version 0.5.1 [2012-10-16]

## Significant Changes

 * Now package imports **utils**, **R.methodsS3**, and **R.oo**.

## Bug Fixes

 * No longer passing '...' to `NextMethod()`, cf. R-devel thread 'Do
   *not* pass '...' to `NextMethod()` - it'll do it for you; missing
   documentation, a bug or just me?' on Oct 16, 2012.


# Version 0.5.0 [2011-10-24]

## Significant Changes

 * Added a NAMESPACE.


# Version 0.4.3 [2011-08-01]

## Significant Changes

 * In order for the package to work with the most recent version of R
   devel, which automatically add namespaces to packages who do not
   have one, we explicitly have specify that this package should use,
   for instance, `cat()` of **R.utils** (instead of **base**).


# Version 0.4.2 [2010-09-28]

## New Features

 * Improved validation of argument `refs`.


# Version 0.4.1 [2010-06-21]

## New Features

 * Added argument `refs` to `NmfPlm()` and `fitSnpNmf()`.


## Miscellaneous

 * Added more biocViews to the DESCRIPTION file.


# Version 0.4.0 [2010-05-18]

## Bug Fixes

 * The `NmfSnp` class would use a way too large threshold for deciding
   when the NMF algorithm has converged.  The default accuracy 0.02
   was replaced with 10.0 due to a "stray" argument in an internal
   function call.  This bug has been there since March 24, 2009.

## Deprecated and Defunct

 * All flavors of NMF algorithms but the default one (`"v4"`) is
   deprecated and no longer available in `NmfPlm`.


# Version 0.3.0 [2010-05-17]

## New Features

 * Added `doACNE()`.  Added also redundancy tests for it.

## Deprecated and Defunct

 * Now a `flavor` tag is added to `NmfPlm`:s only if `flavor != "v4"`
   (default).


# Version 0.2.0 [2010-05-04]

## Significant Changes

 * Set the license to LGPL v2.1 or newer.

## Miscellaneous

 * Bumped up the version of the package dependencies.


# Version 0.1.2 [2009-11-18]

## Miscellaneous

 * For debug purposes, the internal `NmfPlm` fit function as well as
   `fitSnpNmf()` used to save the input data for each SNP fitted.
   This is now removed, and should speed up the processing
   substantially.


# Version 0.1.1 [2009-11-11]

## Miscellaneous

 * Removed an obsolete `testScripts/`.

## Bug Fixes

 * One of the `testScripts/` had a type.


# Version 0.1.0 [2009-11-03]

## Significant Changes

 * Renamed package to **ACNE** (from **aroma.affymetrix.nmf**).

## Miscellaneous

 * Moved the package to the r-forge.r-project.org repository.


# Version 0.0.5 [2009-03-25]

## New Features

 * Added `fitSnpNmfArray()`, which takes an Kx2xI
   array. `fitSnpNmfArray()` returns a list object of class
   `SnpNmfFit` for which there is a `plot()` function. Note,
   `fitSnpNmf()` takes a 2KxI matrix.

 * Added `snpMatrixToArray()` and `snpArrayToMatrix()`.

## Documentation

 * Added example code with example data for `fitSnpNmfArray()`.


# Version 0.0.4 [2009-03-24]

## Significant Changes

 * Removed obsolete dependency on the **impute** package.

## Documentation

 * Added Rdoc comments (Rd help pages).

## Miscellaneous

 * Added sanity checks of all input and output to functions.

 * Cleaned up code.


# Version 0.0.3 [2009-02-02]

## Miscellaneous

 * Unknown changes.


# Version 0.0.2 [2009-01-28]

## Significant Changes

 * `NmfPlm` now inherits from `ProbeLevelModel`.

## Deprecated and Defunct

 * Removed old `MyProbeLevelModel` class.

 * Created temporary package.


# Version 0.0.1 [NA]

## Significant Changes

 * Created.

