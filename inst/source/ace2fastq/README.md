
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ace2fastq

<!-- badges: start -->

[![Project Status: Active ÃƒÂ¢Ã¢â€šÂ¬Ã¢â‚¬Å“ The project has reached a
stable, usable state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/c5sire/ace2fastq?branch=master&svg=true)](https://ci.appveyor.com/project/c5sire/ace2fastq)
[![Travis build
status](https://travis-ci.org/c5sire/ace2fastq.svg?branch=master)](https://travis-ci.org/c5sire/ace2fastq)
[![Codecov test
coverage](https://codecov.io/gh/c5sire/ace2fastq/branch/master/graph/badge.svg)](https://codecov.io/gh/c5sire/ace2fastq?branch=master)

[![CRAN
status](https://www.r-pkg.org/badges/version/ace2fastq)](https://CRAN.R-project.org/package=ace2fastq)
[![Downloads](https://cranlogs.r-pkg.org/badges/ace2fastq)](https://CRAN.R-project.org/package=ace2fastq)
<!-- badges: end -->

The package provides a function that converts “.ace” files (ABI Sanger
capillary sequence assembly files) to standard “.fastq” files. The file
format is currently used in genomics to store contigs. To the best of
our knowledge, no R function is available to convert this format into
the more popular fastq file format. The development was motivated in the
context of the analysis of 16S metagenomic data by the need to convert
the .ace files for further analysis.

## Installation

You can install the released version of ace2fastq from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ace2fastq")
```

Latest version can be installed from github:

``` r
install.packages(devtools)

devtools::install_github("c5sire/ace2fastq")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ace2fastq)


filename <- system.file("sampledat/1.seq.ace", package = "ace2fastq")

out_file <- ace_to_fastq(filename, target_dir = tempdir())

lines <- readLines(out_file$path)
```

    #> [1] "@1.seq CO Contig1 1489 2 12 U"
    #> [1] "gctccctgatgttagcggcggACGGGTGAGTAACACGTGGG"
    #> [1] "+"
    #> [1] "!!!!!!!!!!!!!!!!!!!!!DUNUUUUUUUNUDIIIUUUU"
