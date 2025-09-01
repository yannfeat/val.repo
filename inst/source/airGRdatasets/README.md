# airGRdatasets: Hydro-Meteorological Catchments Datasets for the 'airGR' packages

## Overview

'airGRdatasets' provides metadata and catchment-scale aggregated hydro-meteorological time series on a pool of French catchments for use by the 'airGR' packages. More especially, it can be used by teachers and students for hydrological modeling exercises adapted to the 'airGRteaching' package as described in [Delaigue et al. (submitted)](https://doi.org/10.5194/hess-2022-421) and in the 'airGRteaching' vignettes.

For more details:
``` r
?airGRdatasets
```

## Licence

[Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0)](https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode.txt)

## Installation

### Release version

To install the version of the package that is on the CRAN, you just have to use the following command line:

``` r
install.packages("airGRdatasets")
```

### Unrelease version

To use the development version of the package that is on GitLab, you have first to install the 'remotes' package. Then you can install the 'airGRdatasets' package in the R environment:

``` r
install.packages("remotes")
remotes::install_gitlab(repo = "HYCAR-Hydro/airgrgalaxy/airgrdatasets", 
                        host = "https://gitlab.irstea.fr")
```
