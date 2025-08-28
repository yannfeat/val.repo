# val.repo

val.repo is a curated repository of R packages that is structured similarly to
CRAN but with additional metadata: It hosts validation reports on its
associated website, published via GitHub Pages.

## Repository Functionality

val.repo provides a repository that users can point to without
installing the pharmapkgs R package. This can be achieved by setting the
repository to the pharmapkgs repository hosted on GitHub.

### Using the Repository

``` r
options(
    repos = "https://raw.githubusercontent.com/pharmaR/val.repo/refs/heads/main/repos",
    pkgType = "source"
)
available_packages <- available.packages()
nrow(available_packages)
#> [1] 7336
```

Once set, calling `available.packages()` will list the packages
available in the pharmapkgs repository. The repository currently
contains around 7,000 packages, a subset of CRAN’s nearly 30,000
packages.

### Installation Limitations

The val.repo repository differs from a traditional CRAN repository. It
does not store package tarballs but instead relies on a `download_url`
field that links to the CRAN mirror on GitHub. Base R’s
`install.packages()` expects package tarballs within the same
repository, so it does not work natively with val.repo. Instead, users
should use **pak** to install packages from this repository.

## Installation via `pak`

``` r
options(
    repos = "https://raw.githubusercontent.com/pharmaR/val.repo/refs/heads/main/repos",
    pkgType = "source"
)
install.packages("pak")
pak::pkg_install("<package_name>")
```

The `pak` package understands the `download_url` field and fetches the
package source accordingly.

## Filtering Packages

val.repo features an enhanced `PACKAGES` file containing additional metadata.
Users can define custom filters for selecting packages, for example by using
the [val.criterion](https://github.com/pharmaR/val.criterion) package.
The CRAN-like structure is located under `src/contrib`.

### Example: Filtering by Dependencies Score

``` r
my_filters <- val.criterion::risk_filter(
    dependencies > 0.6
)
#> val.criterion log level:DEBUG
#> val.criterion: config values loaded
#> {
#>   "excluded_riskmetric_assessments": ["assess_covr_coverage", "assess_r_cmd_check"],
#>   "local_packages": "/tmp/RtmpdQIMZX/temp_libpath1354e8a6171/val.repo/repos/src/contrib/PACKAGES",
#>   "project_path": "/workspaces/val.criterion",
#>   "local_base": "/tmp/RtmpdQIMZX/temp_libpath1354e8a6171/val.repo/repos",
#>   "remote_base": "https://cloud.r-project.org/",
#>   "limit": 5
#> }
filtered_packages <- available.packages(filters = my_filters, fields = "dependencies")
nrow(filtered_packages)
#> [1] 3070
```

This command filters packages where the `dependencies` score is greater than
0.6.

Notice how `filtered_packages` has less rows than `available_packages`.

## Handling Dependencies

Since the val.repo repository is incomplete, some dependencies may not
be available. To mitigate this, users can specify multiple repositories
so that missing packages are fetched from CRAN.

``` r
options(repos = c(Pharma = "https://raw.githubusercontent.com/pharmaR/val.repo/refs/heads/main/repos", CRAN = "https://cran.r-project.org"))
```

This setup allows pak to first check val.repo, then fall back to CRAN
if a package is missing.

## Technical Details

### Repository Structure

- The repository contains a `PACKAGES` file with metadata.
- Instead of hosting package tarballs, the `download_url` field points
  to the GitHub CRAN mirror.

### Why Not Use CRAN Directly?

CRAN changes URLs for archived packages, making direct linking
unreliable. The GitHub CRAN mirror is more stable as each package
version is stored as a tagged release.

### GitHub Actions

A GitHub Actions pipeline updates the repository and generates reports.
However, report generation is secondary to package validation and is
prone to failures. Incremental site builds in Quarto are not yet
supported, requiring full site rebuilds.

## Known Issues and Future Work

- The repository is incomplete and being updated slowly.
- The GitHub Actions pipeline occasionally fails due to report
  generation issues.
- Future work includes optimizing package validation and improving
  repository completeness.
