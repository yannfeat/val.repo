
<!-- README.md is generated from README.Rmd. Please edit that file -->

# allMT

<!-- badges: start -->

[![R-CMD-check](https://github.com/tmungle/allMT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tmungle/allMT/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/allMT)](https://CRAN.R-project.org/package=allMT)
<!-- badges: end -->

## Introduction

Acute lymphoblastic leukemia (ALL) studies across the globe have shown
that delivering optimal maintenance therapy (MT) is crucial to achieve
better treatment outcomes [\[1\]](#ref-schmiegelow2014mercaptopurine).
Delivering optimal MT includes prescribing maximum tolerated doses of
6-mercaptopurine (6MP) and methotraxate (MTX) based on patient’s blood
counts (absolute neutrophil count (ANC), platelet count (PLT) and
hemoglobin (HB)). In view of this, the *allMT* package was created to
analyse and visualize the MT data (ANC/PLT/HB/6MP/MTX) for single
patient or a given cohort at any center[^1]. The package was developed
considering the ICiCLe-ALL-14 protocol [\[3\]](#ref-das2022protocol) as
default. It can be used to analyze data from centers with different MT
protocols where in the user will need to alter the function defined
ipnut argument/parameters.

## Package

Using this package the user may:

1.  Convert maintenance sheets (Tata Medical Center Kolkata, India
    format or user dependent) into a cleaner, single csv with
    longitudinal data for each patient.

2.  Analyze MT [\[2\]](#ref-mungle2020modelling)

    - At an individual patient level

      - To track patient’s MT progression during or after therapy.

      - To track patient’s cycle-by-cycle MT progression during or after
        the therapy using summary measure (SM) weighted mean (6MP\*MTX)
        vs weighted mean ANC parameters in a scatterplot.

      - Asses hematological toxicities - neutropenia, thrombocytopenia
        and anemia during MT.

      - Evaluate median time to first 6MP dose increase for the cohort.

      - Evaluate real time dosing decisions (stop, reduce or increase)
        by the physicians during MT based on MT dosing guidelines
        [\[2\]](#ref-mungle2020modelling)

    - At cohort level

      - SM plot to analyze MT for a given cohort defined by the user. SM
        is evaluated for each patient and plotted together to represent
        the entire cohort.

      - Compare SM for two or more cohort defined by the user to
        evaluate MT practice. Cohort comparisons can include clinical
        interventions, year-wise evolution, patients treated by two
        different set of physicians, or as user see it fit.

      - Asses hematological toxicities - neutropenia, thrombocytopenia
        and anemia during MT.

      - Plot depicting median time to first 6MP dose increase for the
        cohort.

      - Evaluate real time dosing decisions (stop, reduce or increase)
        by the physicians during MT based on MT dosing guidelines
        [\[2\]](#ref-mungle2020modelling)

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->

## Installation

You can install the development version of allMT from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tmungle/allMT")
```

## Ethics approval statement for data

The anonymized clinical data set refers to patients treated on the
ICiCLe-ALL-14 treatment protocol at the Tata Medical Center Kolkata
(institutional review board approval EC/TMC/12/13 for the treatment
protocol). Funding support for the ICiCLe-ALL-14 clinical study
(Clinical Trials Registry-India reference CTRI/2015/12/006434) was
provided by the National Cancer Grid (2016/001; 2016-) and the Indian
Council of Medical Research (79/159/2015/NCD-III; 2017-19)

## Contribution

By abiding to [Contributor Code of
Conduct](https://www.contributor-covenant.org/version/2/1/code_of_conduct/),
contribution to `allMT` in any form bug fixes/code modification/new code
development is welcomed.

## References

<div id="refs" class="references csl-bib-body">

<div id="ref-schmiegelow2014mercaptopurine" class="csl-entry">

<span class="csl-left-margin">\[1\] </span><span
class="csl-right-inline">K. Schmiegelow, S. N. Nielsen, T. L. Frandsen,
and J. Nersting, “Mercaptopurine/methotrexate maintenance therapy of
childhood acute lymphoblastic leukemia: Clinical facts and fiction,”
*Journal of pediatric hematology/oncology*, vol. 36, no. 7, p. 503,
2014.</span>

</div>

<div id="ref-mungle2020modelling" class="csl-entry">

<span class="csl-left-margin">\[2\] </span><span
class="csl-right-inline">T. D. Mungle, “Modelling clinical decision
processes to optimise maintenance chemotherapy in children with acute
lymphoblastic leukaemia,” PhD thesis, IIT Kharagpur, 2020.</span>

</div>

<div id="ref-das2022protocol" class="csl-entry">

<span class="csl-left-margin">\[3\] </span><span
class="csl-right-inline">N. Das *et al.*, “Protocol for ICiCLe-ALL-14
(InPOG-ALL-15-01): A prospective, risk stratified, randomised,
multicentre, open label, controlled therapeutic trial for newly
diagnosed childhood acute lymphoblastic leukaemia in india,” *Trials*,
vol. 23, no. 1, pp. 1–20, 2022.</span>

</div>

</div>

[^1]: The work was initiated as part of Tushar Mungle’s PhD work at IIT
    Kharagpur [\[2\]](#ref-mungle2020modelling) and then continued at
    TTCRC, Tata Medical Center, Kolkata.
