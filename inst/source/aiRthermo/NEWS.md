# Version 1.2.2

* The creation of the pdfs showing the Stuve diagram within the examples of CAPE_CIN function and the aiRthermo package were omitted to comply with CRAN's requirements.

# Version 1.2.1

* The Stuve_diagram function has been deeply updated. Line colours, types and widths are user-adjustable in this version. Additionally, the lines included in the plot are available until 20 hPa and the limits of both axis can be changed according to this value. 

* A previous bug introduced in version 1.2 that prevented the constant mixing ratio lines to appear in the diagram has been solved. Thanks to Dr. Clemens Drüe for reporting us about it. 

# Version 1.2

* Corrected some parts of the internal C function that computes CAPE and CIN that were executed when upToTop is TRUE. There is no need to change calling programs. The structure of return values hasn't changed, either.

* Some parts of the manual have been corrected.

# Version 1.1

* The TTindex function was updated after an error found in the code (reported by the CRAN team).