## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE # Ensure caching is appropriately set
)

options(repos = c(CRAN = "https://cran.rstudio.com"))

# Load the aLBI package (required for vignette to access functions)
library(aLBI)

# Suppress warnings during vignette build for cleaner output
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(stats))

## ----installing_package-------------------------------------------------------
# You can install the package using the following commands in your R session:
# Install devtools if you haven't already
# install.packages("devtools")
# # Install dplyr if you haven't already
# install.packages("dplyr")
# # Install readxl if you haven't already
# install.packages("readxl")
# Install ggplot2 if you haven't already
# install.packages("ggplot2")
# Install openxlsx if you haven't already
# install.packages("openxlsx")
# install aLBI package from CRAN
# install.packages("aLBI")
# Install the most updated version of aLBI package from GitHub
# devtools::install_github("Ataher76/aLBI")


## ----package_management-------------------------------------------------------
# Check if required packages are installed and load them
# Check if required packages are installed and load them
required_packages <- c("aLBI", "readxl", "openxlsx", "dplyr", "devtools", "ggplot2")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(paste("Package", pkg, "is required but not installed."))
  } else {
    library(pkg, character.only = TRUE)
  }
}


# Load the aLBI package
library(aLBI)
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(devtools)

## ----data_preparation---------------------------------------------------------
library(readxl)
# Load your length data from the system file
lenfreq_path <- system.file("exdata", "ExData.xlsx", package = "aLBI")
print(lenfreq_path)  # Check the generated path

if (lenfreq_path == "") {
  stop("The required file ExData.xlsx is missing. Please check the inst/extdata directory.")
}

# load the lenght frequency data
lenght_data <- readxl::read_excel(lenfreq_path)
print(lenght_data)  # check the 
# replace with your data directory



## ----FrequencyTable_Output----------------------------------------------------
# Running the FrequencyTable function
freqTable <- FrequencyTable(data = lenght_data, bin_width = NULL, Lmax = NULL, output_file = "FrequencyTable_Output.xlsx")

# Viewing the results
freqTable$lfqTable  # Display the frequency table
freqTable$lfreq     # Display the summarized frequencies with upper length ranges


## ----data_example-------------------------------------------------------------
library(readxl)
# Load your length-frequency data from the system file
lenfreq_path <- system.file("exdata", "LC.xlsx", package = "aLBI")
print(lenfreq_path)  # Check the generated path

if (lenfreq_path == "") {
  stop("The required file LC.xlsx is missing. Please check the inst/extdata directory.")
}

# load the lenght frequency data
lenfreq_data <- readxl::read_excel(lenfreq_path)
print(lenfreq_data)  # check the data 
# replace with your data directory

## ----FishPar_Output-----------------------------------------------------------
# Running the FishPar function
results <- FishPar(data = lenfreq_data, resample = 1000, progress = FALSE, Linf = NULL, Linf_sd = 0.5, Lmat = NULL, Lmat_sd = 0.5)

# Viewing the results
results$estimated_length_par
results$estimated_froese_par
results$forese_ind_vs_target
results$Total_ind
results$LM_ratio
results$Pobj


## ----FishSS_Example-----------------------------------------------------------
# Load the stock status criteria data
cpdata_path <- system.file("exdata", "cpdata.xlsx", package = "aLBI")
print(cpdata_path) #check if the path exist

if (cpdata_path == "") {
  stop("The required file cpdata.xlsx is missing. Please check the inst/extdata directory.")
}
# loading the cope and punt table
cpdata <- readxl::read_excel(cpdata_path)
print(cpdata)

# Running the FishSS function
stock_status <- FishSS(data = cpdata,
                       LM_ratio = results$LM_ratio,
                       Pobj = results$Pobj,
                       Pmat = results$estimated_froese_par[1, 2],
                       Popt = results$estimated_froese_par[2, 2])

# Viewing the stock status
stock_status


## ----LWR_Example--------------------------------------------------------------
# Load the stock status criteria data
LWdata_path <- system.file("exdata", "LWdata.xlsx", package = "aLBI")
print(LWdata_path) #check if the path exist

if (LWdata_path == "") {
  stop("The required file LWdata.xlsx is missing. Please check the inst/extdata directory.")
}
# loading the cope and punt table
LWdata <- readxl::read_excel(LWdata_path)
print(LWdata)

# Running the LWR function
lwr_result <- LWR(data = LWdata,
                log_transform = TRUE,
                point_col = "black",
                line_col = "red",
                shade_col = "red",
                point_size = 2,
                line_size = 1,
                alpha = 0.2,
                main = "Length-Weight Relationship",
                xlab = NULL,
                ylab = NULL,
                save_output = TRUE)

# Viewing the stock status
lwr_result


