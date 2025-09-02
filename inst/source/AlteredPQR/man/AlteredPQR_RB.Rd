\name{AlteredPQR_RB}
\alias{AlteredPQR_RB}
\title{Identification of outlier log ratios in quantitative proteomic measurements for investigated protein pairs.}
\usage{
AlteredPQR_RB (modif_z_score_threshold = 3.5, fraction_of_samples_threshold = 0.10,
modif = 1, filter_variable_in_ref_set = "NO", write_table = "NO", print_recomm = "NO", 
quant_data_all_local = quant_data_all, 
cols_with_reference_data_local = cols_with_reference_data)
}
\arguments{
  \item{modif_z_score_threshold}{Numeric value defining a threshold to consider log ratio in the tested sample as a outlier.}

  \item{fraction_of_samples_threshold}{Numeric value defining a fraction of samples that need to be classified as outliers for the protein pair to be included in the results table.}

  \item{modif}{Numeric value defining a modifier value that is used to idenify proteins in the pair that contributed to the outlier signal. The higher the variable 'modif', the higher the modified z score value the single protein needs to have (compared to its own values in the reference samples) to be considered as an outlier in at least half of the samples classified as outliers for the protein pair. Proteins classified as outliers in their own measuremnts are listed as 'driving the signal' in the reults table.}

  \item{filter_variable_in_ref_set}{Option (T or F) to exclude from the results table protein pairs that strongly varied in the reference samples.}

  \item{write_table}{Option (T or F) to save results table as a text file.}

 \item{print_recomm}{Option (T or F) to get information on the distribution of all modified z-scores in the test samples and recmmendation on the thresholds for the user defined qunatitative proteomics dataset.}

  \item{quant_data_all_local}{A data matrix with quantiative proteomics measurements in which rows represent uniprot protein identifiers, and columns samples.}

  \item{cols_with_reference_data_local}{Numeric vector with information on columns that contain reference data.}
}
\value{representative_pairs table}
\description{
The function identifies outliers in protein pair log ratios compared to the reference set of measurements.
}
\examples{
data("int_pairs", package = "AlteredPQR")
data("quant_data_all", package = "AlteredPQR")
cols_with_reference_data = 1:23
RepresentativePairs = AlteredPQR_RB()
}
\author{
  Marija Buljan <marija.buljan@empa.ch>
}
