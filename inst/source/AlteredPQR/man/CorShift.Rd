\name{CorShift}
\alias{CorShift}
\title{Changes in correlation trends}
\usage{
  CorShift(samplesA = samplesGroupA, samplesB = samplesGroupB, shift_threshold = 0.6,
  writeTable = FALSE, min_cor_in_samples = 0.6, cor_signif = 0.01, 
  quant_data_all_local = quant_data_all, int_pairs_local = int_pairs)
}
\arguments{

  \item{samplesA}{Numeric vector with information on column numbers for the samples in the first group for the comparison.}

  \item{samplesB}{Numeric vector with information on column numbers for the samples in the second group for the comparison.}

  \item{shift_threshold}{Numeric value defining a minimum thresold of the Pearson correlation value between the two sample groups in order for them to be included in the results table.}

  \item{writeTable}{Option (T or F) to save results table as a text file.}

  \item{min_cor_in_samples}{Numeric value defining a minimum Pearson correlation value of protein quantities, which is taken as a threshold to consider that two proteins correlate in either of the two compared groups.}

 \item{cor_signif}{Numeric value defining a maximum allowed p-value for the Pearson correlation, which is taken as a threshold to consider that quantiative measurements for the two proteins correlate significantly in either of the two compared groups.}

   \item{quant_data_all_local}{A data matrix with quantiative proteomics measurements in which rows represent uniprot protein identifiers, and columns samples.}

  \item{int_pairs_local}{A data matrix with two columns. Rows contain information on interacting protein pairs.}

}
\value{cor_table table}
\description{
The function identifies instances in which two proteins correlate strongly only in one of the two studied groups.
}
\examples{
data("int_pairs", package = "AlteredPQR")
data("quant_data_all", package = "AlteredPQR")
samplesGroupA = 1:23
samplesGroupB = (1+23):(23+18)
cor_results = CorShift()
}
\author{
  Marija Buljan <marija.buljan@empa.ch>
}
