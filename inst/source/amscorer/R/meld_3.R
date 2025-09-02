#' @title MELD 3.0 Score Calculator
#' @description The Model for End-Stage Liver Disease (MELD) is a prognostic score measuring liver failure severity and estimating short-term survival in chronic liver disease patients.
#' Since 2002, it has prioritized organ allocation for liver transplants. MELDNa, the second version, is used by the Organ Procurement and Transplant Network today.
#'  Recently, MELD 3.0 was introduced to include additional variables, enhancing prediction accuracy and addressing gender disparity.
#' For more information, visit <https://www.mdcalc.com/calc/10437/model-end-stage-liver-disease-meld>.
#'
#' @usage meld_3(my_data)
#'
#' @details The function calculates the MELD 3.0 score based on the following parameters:
#' \itemize{
#'   \item \code{Sex:}  Sex (Male = 1 / Female = 0).
#'   \item \code{Bilirubin:} Numeric value representing the bilirubin level (mg/dL).
#'   \item \code{INR:} Numeric value representing the International Normalized Ratio (INR).
#'   \item \code{Creatinine:} Numeric value representing the creatinine level (mg/dL).
#'   \item \code{Albumin :} Numeric value representing the Albumin level (g/dL).
#'   \item \code{Sodium:} Numeric value representing the serum sodium level (mEq/L).
#' }
#'
#' @param my_data A data frame containing the following columns: A data frame containing the columns 'Sex', 'Creatinine', 'Bilirubin', 'INR', 'Sodium', and 'Albumin
#'
#' @return A data frame with the calculated MELD 3.0 score and risk classification of 90 day survival. Returns NA for cases with missing values.
#' @references Kamath et al. (2001) <doi:10.1053/jhep.2001.22172>
#' @export
#' @examples
#' my_data <- data.frame(
#' Sex = c(1, 0, 1),
#' Creatinine = c(1.5, 2.0, 3.1),
#' Bilirubin = c(1.2, 2.5, 1.8),
#' INR = c(1.1, 1.4, 2.0),
#' Sodium = c(135, 130, 140),
#' Albumin = c(3.0, 2.5, 3.5))
#'
#' meld_3(my_data)
#'

meld_3 <- function(my_data) {

  save_data <- my_data
  required_cols <- c("sex", "creatinine", "bilirubin", "inr", "sodium", "albumin")
  colnames(my_data) <- tolower(colnames(my_data))

  if (!all(required_cols %in% colnames(my_data))) {
    stop("The data frame must contain the columns 'Sex', 'Creatinine', 'Bilirubin', 'INR', 'Sodium', and 'Albumin'")
  }

  my_data$MELD_3 <- NA
  my_data$Estimated_90day_survival <- NA
  complete_cases <- stats::complete.cases(my_data[required_cols])

  if (any(complete_cases)) {
    my_data_complete <- my_data[complete_cases, ]
    my_data_complete$sex <- as.numeric(as.character(my_data_complete$sex))
    my_data_complete$sex  <- ifelse( my_data_complete$sex ==0,1, 0)
    my_data_complete$creatinine <- pmax(my_data_complete$creatinine, 1)
    adjusted_creatinine <- pmin(my_data_complete$creatinine, 3)
    my_data_complete$bilirubin <- pmax(my_data_complete$bilirubin, 1)
    my_data_complete$inr <- pmax(my_data_complete$inr, 1)
    adjusted_sodium <- pmin(pmax(my_data_complete$sodium, 125), 137)
    adjusted_albumin <- pmin(pmax(my_data_complete$albumin, 1.5), 3.5)

    MELD_3 <- 1.33 * my_data_complete$sex +
      4.56 * log(my_data_complete$bilirubin) +
      0.82 * (137 - adjusted_sodium) -
      0.24 * (137 - adjusted_sodium) * log(my_data_complete$bilirubin) +
      9.09 * log(my_data_complete$inr) +
      11.14 * log(adjusted_creatinine) +
      1.85 * (3.5 - adjusted_albumin) -
      1.83 * (3.5 - adjusted_albumin) * log(adjusted_creatinine) + 6

    MELD_3 <- pmin(MELD_3, 40)
    estimated_90day_survival <- paste0(round(0.946^(exp(0.17698 * MELD_3 - 3.56)) * 100, 2), " % survival")
    my_data <- save_data
    my_data$MELD_3[complete_cases] <- round(MELD_3)
    my_data$Estimated_90day_survival[complete_cases] <- estimated_90day_survival
  }

  return(my_data)
}

