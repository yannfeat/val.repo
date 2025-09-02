#' @title MELD-Na (UNOS/OPTN) Score Calculator
#' @description Calculates the MELD-Na score for patients greater than or equal to 12 years, incorporating serum sodium levels. This score is used for assessing the severity of end-stage liver disease for transplant planning. Note that since January 2016, the MELD score calculation includes the serum sodium level.
#' For more information, visit <https://www.mdcalc.com/calc/78/meld-score-model-end-stage-liver-disease-12-older#evidence>.
#'
#' @usage meld_NA(my_data)
#'
#' @details The function calculates the MELD-Na score based on the following parameters:
#' \itemize{
#'   \item \code{Bilirubin:} Numeric value representing the bilirubin level (mg/dL).
#'   \item \code{INR:} Numeric value representing the International Normalized Ratio (INR).
#'   \item \code{Creatinine:} Numeric value representing the creatinine level (mg/dL).
#'   \item \code{Hemodialysis:} Logical value indicating if the patient is on hemodialysis (0 = No, 1 = Yes).
#'   \item \code{Sodium:} Numeric value representing the serum sodium level (mEq/L).
#' }
#'
#' @param my_data A data frame containing columns 'Creatinine', 'Bilirubin', 'INR', 'Hemodialysis', and 'Sodium'.
#'
#' @return A modified data frame (my_data) with the calculated MELD-Na score and risk classification of three-month mortality. Returns NA for cases with missing values.
#' @references Kamath et al. (2001) <doi:10.1053/jhep.2001.22172>
#' @export
#' @examples
#' my_data <- data.frame(
#'   Creatinine = c(1.2, 2.5, 3),
#'   Bilirubin = c(0.5, 1.0, 2.1),
#'   INR = c(1.1, 1.5, 1.8),
#'   Sodium = c(130, 140, 125),
#'   Hemodialysis = c(0, 1, 0)
#' )
#' meld_NA(my_data)
#'
#'
meld_NA <- function(my_data) {
  save_data <- my_data
  required_columns <- c("Creatinine", "Bilirubin", "INR", "Sodium", "Hemodialysis")

  # Standardize column names to lowercase
  colnames_lower <- tolower(colnames(my_data))
  required_columns_lower <- tolower(required_columns)

  # Check if all required columns are present, ignoring case
  if (!all(required_columns_lower %in% colnames_lower)) {
    stop("The dataframe must contain the columns 'Creatinine', 'Bilirubin', 'INR', 'Sodium', and 'Hemodialysis'.")
  }

  colname_mapping <- stats::setNames(colnames(my_data), colnames_lower)

  my_data <- my_data[, colname_mapping[required_columns_lower]]
  names(my_data) <- required_columns

  my_data$MELD_na_score <- NA
  my_data$Three_Month_Mortality <- NA
  complete_cases <- stats::complete.cases(my_data[required_columns])

  if (any(complete_cases)) {
    my_data_complete <- my_data[complete_cases, ]

    # Calculate initial MELD score
    score_initial <- amscorer::meld_pre_2016(my_data_complete)$MELD_score
    adjusted_Sodium <- pmin(pmax(my_data_complete$Sodium, 125), 137)
    score_final <- score_initial +
      1.32 * (137 - adjusted_Sodium) -
      0.033 * score_initial * (137 - adjusted_Sodium)
    score_final <- round(pmin(ifelse(score_initial > 11, score_final, score_initial), 40))
    breaks <- c(-Inf, 9, 19, 29, 39, 40, Inf)
    labels <- c("1.9% mortality", "6.0% mortality", "19.6% mortality",
                "52.6% mortality", "71.3% mortality", "71.3% mortality")
    Three_Month_Mortality <- cut(score_final, breaks = breaks, labels = labels, right = TRUE)
    my_data <- save_data
    my_data$MELD_na_score[complete_cases] <- round(score_final)
    my_data$Three_Month_Mortality[complete_cases] <- as.character(Three_Month_Mortality)
  }

  my_data$MELD_na_score[!complete_cases] <- NA
  my_data$Three_Month_Mortality[!complete_cases] <- NA

  return(my_data)
}
