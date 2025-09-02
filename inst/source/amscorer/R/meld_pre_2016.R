#' @title MELD Score (Original, Pre-2016, Model for End-Stage Liver Disease)
#' @description Calculates the MELD score based on the original pre-2016 formula, which does not include serum sodium levels, as used by non-US transplant societies.
#' Visit <https://www.mdcalc.com/calc/2693/meld-score-original-pre-2016-model-end-stage-liver-disease> for more information.
#'
#' @usage meld_pre_2016(my_data)
#'
#' @details This function calculates the MELD score using the following parameters:
#' \itemize{
#'   \item \code{Bilirubin : } Numeric value representing the bilirubin level (mg/dL).
#'   \item \code{INR :} Numeric value representing the International Normalized Ratio (INR).
#'   \item \code{Creatinine :} Numeric value representing the creatinine level (mg/dL).
#'   \item \code{Hemodialysis :} Logical value indicating if the patient is on hemodialysis (0 = No, 1 = Yes).
#' }
#'
#' @param my_data A data frame containing columns 'Creatinine', 'Bilirubin', 'INR', and 'Hemodialysis'.
#'
#' @return A modified data frame with the calculated MELD score and risk classification of three-month mortality. Returns NA for cases with missing values.
#' @references
#' Kamath et al. (2001) <doi:10.1053/jhep.2001.22172>
#' @export
#' @examples
#' my_data <- data.frame(
#'   Hemodialysis = c(0, 0, 1),
#'   Creatinine = c(1.2, 0.9, 1.5),
#'   Bilirubin = c(0.7, 1.1, 0.9),
#'   INR = c(1.0, 1.2, 1)
#' )
#'
#' meld_pre_2016(my_data)


meld_pre_2016 <- function(my_data) {
  save_data <- my_data
  required_cols <- c("creatinine", "bilirubin", "inr", "hemodialysis")
  colnames_lower <- tolower(colnames(my_data))

  if (!all(tolower(required_cols) %in% colnames_lower)) {
    stop("The data frame must contain the columns 'Creatinine', 'Bilirubin', 'INR', and 'Hemodialysis'.")
  }

  my_data <- stats::setNames(my_data, tolower(colnames(my_data)))

  my_data$MELD_score <- NA
  my_data$three_month_mortality <- NA
  complete_cases <- stats::complete.cases(my_data[required_cols])

  if (any(complete_cases)) {
    my_data$creatinine <- ifelse(my_data$hemodialysis == 1, 4, my_data$creatinine)
    my_data$creatinine <- ifelse(my_data$creatinine < 1, 1, my_data$creatinine)
    my_data$creatinine <- ifelse(my_data$creatinine > 4, 4, my_data$creatinine)
    my_data$bilirubin <- ifelse(my_data$bilirubin < 1, 1, my_data$bilirubin)
    my_data$inr <- ifelse(my_data$inr < 1, 1, my_data$inr)

    MELD_score <- round(10.00 * ((0.957 * log(my_data$creatinine[complete_cases])) +
                                   (0.378 * log(my_data$bilirubin[complete_cases])) +
                                   (1.12 * log(my_data$inr[complete_cases]))) + 6.43)
    MELD_score <- pmin(MELD_score, 40)

    three_month_mortality <- cut(MELD_score,
                                 breaks = c(-Inf, 9, 19, 29, 39, 40, Inf),
                                 labels = c("1.9% mortality",
                                            "6.0% mortality",
                                            "19.6% mortality",
                                            "52.6% mortality",
                                            "71.3% mortality",
                                            "71.3% mortality"),
                                 right = TRUE)
    my_data <- save_data
    my_data$MELD_score[complete_cases] <- round(MELD_score)
    my_data$three_month_mortality[complete_cases] <- as.character(three_month_mortality)
  }

  return(my_data)
}




