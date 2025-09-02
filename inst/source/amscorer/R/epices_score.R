#' @title Calculate EPICES Score
#'
#' @description This function calculates the EPICES score, which is used to investigate the relationship between social precariousness and risk factors.
#'  The EPICES score is an individual and quantitative scale based on 11 questions regarding material and social problems.
#' Visit <https://www.santepubliquefrance.fr/docs/le-score-epices-un-score-individuel-de-precarite.-construction-du-score-et-mesure-des-relations-avec-des-donnees-de-sante-dans-une-population-de> for more information.
#
#' @param my_data A data frame containing the necessary columns for the EPICES score calculation (11 columns).
#' Yes/no responses should be coded as yes-1, no-0.
#' @param prefix A string prefix for the column names (default is "epices").
#' @param replace_na_with_zero A boolean indicating whether to replace NA values with zero (default is TRUE).
#'
#' @details The data frame should contain the EPICES items ordered from 1 to 11::
#' \itemize{
#' \item \code{epices_1 :} Do you sometimes meet with a social worker?
#' \item \code{epices_2 :} Do you have supplementary health insurance?
#' \item \code{epices_3 :} Do you live with a partner?
#' \item \code{epices_4 :} Do you own your home?
#' \item \code{epices_5 :} Are there times during the month when you experience real financial difficulties in meeting your needs (food, rent, electricity, etc.)?
#' \item \code{epices_6 :} Have you done any sports in the past 12 months?
#' \item \code{epices_7 :} Have you been to a show in the past 12 months?
#' \item \code{epices_8 :} Have you gone on vacation in the past 12 months?
#' \item \code{epices_9 :} In the past 6 months, have you had contact with family members other than your parents or children?
#' \item \code{epices_10 :} In case of difficulties, are there people in your surroundings whom you can rely on to accommodate you for a few days if needed?
#' \item \code{epices_11 :} In case of difficulties, are there people in your surroundings whom you can rely on to provide you with material assistance?
#' }
#' @return A data frame with an additional column "epices_score" containing the calculated EPICES scores. Returns NA for cases with missing values.
#' @references
#' Sass et al. (2006) <doi:10.1007/s10332-006-0131-5>,
#' @export
#' @examples
#'my_data <- data.frame(
#'epices_1 = c(1, 0, 1),
#'epices_2 = c(0, 1, 1),
#'epices_3 = c(0, 0, 0),
#'epices_4 = c(1, 0, 0),
#'epices_5 = c(0, 1, 0),
#'epices_6 = c(1, 0, 1),
#'epices_7 = c(0, 1, 0),
#'epices_8 = c(0, 0, 1),
#'epices_9 = c(1, 1, 0),
#'epices_10 = c(0, 0, 1),
#'epices_11 = c(1, 0, NA)
#')
#' epices_score(my_data, prefix = "epices", replace_na_with_zero = FALSE)

epices_score <- function(my_data, prefix = "epices", replace_na_with_zero = FALSE) {

  if (replace_na_with_zero) warning("Missing values are considered as zero")
  required_columns <- paste0(prefix, "_", 1:11)
  missing_columns <- setdiff(required_columns, names(my_data))
  if (length(missing_columns) > 0) {
    stop(paste("The following columns are missing in the data frame:", paste(missing_columns, collapse = ", ")))
  }
  for (column in required_columns) {
    if (any(my_data[[column]] > 1, na.rm = TRUE)) {
      stop(paste("The column", column, "contains non-binary values (not 0/1) and will be considered as missing"))
    }
  }

  for (column in required_columns) {
    if (replace_na_with_zero) {
      my_data[[column]][is.na(my_data[[column]])] <- 0
    }
    my_data[[column]] <- ifelse(my_data[[column]] == "1", 1, ifelse(my_data[[column]] == "0", 0, NA))
  }
  coefficients <- c(10.06, -11.83, -8.28, -8.28, 14.8, -6.51, -7.10, -7.10, -9.47, -9.47, -7.1)

  my_data$epices_score <- 75.14 + rowSums(sapply(1:length(required_columns), function(i) {
    my_data[[required_columns[i]]] * coefficients[i]
  }))

  if (!replace_na_with_zero) {
    na_rows <- apply(my_data[required_columns], 1, function(row) any(is.na(row)))
    my_data$epices_score[na_rows] <- NA
  }
  return(my_data)
}



