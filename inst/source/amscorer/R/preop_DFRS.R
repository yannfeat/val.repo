#' @title Preoperative Distal Fistula Risk Score (D-FRS)
#' @description The preoperative fistula risk score estimates the probability of clinically relevant (grade B/C) postoperative pancreatic fistula (POPF) based on the 2016 ISGPS definition.
#' It is the first validated risk score for POPF after distal pancreatectomy, categorizing patients into three risk groups for personalized treatment and benchmarking.
#' For more information, visit <https://www.evidencio.com/models/show/2573>.
#'
#' @usage preop_DFRS(my_data)
#'
#' @details The function calculates the (D-FRS)score based on the following parameters:
#' \itemize{
#'   \item \code{PT:} Size of the pancreatic thickness in mm
#'   \item \code{PD_size:} Size of the pancreatic duct in mm
#' }
#'
#' @param my_data A data frame containing the columns PD_size Numeric (Pancreatic duct diameter in millimeters) and PT (Pancreatic thickness (in mm))
#'
#' @return  A data frame with the (D-FRS) score and risk classification of POPF. Returns NA for cases with missing values.
#' @references
#' Pastena et al. (2023) <doi:10.1097/SLA.0000000000005497>
#' @export
#' @examples
#' my_data <- data.frame(ID = 1:4,
#' PT = c(5, 43, 1, 4),
#' PD_size = c(25, 5, 1, 19) )
#'
#'preop_DFRS(my_data)



preop_DFRS <- function(my_data) {
  required_columns <- c("PD_size", "PT")

  # Convert column names to lowercase to handle case insensitivity
  colnames_lower <- tolower(colnames(my_data))

  if (!all(tolower(required_columns) %in% colnames_lower)) {
    stop("The data frame must contain the columns: pancreatic duct size ('PD_size') and pancreatic thickness ('PT')")
  }

  save_data <- my_data
  my_data$POPF_risk <- NA
  my_data$risk_category <- NA
  complete_cases <- stats::complete.cases(my_data[required_columns])
  limits <- list(PD_size = c(min = 0, max = 20), PT = c(min = 1, max = 50))

  # Adjust values that are out of specified limits
  for (col in names(limits)) {
    if (any(my_data[[col]][complete_cases] < limits[[col]]["min"] | my_data[[col]][complete_cases] > limits[[col]]["max"])) {
      warning(paste("Some values of", col, "are very high or very low"))
      my_data[[col]][my_data[[col]] < limits[[col]]["min"]] <- limits[[col]]["min"]
      my_data[[col]][my_data[[col]] > limits[[col]]["max"]] <- limits[[col]]["max"]
    }
  }

  M <- -4.211 + 0.388 * my_data$PD_size[complete_cases] + 0.131 * my_data$PT[complete_cases]
  POPF_risk <- round(100 * exp(M) / (1 + exp(M)))

  risk_category <- ifelse(POPF_risk < 10, "Low risk",
                          ifelse(POPF_risk <= 25, "Intermediate risk", "High risk"))

  my_data <- save_data
  my_data$POPF_risk[complete_cases] <- paste0(POPF_risk, "%")
  my_data$risk_category[complete_cases] <- risk_category
  return(my_data)
}

