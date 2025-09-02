#' @title Alternative Fistula Risk Score (a-FRS)
#' @description Calculates the alternative Fistula Risk Score (FRS) based on gland Texture, pancreatic duct diameter, and BMI.
#' Postoperative pancreatic fistula (POPF) is a significant complication following pancreatoduodenectomy (PD).
#' The Fistula Risk Score (FRS), initially proposed by Callery et al. in 2013, includes parameters such as gland texture, pancreatic duct diameter, and pathology.
#' Recent validations suggest reconsidering the inclusion of intraoperative blood loss in the FRS due to its minimal impact on POPF outcomes, exclusion from national audits like US-NSQIP and DPCA, and its dependence on surgical technique rather than patient factors.
#' Therefore, a blood loss-independent fistula risk score is essential for accurate risk assessment in future studies. Visit <https://www.evidencio.com/models/show/621> for more information.
#'
#' @usage a_FRS(my_data)
#'
#' @details This function calculates the risk of Postoperative Pancreatic Fistula (POPF) following pancreatoduodenectomy using the specified parameters:
#' \itemize{
#'   \item \code{Texture : } Nature of pancreatic texture (Firm or Hard = 0 or Soft = 1)
#'   \item \code{BMI :} Body mass index in Kg/m²
#'   \item \code{PD_size :} Size of the pancreatic duct in mm
#' }
#' @param my_data A data frame containing the columns Texture ("soft = 1" or "hard = 0"), PD_size Numeric (Pancreatic duct diameter in millimeters), and BMI Numeric (Body mass index in kg/m²).
#' @return A data frame with the calculated alternative Fistula Risk Score (FRS) and risk classification of CR-POPF following pancreatoduodenectomy. Returns NA for cases with missing values.
#' @references
#' Mungroop et al. (2019) <doi:10.1097/SLA.0000000000002620>
#' @export
#' @examples
#' my_data <- data.frame(
#'   ID = 1:4,
#'   Texture = c(0, 1, 0, 0),
#'   BMI = c(22, 25, 30, 20),
#'   PD_size = c(5, 10, 1, 2)
#' )
#' a_FRS(my_data)

a_FRS <- function(my_data) {
  save_data <- my_data
  colnames_lower <- tolower(colnames(my_data))
  names(my_data) <- colnames_lower
  required_cols <- c("texture", "bmi", "pd_size")
  missing_cols <- setdiff(required_cols, colnames_lower)
  if (length(missing_cols) > 0) {
    stop(paste("The data frame must contain the columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check 'texture' column values
  my_data$texture <- as.numeric(as.character(my_data$texture))
  if (!all(my_data$texture %in% c(0, 1), na.rm = TRUE)) {
    stop("The 'texture' column must be binary (0=hard or 1=soft)")
  }

  # Ensure BMI and PD_size are within valid ranges
  my_data$bmi <- pmin(pmax(my_data$bmi, 10), 50)
  my_data$pd_size <- pmin(pmax(my_data$pd_size, 0), 5)

  # Adjust PD_size if it exceeds 5
  adjust_PD_size <- ifelse(my_data$pd_size > 5, 5, my_data$pd_size)

  # Calculate POPF risk
  M <- -3.136 + 0.947 * my_data$texture + 0.0679 * my_data$bmi - 0.385 * adjust_PD_size
  POPF_risk <- round(100 * exp(M) / (1 + exp(M)), 2)

  # Prepare output in original case format
  my_data$POPF_risk <- NA
  my_data$Risk_category <- NA
  complete_cases <- stats::complete.cases(my_data[, required_cols])
  my_data <-  save_data
  my_data$POPF_risk[complete_cases] <- paste0(POPF_risk[complete_cases], "%")
  my_data$Risk_category[complete_cases] <- ifelse(POPF_risk[complete_cases] < 5, "Low risk",
                                                  ifelse(POPF_risk[complete_cases] <= 20, "Intermediate risk", "High risk"))

  return(my_data)
}
