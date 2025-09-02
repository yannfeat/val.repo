#' @title Updated Alternative Fistula Risk Score (ua-FRS)
#' @description Calculates the alternative Fistula Risk Score (FRS) based on gland Texture, pancreatic duct diameter, BMI, and Sex.
#' Objective is to validate and optimize the alternative Fistula Risk Score (ua-FRS) specifically for patients undergoing minimally invasive pancreatoduodenectomy (MIPD) using data from a comprehensive pan-European cohort.
#' Visit <https://www.evidencio.com/models/show/1408> for more information.
#'
#'
#' @details This function calculates the risk of Postoperative Pancreatic Fistula (POPF) following pancreatoduodenectomy using the specified parameters:
#' \itemize{
#'   \item \code{Texture : } Nature of pancreatic texture (Not soft = 0 or Soft = 1)
#'   \item \code{BMI :} Body mass index in Kg/m²
#'   \item \code{PD_size :} Size of the pancreatic duct in mm (truncated to 5)
#'   \item \code{Sex :} Sex (Female = 0, Male = 1)
#' }
#' @param my_data A data frame containing the columns Sex (Female = 0, Male = 1), Texture ("soft = 1" or "hard = 0"), PD_size Numeric (Pancreatic duct diameter in millimeters), and BMI Numeric (Body mass index in kg/m²).
#'
#' @return A modified data frame with the calculated alternative Fistula Risk Score (FRS) and risk classification of CR-POPF following pancreatoduodenectomy. Returns NA for cases with missing values.
#' @references
#' Mungroop et al. (2019) <doi:10.1097/SLA.0000000000003234>.
#'
#' @export
#' @examples
#' my_data <- data.frame(
#'   ID = 1:4,
#'   Sex = c(0, 0, 1, 1),
#'   Texture = c(0, 1, 0, 0),
#'   BMI = c(22, 25, 30, 20),
#'   PD_size = c(5, 10, 1, 2)
#' )
#' ua_FRS(my_data)


ua_FRS <- function(my_data) {
  save_data <- my_data
  colnames_lower <- tolower(colnames(my_data))
  names(my_data) <- colnames_lower

  required_cols <- c("texture", "bmi", "pd_size", "sex")
  missing_cols <- setdiff(required_cols, colnames_lower)
  if (length(missing_cols) > 0) {
    stop(paste("The data frame must contain the columns:", paste(missing_cols, collapse = ", ")))
  }

  my_data$texture <- as.numeric(as.character(my_data$texture))
  my_data$sex <- as.numeric(as.character(my_data$sex))

  if (!all(my_data$texture %in% c(0, 1), na.rm = TRUE)) {
    stop("The 'texture' column must be binary (0 = Not soft, 1 = Soft)")
  }
  if (!all(my_data$sex %in% c(0, 1), na.rm = TRUE)) {
    stop("The 'sex' column must be binary (0 = female, 1 = male)")
  }

  my_data$POPF_risk <- NA
  my_data$Risk_category <- NA

  my_data$bmi <- pmin(pmax(my_data$bmi, 10), 50)
  my_data$pd_size <- pmin(pmax(my_data$pd_size, 0), 5)

  adjust_PD_size <- ifelse(my_data$pd_size > 5, 5, my_data$pd_size)

  M <- -2.36 + 0.95 * my_data$texture + 0.07 * my_data$bmi - 0.39 * adjust_PD_size + 0.64 * my_data$sex
  POPF_risk <- round(100 * exp(M) / (1 + exp(M)), 2)

  complete_cases <- stats::complete.cases(my_data[, required_cols])

  my_data <- save_data
  my_data$POPF_risk[complete_cases] <- paste0(POPF_risk[complete_cases], "%")

  my_data$Risk_category[complete_cases] <- ifelse(POPF_risk[complete_cases] < 5, "Low risk",
                                                  ifelse(POPF_risk[complete_cases] <= 20, "Intermediate risk", "High risk"))

  return(my_data)
}

