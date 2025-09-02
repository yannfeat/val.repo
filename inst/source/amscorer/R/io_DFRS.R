#' @title Calculate Intra-operative Distal Fistula Risk Score (D-FRS)
#' @description Calculates the intra-operative Distal Fistula Risk Score (D-FRS) to estimate the probability of clinically relevant (grade B/C) postoperative pancreatic fistula (POPF) according to the 2016 ISGPS definition.
#' Visit <https://www.evidencio.com/models/show/2587#> for more information.
#'
#' @usage io_DFRS(my_data)
#'
#' @details The intra-operative Distal Fistula Risk Score (D-FRS) estimates the probability of clinically relevant (grade B/C) postoperative pancreatic fistula (POPF) as per the 2016 ISGPS definition. It is the initial risk assessment tool specific to POPF following distal pancreatectomy,
#' enabling tailored treatment and performance comparison across three risk categories.
#' @param my_data A data frame containing the following columns:
#'   - Texture: Categorical variable ("soft/normal = 1" or "hard/fibrotic = 0")
#'   - PD_size: Numeric (Pancreatic duct diameter in millimeters)
#'   - BMI: Numeric (Body mass index in kg/m2)
#'   - PT: Numeric (Pancreatic thickness in mm)
#'   - OP_time: Numeric (Operating time in minutes)
#' @return A data frame with the Intra-operative (D-FRS) score and risk classification of POPF. Returns NA for cases with missing values.
#' @references
#' Pastena et al. (2023) <doi:10.1097/SLA.0000000000005497>
#' @export
#' @examples
#' my_data <- data.frame(
#'   ID = 1:4,
#'   BMI = c(NA, 25, 30, 10),
#'   PT = c(5, 43, 1, 20),
#'   PD_size = c(100, 0, 1, 19),
#'   OP_time = c(NA, 20, 605, NA),
#'   Texture = c(0, 1, 0, 1)
#')
#' io_DFRS(my_data)
#'

io_DFRS <- function(my_data) {
  save_data <- my_data
  required_cols <- c("texture", "bmi", "pd_size", "pt", "op_time")
  colnames_lower <- tolower(colnames(my_data))
  missing_cols <- setdiff(required_cols, colnames_lower)
  if (length(missing_cols) > 0) {
    stop(paste("The data frame must contain the columns:", paste(missing_cols, collapse = ", ")))
  }
  colname_mapping <- stats::setNames(colnames(my_data), colnames_lower)
  my_data <- my_data[, colname_mapping[required_cols]]
  colnames(my_data) <- required_cols
  my_data$POPF_risk <- NA
  my_data$risk_category <- NA
  complete_cases <- stats::complete.cases(my_data[, required_cols])
  my_data$texture <- as.numeric(as.character(my_data$texture))
  if (!all(my_data$texture %in% c(0, 1), na.rm = TRUE)) {
    stop("The 'Texture' column must be binary (0 or 1)")
  }
  my_data$bmi <- pmin(pmax(my_data$bmi, 10), 50)
  my_data$pt <- pmin(pmax(my_data$pt, 1), 50)
  my_data$pd_size <- pmin(pmax(my_data$pd_size, 0), 20)
  my_data$op_time <- pmin(pmax(my_data$op_time, 1), 600)
  if (any(complete_cases)) {
    M <- with(my_data[complete_cases, ], -11.923 + 1.592 * texture + 0.107 * bmi + 0.783 * pd_size + 0.199 * pt + 0.005 * op_time)
    POPF_risk <- round(100 * exp(M) / (1 + exp(M)), 2)
    risk_category <- ifelse(POPF_risk < 10, "Low risk",
                            ifelse(POPF_risk <= 25, "Intermediate risk", "High risk"))
    my_data <- save_data
    my_data$POPF_risk[complete_cases] <- paste0(POPF_risk, "%")
    my_data$risk_category[complete_cases] <- risk_category
  }

  return(my_data)
}
