#' @title Calculate Charlson Comorbidity Index
#'
#' @description The Charlson Comorbidity Index is a tool used to assess the risk of mortality in patients with multiple comorbidities. This function calculates the Charlson Comorbidity Index (CCI) using various binary or ternary columns in a data frame.
#'
#' @param my_data A data frame containing the necessary columns for score calculation (17 columns). The columns should have values 0, 1, 2, or NA. Yes/no responses should be coded as yes-1, no-0.
#' For the column "liver_disease (None/Mild/Moderate to severe)", encode as None-0, Mild-1, Moderate to severe-2.
#' For the column "diabetes_mellitus (None or diet-controlled/uncomplicated/End-organ)", encode as None or diet-controlled-0, uncomplicated-1, End-organ-2.
#' For the column "solid_tumor (None/Localized/Metastatic)", encode as None-0, Localized-1, Metastatic-2.
#' @param replace_na_with_zero A boolean indicating whether to replace NA values with zero (no-0), except for age (default is FALSE).
#'
#' @details The data frame should contain the following CCI items:
#' \itemize{
#' \item \code{age} : Age
#' \item \code{mi} : Myocardial infarction
#' \item \code{chf} : Congestive heart failure
#' \item \code{pvd} : Peripheral vascular disease
#' \item \code{cevd} : Cerebrovascular accident with minor or no residua and transient ischemic attacks
#' \item \code{dementia} : Dementia
#' \item \code{cpd} : Chronic obstructive pulmonary disease
#' \item \code{ctd} : Connective tissue disease
#' \item \code{pud} : Peptic ulcer disease
#' \item \code{liver_disease} : Liver disease
#' \item \code{diabetes_mellitus} : Diabetes mellitus
#' \item \code{hp} : Hemiplegia
#' \item \code{ckd} : Moderate to severe Chronic kidney disease
#' \item \code{solid_tumor} : Solid tumor
#' \item \code{leuk} : Leukemia
#' \item \code{lym} : Lymphoma
#' \item \code{aids} : AIDS
#' }
#'
#' @return A data frame (my_data) with an additional column 'cci_score' containing the calculated scores and an additional column 'estimated_10_year_survival'. Returns NA for cases with missing values.
#' @references
#' Charlson et al. (1987) <doi:10.1016/0021-9681(87)90171-8>
#' @export
#' @examples
#' set.seed(123)
#' n <- 10
#' my_data <- data.frame(
#'   age = sample(30:90, n, replace = TRUE), # age
#'   mi = sample(0:1, n, replace = TRUE), # Myocardial infraction
#'   chf = sample(0:1, n, replace = TRUE), # Congestive heart failure
#'   pvd = sample(0:1, n, replace = TRUE), # preripheral vascular disease
#'   cevd = sample(0:1, n, replace = TRUE), # Cerebrovascular accident or Transient ischemic attack
#'   dementia = sample(0:1, n, replace = TRUE), # Dematia
#'   cpd = sample(0:1, n, replace = TRUE),# Chronic obstructive pulmonary disease
#'   ctd = sample(0:1, n, replace = TRUE),# Connective tissue disease
#'   pud = c(sample(0:1, (n-1), replace = TRUE) , NA), # peptide ulcer disease
#'   liver_disease = sample(0:2, n, replace = TRUE), #Liver disease(None,Mild,Moderate to severe)
#'   diabetes_mellitus = sample(0:2, n, replace = TRUE),#Diabetes(None,uncomplicated,End-organ)
#'   hp = sample(0:1, n, replace = TRUE), # Hemipledia
#'   ckd = sample(0:1, n, replace = TRUE), #Moderate to severe Chronic kidney disease
#'   solid_tumor = sample(0:2, n, replace = TRUE), #Solid tumor(None,Localized,Metastatic)
#'   leuk = sample(0:1, n, replace = TRUE), # Leukemia
#'   lym =  c(sample(0:1, (n-2), replace = TRUE) , c(NA , NA)), # Lymphoma
#'   aids = sample(0:1, n, replace = TRUE) ) # AIDS
#'
#' cci(my_data, replace_na_with_zero = FALSE)
#'
#'

cci <- function(my_data, replace_na_with_zero = FALSE) {
  save_data <- my_data
  estimated_10_year_survival <- NA
  colnames(my_data) <- tolower(colnames(my_data))
  required_columns <- c("age", "mi", "chf", "pvd", "cevd", "dementia", "cpd", "ctd", "pud",
                        "liver_disease", "diabetes_mellitus", "hp", "ckd", "solid_tumor",
                        "leuk", "lym", "aids")

  if (!all(required_columns %in% names(my_data))) {
    stop("The data frame must contain the following columns: ", paste(required_columns, collapse = ", "))
  }

  valid_values <- function(x) all(is.na(x) | x %in% c(0, 1, 2))
  invalid_values <- sapply(my_data[, !names(my_data) %in% "age"], function(col) !valid_values(col))
  if (any(invalid_values)) {
    stop("Values must be within 0, 1, 2, or NA.")
  }

  if (replace_na_with_zero) {
    my_data[, !names(my_data) %in% "age"][is.na(my_data[, !names(my_data) %in% "age"])] <- 0
  }

  calc_points <- function(condition, points_if_true, points_if_false = 0) {
    ifelse(condition, points_if_true, points_if_false)
  }
  mi <- calc_points(my_data$mi == 1, 1)
  chf <- calc_points(my_data$chf == 1, 1)
  pvd <- calc_points(my_data$pvd == 1, 1)
  cevd <- calc_points(my_data$cevd == 1, 1)
  dementia <- calc_points(my_data$dementia == 1, 1)
  cpd <- calc_points(my_data$cpd == 1, 1)
  ctd <- calc_points(my_data$ctd == 1, 1)
  pud <- calc_points(my_data$pud == 1, 1)
  liver_disease <- calc_points(my_data$liver_disease == 2, 3, calc_points(my_data$liver_disease == 1, 1))
  diabetes_mellitus <- calc_points(my_data$diabetes_mellitus == 2, 2, calc_points(my_data$diabetes_mellitus == 1, 1))
  hp <- calc_points(my_data$hp == 1, 2)
  ckd <- calc_points(my_data$ckd == 1, 2)
  solid_tumor <- calc_points(my_data$solid_tumor == 2, 6, calc_points(my_data$solid_tumor == 1, 2))
  leuk <- calc_points(my_data$leuk == 1, 2)
  lym <- calc_points(my_data$lym == 1, 2)
  aids <- calc_points(my_data$aids == 1, 6)
  age <- calc_points(my_data$age >= 50, pmin(4, 1 + (my_data$age - 50) %/% 10))
  my_data <- save_data
  my_data$cci_score <- mi + chf + pvd + cevd + dementia + cpd + ctd + pud + liver_disease +
    diabetes_mellitus + hp + ckd + solid_tumor + leuk + lym + aids + age
  my_data$estimated_10_year_survival <- ifelse(is.na(my_data$cci_score), "NA", paste0(round(0.983^(exp(my_data$cci_score * 0.9)) * 100, 2), "%"))

  return(my_data)
}
