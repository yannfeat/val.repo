#' Leukemia data set
#'
#' @name alloauto
#' @docType data
#' @author Abdisalam Hassan Muse, Samuel Mwalili, Oscar Ngesa,Christophe Chesneau, \email{abdisalam.hassan@amoud.edu.so}
#' @keywords datasets
#' @description The alloauto data frame has 101 rows and 3 columns.
#' @format This data frame contains the following columns:
#'   \itemize{
#'     \item time: Time to death or relapse, months
#'     \item type :Type of transplant (1=allogeneic, 2=autologous)
#'    \item delta:Leukemia-free survival indicator (0=alive without relapse, 1=dead
#'                                        or relapse)
#'  }
#'@source {
#' Klein and Moeschberger (1997) \emph{Survival Analysis Techniques for Censored
#'   and truncated data}, Springer.
#'  Kardaun Stat. Nederlandica 37 (1983), 103-126.
#'}
#'@examples {
#'data(alloauto)
#'str(alloauto)
#'}
NULL
