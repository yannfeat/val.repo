#' Peinador airport data
#'
#' The data includes several variables describing 833 aircraft operations — 418 take-offs and 415 landings —
#' involving 27 different aircraft types at Peinador Airport (Vigo, Spain) during January 2025.
#'
#' @usage Peinador
#'
#' @format A data frame with 27 rows and 6 columns (variables):
#' \describe{
#' \item{type}{Aircraft type}
#' \item{i}{Aircraft index (subscript)}
#' \item{TO}{Number of takeoffs}
#' \item{LDG}{Number of landings}
#' \item{MTOW}{Maximum takeoff weight (in tons)}
#' \item{MLW}{Maximum landing weight (in tons)}
#' }
#'
#' @source AENA. (2025, April 23). \emph{Estadísticas de tráfico aéreo: Consultas personalizadas}. \url{https://www.aena.es/es/estadisticas/consultas-personalizadas.html}.
#'
#' @examples
#' # Allocation by MTOW (in t)
#' multiclonesrules(Peinador$MTOW, Peinador$TO, c("SFC", "SEC", "CEC", "CP"),
#' labels = FALSE)
#'
#' # Allocation by MLW (in t)
#' positives <- Peinador$LDG > 0
#' MLW <- Peinador$MLW[positives]
#' LDG <- Peinador$LDG[positives]
#' multiclonesrules(MLW, LDG, c("SFC", "SEC", "CEC", "CP"), labels = FALSE)
"Peinador"
