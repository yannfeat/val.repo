#' Elevator data
#'
#' The data includes 4 variables related to the 14 apartments in a building,
#' within the context of distributing the costs associated with installing an elevator
#' in that building.
#'
#' @usage elevator
#'
#' @format A data frame with 14 rows and 4 columns (variables):
#' \describe{
#' \item{id}{Unit identifier}
#' \item{floor}{Floor number}
#' \item{c}{Cumulative elevator installation cost for each dwelling (in euros).}
#' \item{area}{Area of each dwelling (in square meters).}
#' }
#'
#' @source Own elaboration.
#'
#' @examples
#' # By number of floors (floor)
#' cw <- clonesgroups(elevator$c)$cw
#' eta <- clonesgroups(elevator$c)$eta
#' rules <- c("SFC", "SEC", "CEC", "CP", "CC")
#' by_floor <- list()
#' for (rule in rules){
#'   by_floor[[rule]] <- clonesrule(cw, eta, rule)
#' }
#' comparisonallocations(cw, by_floor, agents_names = unique(elevator$floor),
#' legend = rules, labels = FALSE)
#'
#' # Based on the area of each dwelling (area), by unit identifier (id)
#' c <- elevator$c
#' w <- elevator$area
#' by_id <- multiweightedrules(c, w, rules = c("SFC", "SEC", "CSEC", "CEC", "CP"),
#'          draw = TRUE, agents_names = elevator$id, labels = FALSE)
#' round(by_id, 2)
"elevator"
