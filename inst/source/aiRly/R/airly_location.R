#' Creates an object representing Airly location
#'
#' @param item list returned by Airly API
#'
#' @return tibble representing an airly_location
#'
create_airly_location <- function(item) {
  item <- replace_null(item)
  airly_location <- tibble::tibble(id = item$id,
                                   elevation = item$elevation,
                                   is_airly = item$airly,
                                   location = tibble::as_tibble(item$location),
                                   address = tibble::as_tibble(item$address),
                                   sponsor = tibble::as_tibble(item$sponsor))
  validate_airly_location(airly_location)
  airly_location
}

#' Checks whether the given object is of the class
#' airly_location
#'
#' @param x object to test if it is of the class airly_location
#'
#' @return TRUE if the object is of the class airly_location
#'
is_airly_location <- function(x) {
  inherits(x, "airly_location")
}

#' Checks whether the given object is correctly defined
#' airly_location class
#'
#' @param airly_location tibble airly_location
#'
validate_airly_location <- function(airly_location) {
  assert(exists("id", where = airly_location), "Object's id is required")
}
