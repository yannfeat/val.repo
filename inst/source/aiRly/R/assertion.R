#' Asserts a given expression and throws an error
#' if it returns FALSE
#'
#' @param expression R expression to be evaluated
#'
#' @param error message to be displayed when the
#' expression is not fulfilled
#'
assert <- function(expression, error) {
  if (!expression) {
    stop(error, call. = FALSE)
  }
}


#' Checks whether ids are correctly defined. If not throws an error
#'
#' @param ids maximum number of ids to retrieve
#
assert_ids <- function(ids) {
  assert(all(is.numeric(ids)), "every id must be a numeric type")
  assert(all(ids > 0),
         "every id must be greater than 0")
}

#' Checks whether apikey is correctly set
#'
#' @param key airly apikey
#
assert_apikey <- function(key) {
  assert(!is.null(key), "You have to set apikey first! See set_apikey function.")
  assert(is.character(key), "apikey must be a string")
}

#' Checks whether apikey is correctly set
#'
#' @param lat latitude as decimal degree
#' @param lng longitude as decimal degree
#
assert_coordinates <- function(lat, lng) {
  assert(is.numeric(lat) & is.numeric(lng), "Both, latitude and longitude have to be numeric")
  assert(-90 <= lat & lat <= 90, "The valid range of latitude in degrees is -90 and +90")
  assert(-180 <= lng & lng <= 180, "The valid range of longitude  in degrees is -180 and +180")
}
