#' Parses a json response
#'
#' @param response response object to parse
#'
#' @return parsed content of the given response
#'
parse_json <- function(response) {
  assert(httr::http_type(response) == "application/json",
         "The given response is not of type json")

  content <- httr::content(response, "text")
  jsonlite::fromJSON(content)
}

#' Replaces NULL with NA for nested lists. Useful when NULL value
#' leads to error while object casting
#' @param x nested list
#'
#' @return same list with NULL replaced with NA
#'
replace_null <- function(x) {
  lapply(x, function(x) {
    if (is.list(x)){
      replace_null(x)
    } else{
      if(is.null(x)) NA else(x)
    }
  })
}
