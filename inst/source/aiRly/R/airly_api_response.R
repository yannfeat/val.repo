#' Creates an object representing a response from the
#' Airly API. Also every API call return information about current limits
#' What is used to assign variables in pkg.env
#'
#' @param response response object
#'
#' @return object representing a response from the
#' Airly API
#'
create_airly_api_response <- function(response) {
  parsed_content <- parse_json(response)
  if(exists("x-ratelimit-limit-day", response$headers)) {
    assign("limit", as.numeric(response$headers$`x-ratelimit-limit-day`), envir = pkg.env)
    assign("remaining", as.numeric(response$headers$`x-ratelimit-remaining-day`), envir = pkg.env)
  }
  structure(
    list(
      content = parsed_content,
      path = response$url,
      response = response
    ),
    class = "airly_api_response"
  )
}

#' Checks whether the given object is of the class
#' airly_api_response
#'
#' @param x object to test if it is of the class airly_api_response
#'
#' @return TRUE if the object is of the class airly_api_response
#'
is_airly_api_response <- function(x) {
  inherits(x, "airly_api_response")
}

#' Retrieves the response content
#'
#' @param x airly_api_response object to retrieve content from
#'
#' @return content of the given airly_api_response object
#'
get_content <- function(x) {
  assert(is_airly_api_response(x))
  x$content
}

#' Checks if the given response is not empty and that
#' it did not return an error http code.
#'
#' @param airly_api_response airly_api_response object to be checked
#'
validate_airly_api_response <- function(airly_api_response) {
  assert(!httr::http_error(airly_api_response$response),
         sprintf("The request resulted with an error [%s]\n%s\n<%s>",
                 airly_api_response$response$status_code,
                 airly_api_response$content,
                 airly_api_response$path))
  if (is.null(airly_api_response$content)) {
    warning(
      sprintf("The content of the response is empty!\n<%s>", airly_api_response$path)
    )
  }
}

#' @export
print.airly_api_response <- function(x, ...) {
  cat(sprintf("---AIRLY API RESPONSE [%s][%s]---", x$path, x$response$status_code))
  utils::str(x$content)
}
