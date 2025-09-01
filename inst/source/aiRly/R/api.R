
# Create empty env to store all usefull informations
# emptyenv added to protect against picking variables from parent env

pkg.env <- new.env(parent = emptyenv())

# Assign initial values to NULL
assign("apikey", NULL, envir = pkg.env)
assign("limit", NULL, envir = pkg.env)
assign("remaining", NULL, envir = pkg.env)

#' Return base url of Airly API v2
#'
.base_url <- function() {
  'https://airapi.airly.eu/v2'
}


#' Sends a request to the specified url and retrieves it's content.
#'
#'
#' @param request_url url to be used
#' @param apikey airly apikey
#' @param query Default value is NULL. Optional argument if you want to add query to request
#'
#' @return parsed content of the response object
#'
.send_request <- function(request_url, apikey, query = NULL) {
  response <- httr::GET(url = request_url,
                        httr::add_headers(Accept = "application/json",
                                          apikey = apikey),
                        query = query)

  airly_response <- create_airly_api_response(response)
  validate_airly_api_response(airly_response)

  get_content(airly_response)
}


#' @title Set Airly apikey
#'
#' @description On a free plan, API consumer is required to use our API only in non-commercial projects.
#' More details are available in under \url{https://airly.eu/docs/tos-en.pdf}.
#'
#' @param key string. Get your api key \url{https://developer.airly.eu/}
#'
#' @export
#'
#' @examples
#' \donttest{
#' set_apikey("abctest")
#' }
#'
set_apikey <- function(key) {
  assert(is.character(key), "apikey must be a string")
  assign("apikey", key, envir = pkg.env)
}


#' @title Get Airly apikey
#'
#' @description Get apikey that was set by user
#'
#' @return apikey value of set api key
.get_apikey <- function() {
  tryCatch(
    key <- get("apikey", envir = pkg.env),
    error = function(e) {
      key <- NULL
    }
  )
  key
}

#' @title Get Airly installation by id
#'
#' @description Endpoint returns single installation metadata, given by id
#'
#' @param id integer
#'
#' @export
#'
#' @return airly_location item
#'
#' @examples
#' \donttest{
#' get_installation_by_id(2137)
#' }
#'
get_installation_by_id <- function(id) {
  assert_ids(id)
  api_key <- .get_apikey()
  assert_apikey(api_key)

  request_url <- create_request_url(.base_url(), c("installations", id))
  item <- .send_request(request_url, api_key)
  create_airly_location(item)
}


#' @title Get Airly nearest installations to given point
#'
#' @description Endpoint returns list of installations which are closest to a given point, sorted by distance to that point.
#'
#' @param lat latitude as decimal degree
#' @param lng longitude as decimal degree
#' @param max_distance default value 3.0. All the returned installations must be located within this limit from the given point (in km). Negative value means no limit
#' @param max_results default value 1. Maximum number of installations to return. Negative value means no limit
#' @export
#'
#' @return data.frame of airly_location items
#'
#' @examples
#' \donttest{
#' get_nearest_installations(50.11670, 19.91429, max_distance = 20)
#' }
#'
get_nearest_installations <- function(lat, lng, max_distance=NULL, max_results=NULL) {
  assert_coordinates(lat, lng)
  api_key <- .get_apikey()
  assert_apikey(api_key)

  query <- list(
    lat = lat,
    lng = lng
  )
  if (!is.null(max_distance)) query[["maxDistanceKM"]] = max_distance
  if (!is.null(max_results)) query[["maxResults"]] = max_results

  request_url <- create_request_url(.base_url(),
                                    paths=c("installations", "nearest"),
                                    add_json_ext = FALSE)
  items <- .send_request(request_url, api_key, query)
  create_airly_location(items)
}

#' @title Get Airly nearest measurements to given point
#'
#' @description Endpoint returns measurements for an installation closest to a given location
#'
#' @param lat latitude as decimal degree
#' @param lng longitude as decimal degree
#' @param max_distance default value 3.0. All the returned installations must be located within this limit from the given point (in km). Negative value means no limit
#' @export
#'
#' @return data.frame of airly_measurements items
#'
#' @examples
#' \donttest{
#' get_nearest_measurements(50.11670, 19.91429, max_distance = 10)
#' }
#'
get_nearest_measurements <- function(lat, lng, max_distance=NULL) {
  assert_coordinates(lat, lng)
  api_key <- .get_apikey()
  assert_apikey(api_key)
  query <- list(
    lat = lat,
    lng = lng
  )
  if (!is.null(max_distance)) query[["maxDistanceKM"]] = max_distance

  request_url <- create_request_url(.base_url(),
                                    paths=c("measurements", "nearest"),
                                    add_json_ext = FALSE)
  items <- .send_request(request_url, api_key, query)
  create_airly_measurement(items)
}

#' @title Get Airly measurements for any geographical location
#'
#' @description Endpoint returns measurements for any geographical location
#'
#' @param lat latitude as decimal degree
#' @param lng longitude as decimal degree
#'
#' @export
#'
#' @return object of airly_measurements class
#'
#' @examples
#' \donttest{
#' get_point_measurements(50.11670, 19.91429)
#' }
#'
get_point_measurements <- function(lat, lng) {
  assert_coordinates(lat, lng)
  api_key <- .get_apikey()
  assert_apikey(api_key)
  query <- list(
    lat = lat,
    lng = lng
  )

  request_url <- create_request_url(.base_url(),
                                    paths=c("measurements", "point"),
                                    add_json_ext = FALSE)
  item <- .send_request(request_url, api_key, query)
  create_airly_measurement(item)
}


#' @title Get Airly measurements for any geographical location given installation id
#'
#' @description Endpoint returns measurements for concrete installation given by installation Id
#'
#' @param id integer, installation identifier
#'
#' @export
#'
#' @return object of airly_measurements class
#'
#' @examples
#' \donttest{
#'  get_installation_measurements(8077)
#' }
#'
get_installation_measurements <- function(id) {
  assert_ids(id)
  api_key <- .get_apikey()
  assert_apikey(api_key)
  query <- list(
    installationId = id
  )
  request_url <- create_request_url(.base_url(),
                                    paths=c("measurements", "installation"),
                                    add_json_ext = FALSE)
  item <- .send_request(request_url, api_key, query)
  create_airly_measurement(item)
}

#' @title Get Airly available indexes
#'
#' @description Endpoint returns a list of all the index types supported in the API along with lists of levels defined per each index type.
#'
#' @export
#'
#' @return object of airly_meta class
#'
#' @examples
#' \donttest{
#'  get_indexes()
#' }
#'
get_indexes <- function() {
  api_key <- .get_apikey()
  assert_apikey(api_key)
  request_url <- create_request_url(.base_url(),
                                    paths=c("meta", "indexes"),
                                    add_json_ext = FALSE)
  item <- .send_request(request_url, api_key)
  create_airly_meta(item)
}


#' @title Get measures used in Airly
#'
#' @description Endpoint returns list of all the measurement types supported in the API along with their names and units.
#'
#' @export
#'
#' @return data.frame with measure names and units
#'
#' @examples
#' \donttest{
#'  get_measurements_info()
#' }
#'
get_measurements_info <- function() {
  api_key <- .get_apikey()
  assert_apikey(api_key)

  request_url <- create_request_url(.base_url(),
                                    paths=c("meta", "measurements"),
                                    add_json_ext = FALSE)
  item <- .send_request(request_url, api_key)
  item
}
#' @title Get information about remaining API requests
#'
#' @description Default rate limit per apikey is 100 API requests per day for all users. In order to get information, user has to make at least one request.
#'
#' @export
#'
#' @return list containing information about remaining requests and daily limit
#'
#' @examples
#' \donttest{
#' # Make any request before calling this function
#' remaining_requests()
#' }
#'
remaining_requests <- function() {
  api_key <- .get_apikey()
  assert_apikey(api_key)
  limit <- get("limit", envir = pkg.env)
  remaining <-  get("remaining", envir = pkg.env)
  if(is.null(limit) & is.null(remaining)) {
    warning("You should make at least one request. Check me after making first call.")
  }
  list(
    limit = limit,
    remaining = remaining
  )
}
