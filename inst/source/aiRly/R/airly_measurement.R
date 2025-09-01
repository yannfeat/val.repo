#' @importFrom tibble as_tibble

#' @title Print for "airly_measurement" type objects
#'
#' @param x "airly_measurement" type list
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
#'

print.airly_measurement <- function(x, ...) {
  utils::str(x)
}

#' Creates an object representing Airly measurement
#'
#' @param item list returned by Airly API
#'
#' @return object representing a airly_measurement
#'
create_airly_measurement <- function(item) {
  airly_measurement <- structure(
    list(
      current = build_current_df(item),
      history = build_history_df(item),
      forecast = build_forecast_df(item),
      raw_data = item
    ),
    class = "airly_measurement"
  )
  validate_airly_measurement(airly_measurement)
  airly_measurement
}

#' Creates object containing information about history data for given API response
#'
#' @param item list returned by Airly API
#'
#' @return tibble representing a airly_measurement with time, measures and indexes fields
#'
build_history_df <- function(item) {
  if (exists("history", where = item)) {
    history <- item$history
    measure_df <- do.call(rbind, lapply(history$values, function(x) as.data.frame(t(x[2]))))
    row.names(measure_df) <- 1:nrow(measure_df)
    names(measure_df) <- history$values[[1]]$name

    time_df <- data.frame(from = strptime(history$fromDateTime, "%Y-%m-%dT%H:%M:%OSZ"),
                          to = strptime(history$tillDateTime, "%Y-%m-%dT%H:%M:%OSZ"))

    indexes_df <- do.call(rbind, lapply(history$indexes,function(x) as.data.frame(t(x[2]))))
    row.names(indexes_df) <- 1:nrow(indexes_df)
    names(indexes_df) <- history$indexes[[1]]$name

    airly_history <- tibble::tibble(time = as_tibble(time_df),
                                    measure = as_tibble(measure_df),
                                    index = as_tibble(indexes_df))
    airly_history

  } else {
    warning("History is not available for this point")
    NULL
  }

}

#' Creates object containing information about history data for given API response
#'
#' @param item list returned by Airly API
#'
#' @return tibble representing a airly_measurement with time, measures and indexes fields
#'
build_forecast_df <- function(item) {
  if (exists("forecast", where = item)) {
    forecast <- item$forecast
    measure_df <- do.call(rbind, lapply(forecast$values, function(x) as.data.frame(t(x[2]))))
    row.names(measure_df) <- 1:nrow(measure_df)
    names(measure_df) <- forecast$values[[1]]$name

    time_df <- data.frame(from = strptime(forecast$fromDateTime, "%Y-%m-%dT%H:%M:%OSZ"),
                          to = strptime(forecast$tillDateTime, "%Y-%m-%dT%H:%M:%OSZ"))

    indexes_df <- do.call(rbind, lapply(forecast$indexes,function(x) as.data.frame(t(x[2]))))
    row.names(indexes_df) <- 1:nrow(indexes_df)
    names(indexes_df) <- forecast$indexes[[1]]$name

    airly_forecast <- tibble::tibble(time = as_tibble(time_df),
                                    measure = as_tibble(measure_df),
                                    index = as_tibble(indexes_df))
    airly_forecast
  } else {
    warning("Forecast is not available for this point")
    NULL
  }

}

#' Creates an object representing Airly measurement
#'
#' @param item list returned by Airly API
#'
#' @return object representing a airly_measurement
#'
build_current_df <- function(item) {
  if (exists("current", where = item)) {
    current <- item$current
    measure_df <- as.data.frame(t(current$values[[2]]))
    row.names(measure_df) <- 1:nrow(measure_df)
    names(measure_df) <- current$values$name

    time_df <- data.frame(from = strptime(current$fromDateTime, "%Y-%m-%dT%H:%M:%OSZ"),
                          to = strptime(current$tillDateTime, "%Y-%m-%dT%H:%M:%OSZ"))

    indexes_df <-  as.data.frame(current$indexes)
    indexes_df <- reshape2::acast(indexes_df, value~name)
    row.names(indexes_df) <- 1:nrow(indexes_df)

    airly_current <- tibble::tibble(time = as_tibble(time_df),
                                    measure = as_tibble(measure_df),
                                    index = as_tibble(indexes_df))
    airly_current

  } else {
    warning("Current data is not available for this point")
    NULL
  }

}

#' Checks whether the given object is of the class
#' airly_measurement
#'
#' @param x object to test if it is of the class airly_measurement
#'
#' @return TRUE if the object is of the class airly_measurement
#'
is_airly_measurement <- function(x) {
  inherits(x, "airly_measurement")
}

#' Checks whether the given object is correctly defined
#' airly_measurement class
#'
#' @param airly_measurement object of the class airly_measurement
#'
validate_airly_measurement <- function(airly_measurement) {
  assert(exists("history", where = airly_measurement), "Object does not contain 'history' field")
  assert(exists("raw_data", where = airly_measurement), "Object does not contain 'raw_data' field")
  assert(exists("forecast", where = airly_measurement), "Object does not contain 'forecast' field")
  assert(exists("current", where = airly_measurement), "Object does not contain current field")
  assert(is_airly_measurement(airly_measurement), "Object must be of the class airly_measurement")
}

