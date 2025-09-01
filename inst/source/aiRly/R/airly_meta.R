#' Creates a data.frame representing Airly meta
#' @param item list returned by Airly API
#' @return data.frame representing an airly_meta
#'
create_airly_meta <- function(item) {
  if (exists("name", where = item) & exists("levels", where = item)) {
    for( i in 1:length(item$name)) {
      item$levels[[i]]$name <- item$name[i]
    }
    item <- do.call(rbind, item$levels)
    item <- item[, -which(names(item) %in% c("values", "level"))]
    item
  } else {
    NULL
  }
}


#' Checks whether the given object is correctly correctly defined
#'
#' @param airly_meta object of the class airly_meta
validate_airly_meta <- function(airly_meta) {
  assert(all(c("name", "minValue","maxValue") %in% names(airly_meta)), "Object must have max/minValue and name field")
}
