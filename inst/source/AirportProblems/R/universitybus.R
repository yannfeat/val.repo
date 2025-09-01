#' University bus travel data
#'
#' Data containing 4 variables with information about the round-trip journeys
#' of university students traveling by bus from the Morrazo region (Galicia, Spain)
#' to the University of Vigo.
#'
#' @usage universitybus
#'
#' @format A data frame with 13 rows and 4 columns (variables):
#' \describe{
#' \item{district}{Area where each student boards and alights from the bus}
#' \item{town}{Town to which each student belongs}
#' \item{distance}{Distance in kilometers of the road route (bus path) between a district and the University of Vigo}
#' \item{passengers}{Number of passengers per district}
#' }
#'
#' @source Own elaboration based on data retrieved from Google Maps (accessed on April 25, 2025).
#'
#' @importFrom stats setNames
#'
#' @examples
#' # We assume that the bus is entirely funded by the municipalities
#' # How are the cots distributed among them?
#'
#' # Allocation by district (in km)
#' by_district <- multiclonesrules(universitybus$distance, universitybus$passengers,
#' agents_names = universitybus$district, labels = FALSE)
#'
#' # Allocation by town (in km)
#' district_to_town <- setNames(universitybus$town, universitybus$district)
#' by_town <- t(rowsum(t(by_district), group = district_to_town[colnames(by_district)]))
#' print(by_town)
#'
#' # Cost allocation by town (in euros)
#' cT <- 500 # Total cost of the journey (bus rental cost + variable costs)
#' max_dist <- max(universitybus$distance) # maximum distance
#' by_town_cost <- round(by_town * (cT / max_dist), 2) # km to euros
#' print(by_town_cost)
#'
"universitybus"
