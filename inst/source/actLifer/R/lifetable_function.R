#' Lifetable Function
#'
#' Gives user more control over their lifetable compared to the life_expectancy() function.
#' Allows the user to add in the central death rate and proportion surviving to age x.
#' Allows the user to omit accessory columns which are used to calculate life expectancy.
#'
#'
#' @param data The mortality dataset, includes an age grouping variable,
#' @param age The age grouping variable, must be categorical
#' @param pop Population of each age group, must be numeric
#' @param deaths The midyear number of deaths at each age group, must be numeric
#' @param includeAllSteps If false, will only include the proportion surviving to age x and life expectancy for age x
#' @param includeCDR If true, will include the central death rate for each age group
#' @param includePS If true, will include the proportion surviving for each age group
#' @param ... Other optional grouping variables (can be race, gender, etc.)
#' @import dplyr
#' @return Lifetable
#' @export
#'
#' @examples
#' # Running lifetable() and choosing not to include CentralDeathRate and
#' # ProportionToSurvive (optional columns) in the output dataset
#'
#' lifetable(mortality2, "age_group", "population", "deaths", FALSE, TRUE, TRUE)
lifetable <- function(data, age, pop, deaths, includeAllSteps=TRUE, includeCDR=TRUE, includePS=TRUE, ...) {
  data <- data %>%
    group_by(...) %>%
    central_death_rate(., age, pop, deaths) %>%
    life_expectancy(., age, pop, deaths)

    if (!includeCDR) {
      data <- select(data, -CentralDeathRate)
    }
    if (!includePS) {
      data <- select(data, -PropToSurvive)
    }

  if (!includeAllSteps) {
    data <- select(data, -c(ConditionalProbDeath, ConditionalProbLife, NumberToSurvive, PersonYears, TotalYears))
  }

  return(data)
}
