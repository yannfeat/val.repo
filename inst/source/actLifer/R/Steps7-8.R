


#' Total Years Lived From Age x
#'
#' Adds a new column called TotalYears to the dataset that was input.
#' TotalYears is the number of years lived from age zero to age x.
#'
#' @param data The mortality dataset, includes an age grouping variable,
#' @param age The age grouping variable, must be categorical
#' @param pop Population of each age group, must be numeric
#' @param deaths The midyear number of deaths at each age group, must be numeric
#' @import dplyr
#'
#' @return Dataset that was input with the added columns:
#' ConditionalProbDeath, ConditionalProbLife, NumberToSurvive, PersonYears, and TotalYears.
#' @export
#'
#' @examples
#' # This function will add the ConditionalProbDeath, ConditionalProbLife,
#' # NumberToSurvive, PropToSurvive, PersonYearsm and TotalYears columns to the
#' # dataset
#' total_years_lived(mortality2, "age_group", "population", "deaths")
total_years_lived <- function(data, age, pop, deaths) {
  data <- data %>%
    person_years(., age, pop, deaths) %>%
    mutate(TotalYears = if_else(row_number() == 1, sum(PersonYears), sum(PersonYears)-lag(cumsum(PersonYears))))

  return(data)
}

#' Life Expectancy of Age x
#'
#' Adds a new column called LifeExpectancy to the dataset that was input.
#' LifeExpectancy is how many more years we expect a person of age x to live beyond
#' their current age.
#'
#' @param data The mortality dataset, includes an age grouping variable,
#' @param age The age grouping variable, must be categorical
#' @param pop Population of each age group, must be numeric
#' @param deaths The midyear number of deaths at each age group, must be numeric
#' @import dplyr
#'
#' @return Dataset that was input with the added columns:
#' ConditionalProbDeath, ConditionalProbLife, NumberToSurvive, PersonYears, TotalYears, and LifeExpectancy.
#' @export
#'
#' @examples
#' # This function will add the ConditionalProbDeath, ConditionalProbLife,
#' # NumberToSurvive, PropToSurvive, PersonYears, TotalYears, and LifeExpectancy
#' # columns to the dataset.
#' # This will be a full lifetable
#' life_expectancy(mortality2, "age_group", "population", "deaths")
life_expectancy <- function(data, age, pop, deaths) {
  data <- data %>%
    total_years_lived(., age, pop, deaths) %>%
    mutate(LifeExpectancy = TotalYears/NumberToSurvive)

  return(data)
}
