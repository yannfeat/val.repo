#' The Number of People to Survive to Age x
#'
#' Adds a new column called NumberToSurvive to the dataset that was input.
#' NumberToSurvive represents the number of people living at the beginning of the given age interval,
#' using an arbitrary 100,000 people for the first age group in the table.
#'
#' @param data The mortality dataset, includes an age grouping variable,
#' @param age The age grouping variable, must be categorical
#' @param pop Population of each age group, must be numeric
#' @param deaths The midyear number of deaths at each age group, must be numeric
#' @import dplyr
#' @return Dataset that was input with added columns:
#' ConditionalProbDeath, ConditionalProbLife, and NumberToSurvive.
#' @export
#'
#' @examples
#' # This function will add the ConditionalProbDeath, ConditionalProbLife, and
#' # NumberToSurvive columns to the dataset
#' number_to_survive(mortality2, "age_group", "population", "deaths")
number_to_survive <- function(data, age, pop, deaths){
  data <- data %>%
    conditional_life_prob(., age, pop, deaths) %>%
    mutate(NumberToSurvive = if_else(row_number()==1, 100000, 100000*lag(cumprod(ConditionalProbLife))))
  return(data)
}


#' Proportion to Survive to Age x
#'
#' Adds a new column called PropToSurvive to the dataset that was input.
#' PropToSurvive is the proportion surviving to age x
#'
#' @param data The mortality dataset, includes an age grouping variable,
#' @param age The age grouping variable, must be categorical
#' @param pop Population of each age group, must be numeric
#' @param deaths The midyear number of deaths at each age group, must be numeric
#' @import dplyr
#' @return Data frame that was input with columns for steps up to proportion surviving to age x included.
#' That is, the original data with the following added columns:
#' ConditionalProbDeath, ConditionalProbLife, NumberToSurvive, PropToSurvive
#' @export
#'
#' @examples
#' # This function will add the ConditionalProbDeath, ConditionalProbLife,
#' # NumberToSbrvivem and PropToSurvive columns to the dataset
#' prop_to_survive(mortality2, "age_group", "population", "deaths")
prop_to_survive <- function(data, age, pop, deaths){
  data<- data %>%
    number_to_survive(., age, pop, deaths) %>%
    mutate(PropToSurvive = NumberToSurvive/100000)
  return(data)
  }

#' Person Years Lived at Age x
#'
#' Adds a new column called PersonYears to the dataset that was input.
#' PersonYears represents the number of years lived at age x based on the number surviving to age x.
#'
#' @param data The mortality dataset, includes an age grouping variable,
#' @param age The age grouping variable, must be categorical
#' @param pop Population of each age group, must be numeric
#' @param deaths The midyear number of deaths at each age group, must be numeric
#' @import dplyr
#' @return Dataset that was input with the added columns:
#' ConditionalProbDeath, ConditionalProbLife, NumberToSurvive, PropToSurvive, PersonYears.
#' @export
#'
#' @examples
#' # This function will add the ConditionalProbDeath, ConditionalProbLife,
#' # NumberToSurvive, PropToSurvive, and PersonYears columns to the dataset
#' person_years(mortality2, "age_group", "population", "deaths")
person_years <- function(data, age, pop, deaths){
  data <-data %>%
    prop_to_survive(., age, pop, deaths) %>%
    mutate(PersonYears = (NumberToSurvive+lead(NumberToSurvive))/2) %>%
    mutate(PersonYears = if_else(is.na(PersonYears), NumberToSurvive, PersonYears))

  return(data)
}
