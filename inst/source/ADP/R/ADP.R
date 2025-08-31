#' How many people use my product
#'
#' A data base of adoption probability by triers and users
#' user_p - a vector of percentage  (0<users<1) of users.
#' triers_p - a vector of percentage  (0<triers<1) of triers
#' ADP - vector of predicted percentage (0<ADP<1) of the adoption probability
#' of an innovative product in the population.
#' @author Mickey Kislev and Shira Kislev
#' @details
#' The measuring of triers is relatively easy. It is just a question of whether
#' a person tried a product even once in his life or not. While measuring the
#' rate of people who also adopt it as part of their life is more complicated
#' since the adoption of a product is a subjective view of the individual.
#' Mickey Kislev and Shira Kislev developed a formula to calculates the prevalence
#' of users of a product to overcome this difficulty. The current dataseet
#' assists in calculating the users of a product based on the prevalence of
#' triers in the population.
#'
#' For example, suppose that a candy company launched a new chocolate bar.
#' A candy company can collect data on the number of people who tried their
#' chocolate bar and know from the rate of triers how many people decided to
#' consume the new chocolate bar regularly. It should be noticed that the model
#' was proved only on consumer behaviour of adults above the age of 21 years old
#' and above.
#' @seealso \code{\link{ptriers}}, \code{\link{pusers}}, \code{\link{adp.t}}, and \code{\link{adp.u}}
#' @source Kislev, Mickey M. & Kislev, Shira, (2020). The Market Trajectory of a
#' Radically New Product: E-Cigarettes. IJMS 12(4):63-92, DOI:\href{https://www.ccsenet.org/journal/index.php/ijms/article/view/0/44285}{10.5539/ijms.v12n4p63}
ADP <- data.frame(triers_p = 1:100/100)
ADP$user_p <- ADP$triers_p * 0.25 * exp(1.35*ADP$triers_p)
ADP$ADP <- ADP$user_p / ADP$triers_p

#' Calculates the predicted prevalence of triers according to the users' rate
#'
#' This function develops a prediction of the triers' rate of an innovation in
#' the market, according to the number of users.
#'
#' @param users a vector of percentige (0<users<1) of known users.
#' @return a vector of predicted percentige(0<triers<1) of triers in the population of a certain innovation according to known users rate
#' @author Mickey Kislev and Shira Kislev
#' @details
#' This function calculates the rate of triers in the population of a certain
#' innovation according to known users rate in that population and measured in a survey.
#' @seealso \code{\link{pusers}}, \code{\link{adp.t}}, and \code{\link{adp.u}}
#' @source Kislev, Mickey M. & Kislev, Shira, (2020). The Market Trajectory of a
#' Radically New Product: E-Cigarettes. IJMS 12(4):63-92, DOI:\href{https://www.ccsenet.org/journal/index.php/ijms/article/view/0/44285}{10.5539/ijms.v12n4p63}
#' @examples
#' # 50% rate of users
#' ptriers(0.5)
#' 0.7382
#' # means that 74% of the population tried the product, in case that 50% of
#' # the population are using it.
#' @export
ptriers <- function(users) {
  if(users < 0 | users > 1){
    stop("users prevalence can be dafine in values between 0 to 1", call. = FALSE)
  }
  p <- length(users)
  pvalues <- numeric(p)
  exp_model <- data.frame(triers_p = 1:10000/10000)
  exp_model$user_p <- exp_model$triers_p * 0.25 * exp(1.35*exp_model$triers_p)
  for (i in p) {
    triers <- which(abs(exp_model$user_p-users)==min(abs(exp_model$user_p-users)))/10000
    print(triers)
  }
}

#' Calculates the predicted prevalence of users according to the triers' rate
#'
#' This function develops a prediction of the users' rate of an innovation in
#' the market, according to the number of triers
#'
#' @param triers a vector of percentage (0<triers<1) of known triers
#' @return a vector of predicted percentage (0<users<1) of users in the population of a certain innovation according to known triers rate
#' @author Mickey Kislev and Shira Kislev
#' @details
#' This function calculates the rate of users in the population of a certain
#' innovation according to known triers rate in that population and measured in a survey.
#'
#' The measuring of triers is relatively easy. It is just a question of whether
#' a person tried a product even once in his life or not. While measuring the
#' rate of people who also adopt it as part of their life is more complicated
#' since the adoption of a product is a subjective view of the individual.
#' Mickey Kislev and Shira Kislev developed a formula to calculates the prevalence
#' of users of a product to overcome this difficulty. The current function
#' assists in calculating the users of a product based on the prevalence of
#' triers in the population.
#' @seealso \code{\link{ptriers}}, \code{\link{adp.t}}, and \code{\link{adp.u}}
#' @references Kislev, Mickey M. & Kislev, Shira, (2020). The Market Trajectory of a
#' Radically New Product: E-Cigarettes. IJMS 12(4):63-92, DOI:\href{https://www.ccsenet.org/journal/index.php/ijms/article/view/0/44285}{10.5539/ijms.v12n4p63}
#' @examples
#' # 50% rate of triers
#' pusers(0.5)
#' 0.2455041
#' # means that 24.5% of the population uses the product regularly, in case
#' # that 50% of the population already tried it.
#' @export

pusers <- function(triers) {
  if(triers < 0 | triers > 1){
    stop("triers prevalence can be dafine in values between 0 to 1", call. = FALSE)
  }
  p <- length(triers)
  pvalues <- numeric(p)
  for (i in p) {
    users <- triers * 0.25 * exp(1.35*triers)
    print(users)
  }
}

#' Calculates the predicted adoption probability according to the triers' rate
#'
#' #' This function develops a prediction of the adoption rate of an innovation in
#' the market, according to the number of triers
#'
#' @param triers a vector of percentige (0<triers<1) of known triers
#' @return a vector of predicted percentige(0<ADP<1) of the adoption probability
#' of a innovative product in the population.
#' @author Mickey Kislev and Shira Kislev
#' @details
#' This function calculates the adoption probability in the population of a certain
#' innovation according to known triers rate measured in a survey.
#' @seealso \code{\link{pusers}}, \code{\link{ptriers}}, and \code{\link{adp.u}}
#' @source Kislev, Mickey M. & Kislev, Shira, (2020). The Market Trajectory of a
#' Radically New Product: E-Cigarettes. IJMS 12(4):63-92, DOI:\href{https://www.ccsenet.org/journal/index.php/ijms/article/view/0/44285}{10.5539/ijms.v12n4p63}
#' @examples
#' # 50% rate of triers
#' adp.t(0.5)
#' 0.4910082
#' # means that every second person who tries the product will adopt it, in case
#' # that 50% of the population already tried it.
#' @export


adp.t <- function(triers) {
  if(triers < 0 | triers > 1){
    stop("triers prevalence can be dafine in values between 0 to 1", call. = FALSE)
  }
  p <- length(triers)
  pvalues <- numeric(p)
  for (i in p) {
    y <- triers * 0.25 * exp(1.35*triers)
    ADP <- y / triers
    print(ADP)
  }
}

#' Calculates the predicted adoption probability according to the users' rate
#'
#' This function develops a prediction of the adoption rate of an innovation in
#' the market, according to the number of users.
#'
#' @param users a vector of percentige (0<users<1) of known users.
#' @return a vector of predicted percentige(0<ADP<1) of the adoption probability
#' of a innovative product in the population.
#' @author Mickey Kislev and Shira Kislev
#' @details
#' This function calculates the adoption probability in the population of a certain
#' innovation according to known users rate measured in a survey.
#' @seealso \code{\link{pusers}}, \code{\link{ptriers}}, and \code{\link{adp.t}}
#' @source Kislev, Mickey M. & Kislev, Shira, (2020). The Market Trajectory of a
#' Radically New Product: E-Cigarettes. IJMS 12(4):63-92, DOI:\href{https://www.ccsenet.org/journal/index.php/ijms/article/view/0/44285}{10.5539/ijms.v12n4p63}
#' @examples
#' # 50% rate of users
#' adp.u(0.5)
#' 0.6773232
#' # means that two out of three people who try the product will adopt it,
#' # in case that 50% of the population already uses it.
#' @export
adp.u <- function(users) {
  if(users < 0 | users > 1){
    stop("users prevalence can be dafine in values between 0 to 1", call. = FALSE)
  }
  p <- length(users)
  pvalues <- numeric(p)
  exp_model <- data.frame(triers_p = 1:10000/10000)
  exp_model$user_p <- exp_model$triers_p * 0.25 * exp(1.35*exp_model$triers_p)
  for (i in p) {
    y <- which(abs(exp_model$user_p-users)==min(abs(exp_model$user_p-users)))/10000
    ADP <- users / y
    print(ADP)
  }
}
