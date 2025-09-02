#' @import data.table
#' @importFrom stats  ar na.omit sd
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @title MIM roll
#' @description This function computes the rolling window MIM  for a given data.table
#' @param data.table data.table with the data
#' @param identity.col column name of the identity intrument for example the stock ticker
#' @param Date.col column name of the date column with format format = "%Y-%m%-%d" (for example "2019-12-01")
#' @param rollWindow number of days to compute the MIM
#' @param return.col column name of the return column
#' @param min.obs minimum number of observations to compute the MIM
#' @param max.lag maximum number of lags to compute the MIM. The algorithm will select the number of lags that minimize the AIC
#' but the maximum number of lags is limited by this parameter. In case the AIC is zero for the zero lag then the algorithm will
#' estimate an AR(1) model. This is to avoid zero in the MIM.
#' @param a parameter to scale the MIM. In the paper Tran & Leivrik (2019) we use a=1.0
#' @return data.table with the MIM and the number of lags used to compute the MIM
#' @noRd
#' @examples
#' library(AMIM)
#' library(data.table)
#' data <- AMIM::exampledata # load the example data
#' MIM <- MIM.roll(
#'   data.table = data, identity.col = "ticker", rollWindow = 60,
#'   Date.col = "Date", return.col = "RET", min.obs = 30, max.lag = 10
#' )
MIM.roll <- function(data.table, identity.col, Date.col, rollWindow, return.col, min.obs, max.lag, a) {
  data <- data.table::copy(data.table)
  data.table::setDT(data)
  data.table::setnames(data,
    old = c(return.col),
    new = c("RET")
  ) ## change column name
  data.table::setorderv(data, c(identity.col, Date.col)) # oder columns

  pb <- txtProgressBar(
    min = 0, style = 3,
    max = nrow(data) - rollWindow * data.table::uniqueN(data[, c(identity.col), with = F])
  ) ### SET progress Bar

  dt.dates <- data[, list(date.join = seq(base::as.Date(get(Date.col), format = "%Y-%m%-%d"),
    by = "-1 day", len = rollWindow
  )),
  by = c(identity.col, Date.col)
  ] ## create an extra table for join

  MIM <- data.table::merge.data.table(dt.dates, data,
    by.x = c(identity.col, "date.join"),
    by.y = c(identity.col, Date.col), all.x = T
  )
  remove(dt.dates)
  MIM <- na.omit(MIM)
  MIM <- MIM[,
    {
      if (length(RET) < min.obs | sd(RET, na.rm = T) == 0) {
        MIM <- as.numeric(NA)
        N <- as.numeric(NA)
      } else {
        f <- ar(RET, aic = TRUE, order.max = max.lag)
        if (length(f$ar) == 0) {
          setTxtProgressBar(pb, .GRP) ### Update the progress
          # MIM <- 0.0
          # N <- 0.0
          # If the AIC point zero lag then estimate an AR(1), the coeficient is not significant anyway
          # This will avoid zero in MIM and AMIM
          f <- ar(RET, aic = F, order.max = 1)
        } else {
          g <- solve(t(chol(f$asy.var.coef))) %*% f$ar ### (P^-1)*beta  or L^-1'Beta. Chol give upper matrix U => t(u)=L
          g <- abs(g) * a ## absolute and scale
          MIM <- sum(g, na.rm = T) / (1 + sum(g, na.rm = T))
          N <- as.numeric(f$order)
        }
      }
      setTxtProgressBar(pb, .GRP) ### Update the progress bar
      list(MIM = MIM, N = N)
    },
    by = c(identity.col, Date.col)
  ]
  return(MIM)
}

#' @title AMIM roll
#' @import data.table
#' @description This function computes the rolling window AMIM  for a given data.table
#' @param data.table data.table with the data
#' @param identity.col column name of the identity intrument for example the stock ticker
#' @param Date.col column name of the date column with format "YYYY-mm-dd" (for example "2019-12-01")
#' @param rollWindow number of days to compute the AMIM
#' @param return.col column name of the return column
#' @param min.obs minimum number of observations to compute the AMIM
#' @param max.lag maximum number of lags to compute the MIM and then AMIM. The algorithm will select the number of lags that minimize the AIC
#' but the maximum number of lags is limited by this parameter. In case the AIC is zero for the zero lag then the algorithm will
#' estimate an AR(1) model. This is to avoid zero in the MIM and AMIM.
#' @return data.table with the MIM, AMIM and the number of lags used to compute the MIM, AMIM, confidence interval (CI), and the number of lags (N).
#' @export
#' @examples
#' library(AMIM)
#' library(data.table)
#' data <- AMIM::exampledata # load the example data
#' AMIM <- AMIM.roll(
#'   data.table = data, identity.col = "ticker", rollWindow = 60,
#'   Date.col = "Date", return.col = "RET", min.obs = 30, max.lag = 10
#' )
#'
#' AMIM[, .SD[(.N - 5):(.N), ], by = ticker] # Last 5 rows of each instrument
AMIM.roll <- function(data.table, identity.col, Date.col, rollWindow, return.col, min.obs, max.lag) {
  MIM. <- MIM.roll(
    data.table = data.table, identity.col = identity.col, Date.col = Date.col,
    rollWindow = rollWindow, return.col = return.col, min.obs = min.obs,
    max.lag = max.lag, a = 1
  ) # compute MIM and N. Force a=1 like in the Tran & Leivrik (2019) paper

  MIM. <- data.table::setDT(MIM.)

  CI <- data.table::copy(AMIM::CI) ## copy the CI data from AMIM package
  data.table::setDT(CI)

  MIM. <- data.table::merge.data.table(x = MIM., y = CI[a == 1, .(N, CI)], all.x = T, by.x = "N", by.y = "N", sort = F)

  MIM.$AMIM <- (MIM.$MIM - MIM.$CI) / (1 - MIM.$CI)

  return(MIM.)
}
