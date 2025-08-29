#'@title Data Preparation
#' @description Data preparation for ABCoxPH
#' @param data Raw data sets
#' @param t_int No of days to be considered as single time interval (Default value: 90)
#' @param max_lac Maximum no of lactation to be considered for data preparation (Default value: Max Lactation)
#' @import stats readxl
#' @return
#' \itemize{
#'   \item wide_data - Processed data for ABCoxPH
#' }
#' @export
#'
#' @examples
#' library("ABSurvTDC")
#' library("readxl")
#' data_test<-read_excel(path = system.file("extdata/data_test.xlsx", package = "ABSurvTDC"))
#' PropData<-DataPrep(data =as.data.frame(data_test))
#' @references
#' \itemize{
#'\item J.D. Kalbfleisch and R.L. Prentice (1980). The statistical analysis of failure time data. John Wiley & Sons, Inc., New York, 1980.  <doi:10.1002/9781118032985>
#' \item J.P. Klein and  M L. Moeschberger (2003). Survival Analysis: Techniques for Censored and Truncated Data. Springer New York. <doi:10.1007/b97377>
#' }

DataPrep<-function(data, t_int, max_lac) {
  if (!is.data.frame(data)) {
    stop("Input must be a dataframe with structure as given in example")
  }

  if (missing(t_int)) { # Let user specify the time interval for making covariates
    t_int <- 90
    message("No time interval specified, defaulting to 90 days\n")
  } else {
    message("Time interval set to", t_int, "days\n")
  }

  animal <- unique(data[, 1]) # Vector of animal IDs
  n <- length(animal) # Number of animals observed
  n_lacs <- n_qt <- LAC_final <- name_LAC <- NULL # For output
  Herd <- YearFC <- Season <- HYS <- AFC <- Cen <- NULL # For output
  min_YFC <- min(as.numeric(format(data[,3], format = "%Y"))) # Minimum YearFC

  ## Creating fixed/time-independent variables ##
  for (j in 1:n) { # Check every animal
    animal_j <- subset(data, data[, 1] == animal[j]) # Isolate each animal

    lac_j <- nrow(animal_j) # Count no. of lactations of jth animal
    n_lacs <- append(n_lacs, lac_j) # Vector of no. of lactations corresponding to animal ID

    qt_j <- as.numeric((animal_j[lac_j, 4]) - (animal_j[1, 3])) %/% t_int + 1 # Calculating no. of quarters of jth animal
    n_qt <- append(n_qt,qt_j) # Vector of no. of quarters corresponding to animal ID

    cen_j <- unique(animal_j$Cen) # Check censored status of jth animal
    Cen <- append(Cen, cen_j) # Vector of censored status corresponding to animal ID

    herd_j <- unique(animal_j$Herd) # Check herd no. of jth animal
    Herd <- append(Herd, herd_j) # Vector of herd no. corresponding to animal ID

    yearFC_j <- as.numeric(format(animal_j$Date_Calved[1], format = "%Y")) - min_YFC + 1 # Check year at first calving of jth animal
    YearFC <- append(YearFC, yearFC_j) # Vector of year at first calving corresponding to animal ID

    # Categorisation into seasons by checking DOB #
    DOB_j <- unique(animal_j$DOB) # Read DOB of jth animal
    m_dob_j <- match(months(DOB_j), month.name) # Extract month number of DOB_j
    d_dob_j <- as.numeric(format(DOB_j, '%d')) # Extract day number of DOB_j
    if (((m_dob_j == 2) & (d_dob_j >= 16)) | ((m_dob_j > 2) & (m_dob_j < 6)) | ((m_dob_j == 6) & (d_dob_j < 16))) {
      season_j <- 2 # S=2 if 16/02 <= DOB_j < 16/06
    }
    else if (((m_dob_j == 6) & (d_dob_j >= 16)) | ((m_dob_j > 6) & (m_dob_j < 10)) | ((m_dob_j == 10) & (d_dob_j < 16))) {
      season_j <- 3 # S=3 if 16/06 <= DOB_j < 16/10
    }
    else {
      season_j <- 1 # S=1 if DOB_j >= 16/10 or DOB_j < 16/02
    }
    Season<-append(Season,season_j)

    hys_j <- herd_j * 1000 + (yearFC_j*10) + season_j # Calculate HYS of jth animal
    HYS <- append(HYS, hys_j) # Vector of HYS corresponding to animal ID

    afc_j <- animal_j$Date_Calved[1] - animal_j$DOB[1] # Calculate HYS of jth animal
    AFC <- append(AFC, afc_j) # Vector of AFC corresponding to animal ID

    rm(animal_j, lac_j, qt_j, cen_j, herd_j, yearFC_j, DOB_j, m_dob_j, d_dob_j, season_j, hys_j, afc_j) # Remove intermediary variables
  }

  if (missing(max_lac)) { # Let user choose the number of lactations to be included
    max_lac <- max(n_lacs) # Highest number of lactation among all animals
    message("Max no. of lactations to be considered not specified. Defaulting to the max no. present among all animals in data\n")
  } else {
    message("All animals will be considered for", max_lac, "lactations\n")
  }

  max_qt <- max(n_qt) # Highest number of quarters among all animals

  ## Creating time-dependent variables ##

  for (j in 1:n) { # Check every animal
    animal_j <- subset(data, data[, 1] == animal[j]) # Isolate each animal
    lac_j <- nrow(animal_j) # Count no. of lactations of jth animal
    # Calculating lactation and dry lengths of jth animal
    lac_length_j <- as.vector(as.ts(animal_j[, 4] - animal_j[, 3])) # Lactation lengths
    dry_length_j <- NULL # For dry lengths
    if (!lac_j == 1) {
      for (d in 1:(lac_j-1)) {
        dry <- as.vector(as.ts(animal_j[(d+1),3] - animal_j[d,4]))
        dry_length_j <- append(dry_length_j, dry) # Dry lengths
        rm(dry) # Remove intermediary variables
      }
    }
    # Calculating cumulative lactation and dry lengths #
    lac_cum_j <- (lac_length_j[1] %/% t_int) + 1 # 1st lactation length
    dry_cum_j <- NULL
    if (!lac_j == 1) { # Subsequent lactation lengths (if exist)
      for (l in 2:(lac_j+1)) {
        dry_l_cum <- (sum(lac_length_j[1:(l-1)]) + sum(dry_length_j[1:(l-1)])) %/% t_int + 1 #Cumulative next dry
        lac_l_cum <- (sum(lac_length_j[1:l]) + sum(dry_length_j[1:(l-1)])) %/% t_int + 1 # Cumulative next lactation
        lac_cum_j <- append(lac_cum_j, lac_l_cum)
        dry_cum_j <- append(dry_cum_j, dry_l_cum)
        rm(dry_l_cum, lac_l_cum) # Remove intermediary variables
        if (l == lac_j) {
          break
        }
      }
    }
    # Creating time dependent covariates #
    # Make covariates for first lactation
    lac_count <- 1
    lac_yes <- rep("Yes", lac_cum_j[1]) # "Yes" when animal was lactating
    lac_no <- rep("No", (lac_cum_j[lac_j] - lac_cum_j[1])) # "No" when animal was observed but dry
    lac_NA <- rep(NA, (max_qt - lac_cum_j[lac_j])) # NA when animal was not observed
    LAC <- c(lac_yes, lac_no, lac_NA)
    rm(lac_yes, lac_no, lac_NA) # Remove intermediary variables
    # Make covariates for subsequent lactations (if exist)
    if(!lac_j == 1) {
      for (a in 2:lac_j) {
        lac_pre_no <- rep("No", dry_cum_j[a-1]) # "No" when animal was in previous lactation
        lac_yes <- rep("Yes", (lac_cum_j[a] - dry_cum_j[a-1])) # "Yes" when animal was lactating
        lac_no <- rep("No", (lac_cum_j[lac_j] - lac_cum_j[a])) # "No" when animal was observed but dry
        lac_NA <- rep(NA, (max_qt - lac_cum_j[lac_j])) # NA when animal was not observed
        lac_a <- c(lac_pre_no, lac_yes, lac_no, lac_NA)
        LAC <- c(LAC, lac_a)
        lac_count <- a
        rm(lac_pre_no, lac_yes, lac_no, lac_NA, lac_a) # Remove intermediary variables
        if (a == max_lac) { # Stop if max_lac is reached
          break
        }
      }
    }
    # Fill lactations not reached with NA, if max_lac not reached
    if (!lac_count == max_lac) {
      for (b in (lac_j+1):max_lac) {
        lac_no <- rep("No", lac_cum_j[lac_j]) # "No" when animal was observed but dry
        lac_NA <- rep(NA, (max_qt - lac_cum_j[lac_j])) # NA when animal was not observed
        lac_b <- c(lac_no, lac_NA)
        LAC <- c(LAC, lac_b)
        lac_count <- b
        rm(lac_no, lac_NA, lac_b) # Remove intermediary variables
      }
    }
    LAC_final<-rbind(LAC_final, LAC)
    rm(animal_j, lac_j, lac_length_j, dry_length_j, lac_cum_j, dry_cum_j, LAC) # Remove intermediary variables
  }
  # Naming the lactations
  name_seq <- paste0("L", seq(1, max_lac))
  for (r in 1:max_lac) {# Naming the lactations with time
    name_L <- paste(name_seq[r], seq(1,max_qt,1), sep="_")
    name_LAC <- c(name_LAC, name_L)
    rm(name_L) # Remove intermediary variables
  }
  # Naming the rows and columns of LAC
  colnames(LAC_final) <- name_LAC
  rownames(LAC_final) <- animal
  # Creating output df
  fixed_var<-cbind(n_lacs, n_qt, Cen, HYS, AFC)
  wide_data<-as.data.frame(list(fixed_var, LAC_final))
  # Conversion of "yes/no" into factors
  for (i in c(6:ncol(wide_data))) {
    wide_data[, i] <- factor(wide_data[, i], levels = c("Yes", "No"))
  }
  # Remove unnecessary variables
  rm(n_lacs, n_qt, Cen, HYS, AFC, fixed_var, LAC_final, animal, b, d, a, Herd, i, j, l,
     lac_count, max_lac, max_qt, n, name_LAC, name_seq, r, Season, YearFC)

  # Output the created wide_data as a dataframe
  return(wide_data)
}


#'@title Cox-PH Model for Animal Breeding
#' @description Data preparation for ABCoxPH
#' @param wide_data Dataset from DataPrep function
#' @param lact Number of lactation to be used for model building
#' @import stats survival readxl
#' @return
#' \itemize{
#'   \item Cox_Model - ABCoxPH model
#'   \item LongData- Long data
#' }
#' @export
#'
#' @examples
#' library("ABSurvTDC")
#' library("readxl")
#' data_test<-read_excel(path = system.file("extdata/data_test.xlsx", package = "ABSurvTDC"))
#' PropData<-DataPrep(data =as.data.frame(data_test))
#' ABCoxPH(PropData)
#' @references
#' \itemize{
#'\item J.D. Kalbfleisch and R.L. Prentice (1980). The statistical analysis of failure time data. John Wiley & Sons, Inc., New York, 1980.  <doi:10.1002/9781118032985>
#' \item J.P. Klein and  M L. Moeschberger (2003). Survival Analysis: Techniques for Censored and Truncated Data. Springer New York. <doi:10.1007/b97377>
#' }

ABCoxPH <- function(wide_data, lact){
  unfold <- function(data, time, event, cov, cov.names = paste("covariate",                                                               ".", 1:ncovs, sep = ""), suffix = ".time",
                     cov.times = 0:ncov, common.times = TRUE, lag = 0, ...) {
    vlag <- function(x, lag) c(rep(NA, lag), x[1:(length(x) -
                                                    lag)])
    xlag <- function(x, lag) apply(as.matrix(x), 2, vlag, lag = lag)
    all.cov <- unlist(cov)
    if (!is.numeric(all.cov))
      all.cov <- which(is.element(names(data), all.cov))
    if (!is.list(cov))
      cov <- list(cov)
    ncovs <- length(cov)
    nrow <- nrow(data)
    ncol <- ncol(data)
    ncov <- length(cov[[1]])
    nobs <- nrow * ncov
    if (length(unique(c(sapply(cov, length), length(cov.times) -
                        1))) > 1)
      stop(paste("all elements of cov must be of the same length and \n",
                 "cov.times must have one more entry than each element of cov."))
    var.names <- names(data)
    subjects <- rownames(data)
    omit.cols <- if (!common.times)
      c(all.cov, cov.times)
    else all.cov
    keep.cols <- (1:ncol)[-omit.cols]
    factors <- names(data)[keep.cols][sapply(data[keep.cols],
                                             is.factor)]
    levels <- lapply(data[factors], levels)
    first.covs <- sapply(cov, function(x) x[1])
    factors.covs <- which(sapply(data[first.covs], is.factor))
    levels.covs <- lapply(data[names(factors.covs)], levels)
    nkeep <- length(keep.cols)
    if (is.numeric(event))
      event <- var.names[event]
    events <- sort(unique(data[[event]]))
    if (length(events) > 2 || (!is.numeric(events) && !is.logical(events)))
      stop("event indicator must have values {0, 1}, {1, 2} or {FALSE, TRUE}")
    if (!(all(events == 0:1) || all(events == c(FALSE, TRUE)))) {
      if (all(events = 1:2))
        data[[event]] <- data[[event]] - 1
      else stop("event indicator must have values {0, 1}, {1, 2} or {FALSE, TRUE}")
    }
    times <- if (common.times)
      matrix(cov.times, nrow, ncov + 1, byrow = TRUE)
    else as.matrix(data[, cov.times])
    new.data <- matrix(Inf, nobs, 3 + ncovs + nkeep)
    rownames <- rep("", nobs)
    colnames(new.data) <- c("start", "stop", paste(event, suffix,
                                                   sep = ""), var.names[-omit.cols], cov.names)
    end.row <- 0
    data <- as.matrix(as.data.frame(lapply(data, as.numeric)))
    for (i in 1:nrow) {
      start.row <- end.row + 1
      end.row <- end.row + ncov
      start <- times[i, 1:ncov]
      stop <- times[i, 2:(ncov + 1)]
      event.time <- ifelse(stop == data[i, time] & data[i,
                                                        event] == 1, 1, 0)
      keep <- matrix(data[i, -omit.cols], ncov, nkeep, byrow = TRUE)
      select <- apply(matrix(!is.na(data[i, all.cov]), ncol = ncovs),
                      1, all)
      rows <- start.row:end.row
      cov.mat <- xlag(matrix(data[i, all.cov], nrow = length(rows)),
                      lag)
      new.data[rows[select], ] <- cbind(start, stop, event.time,
                                        keep, cov.mat)[select, ]
      rownames[rows] <- paste(subjects[i], ".", seq(along = rows),
                              sep = "")
    }
    row.names(new.data) <- rownames
    new.data <- as.data.frame(new.data[new.data[, 1] != Inf &
                                         apply(as.matrix(!is.na(new.data[, cov.names])), 1, all), ])
    for (fac in factors) {
      new.data[[fac]] <- factor(levels[[fac]][new.data[[fac]]])
    }
    fcv <- 0
    for (cv in factors.covs) {
      fcv <- fcv + 1
      new.data[[cov.names[cv]]] <- factor(levels.covs[[fcv]][new.data[[cov.names[cv]]]])
    }
    new.data
  }
  max_qt <- max(wide_data$n_qt)
  max_lac <- (ncol(wide_data) - 5)/max_qt
  # Let user choose how many lactations to consider in the model
  if (missing(lact)) {
    lact <- max_lac
    message("CoxPH model considering all lactations available in wide data\n")
  } else if (lact <= max_lac) {
    message("CoxPH model considering", lact, "lactations\n")
  } else {
    stop("CoxPH model cannot have more number of lactations than is present in wide data")
  }
  # Define column numbers corresponding to covariate names
  covariates <- list()
  cnames <- NULL
  for (i in 1:lact) {
    covariates[[i]] <- ((max_qt*(i-1)):((max_qt*i)-1)) + 6
    cnames[i] <- paste0("Lact_", i)
  }
  last_cov <- covariates[[length(covariates)]][length(covariates[[length(covariates)]])]
  # Conversion from wide to long format
  long_data <- unfold(wide_data[, 1:last_cov], time = "n_qt", event = "Cen",
                      cov = covariates, cov.names = cnames)
  # Correcting to factors in long_data
  for (i in c(9:ncol(long_data))) {
    long_data[, i] <- factor(long_data[, i], levels = c("Yes", "No"))
  }
  long_data<<-long_data
  # Formula for model
  eq <- "AFC+HYS"
  for (e in 1:length(cnames)) {
    eq <- paste(eq, cnames[e], sep="+")
  }
  eq <- as.formula(noquote(paste("Surv(start, stop, Cen.time)", eq, sep="~")))
  # Fitting the Cox-PH Model
  cox_mdl <- coxph(eq, method = "efron", data = long_data)
  # Remove unnecessary variables
  rm(max_qt, max_lac, covariates, cnames, last_cov, long_data, eq)

  # Output<-list(Cox_Model<-cox_mdl, LongData=LongData)
  return(cox_mdl)
}


#'@title ABCoxPH Prediction
#' @description Prediction for ABCoxPH model
#' @param Model ABCoxPH model
#' @param NewData New data
#' @param AFC Age (in days) at first calving
#' @param HYS Combine effect of herd, year and season
#' @import stats readxl
#' @return
#' \itemize{
#'   \item SurvProb - Survival probabilities
#' }
#' @export
#'
#' @examples
#' library("ABSurvTDC")
#' library("readxl")
#' data_test<-read_excel(path = system.file("extdata/data_test.xlsx", package = "ABSurvTDC"))
#' PropData<-DataPrep(data =as.data.frame(data_test))
#' model<-ABCoxPH(PropData)
#' Lact_1<-c("Yes","Yes","Yes","No","No","No","No","No","No","No","No")
#' Lact_2<-c("No","No","No","No","Yes","Yes","No","No","No","No","No")
#' Lact_3<-c("No","No","No","No","No","No","No","No","Yes","Yes","Yes")
#' Lact_4<-c("No","No","No","No","No","No","No","No","No","No","No")
#' Lact_5<-c("No","No","No","No","No","No","No","No","No","No","No")
#' Lact_6<-c("No","No","No","No","No","No","No","No","No","No","No")
#' Lact_7<-c("No","No","No","No","No","No","No","No","No","No","No")
#' Lact_8<-c("No","No","No","No","No","No","No","No","No","No","No")
#' Lact_9<-c("No","No","No","No","No","No","No","No","No","No","No")
#' ndata<- data.frame(Lact_1,Lact_2,Lact_3,Lact_4,Lact_5,Lact_6,Lact_7,
#'                    Lact_8,Lact_9)
#' HYS<-2033
#' AFC <- 1400
#' CoxPred(Model=model, NewData=ndata, AFC, HYS)
#'
#' @references
#' \itemize{
#'\item J.D. Kalbfleisch and R.L. Prentice (1980). The statistical analysis of failure time data. John Wiley & Sons, Inc., New York, 1980.  <doi:10.1002/9781118032985>
#' \item J.P. Klein and  M L. Moeschberger (2003). Survival Analysis: Techniques for Censored and Truncated Data. Springer New York. <doi:10.1007/b97377>
#' }

CoxPred<-function(Model, NewData, AFC, HYS){
  subject<- NULL
  datap<-data.frame(AFC, HYS,NewData, start = 0:(nrow(NewData)-1), stop = 1:nrow(NewData), Cen.time = 0,
                    subject = 1)
  Pred<-survfit(Model, newdata = datap, id = subject)

  # Required outputs
  plot(Pred,conf.int = FALSE ,xlab="time",ylab = "proportion under lactation")
  SurvProb<-Pred$surv
  return(SurvProb)
}


