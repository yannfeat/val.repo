#' @title Estimation of Functional Forms of Covaraites under AFT Models
#'
#' @description The function aims to select informative covariates under the AFT model and estimate their corresponding
#'  functional forms with survival time. Specifically, the first step in this function is to derive an unbiased
#'  estimating function by the Buckley-James method with corrected survival times and censoring status. After that, a
#'  boosting algorithm with the cubic-spline method is implemented to an unbiased estimating function to detect
#'  informative covariates and estimate the functional forms of covariates iteratively.
#'
#' @param data A \code{c(n,p+2)} dimension of data, where \code{n} is sample size and
#'  \code{p} is the number of covariates. The first column is survival time and second
#'  column is censoring status, and the other columns are covariates.
#' @param iter The iteration times of the boosting procedure. The default value = 50 and the iteration will stop
#' when the absolute value of increment of every estimated value is small than 0.01.
#'
#' @importFrom ggplot2  "geom_line" "theme_minimal" "labs"  "aes"
#' @importFrom ggplot2  "ggplot"
#' @importFrom stats "lm" "smooth.spline" "fitted"
#'
#' @return covariates The first ten covariates that are selected in the iteration.
#' @return functional_forms The functional forms of the first ten covariates that are selected
#' in the iteration.
#' @return predicted_failure_time The predicted failure time of every sample
#' @return survival_curve Predicted survival curve of the sample.
#'
#' @examples
#' ## generate data with misclassification = 0.9 with n = 50, p = 6
#' ## and variance of noise term is 0.75. The y* is is related to the first
#' ## covariate.
#'
#' b <- matrix(0,ncol=6, nrow = 1)
#' b[1,1] <- 1
#' data <- data_gen(n=50, p=6, pi_01=0.9, pi_10 = 0.9, gamma0=1,
#' gamma1=b, e_var=0.75)
#'
#' ## Assume that covariates are independent and observed failure time is
#' ## related to first covariate with weight equals 1. And the scalar
#' ## in the classical additive measurement error model is 1 and
#' ## Misclassifcation probability = 0.9.
#'
#' matrixb <- diag(6)
#' gamma_0 <-  1
#' gamma_1 <- matrix(0,ncol=6, nrow =1)
#' gamma_1[1,1] <- 1
#' data1 <- ME_correction(pi_10=0.9,pi_01=0.9,gamma0 = gamma_0,
#' gamma1 = gamma_1,
#' cor_covar=matrixb, y=data[,1],
#' indicator=data[,2], covariate = data[,3:8])
#' data1 <- cbind(data1,data[,3:8])
#'
#' ## Data in boosting procedure with iteration times =2
#'
#' result <- Boosting(data=data1, iter=2)
#'
#'
#' @export


Boosting <- function(data, iter=50){
  W <- function(r_star,g_x){
    a <- data.frame(r_star); b <- data.frame(g_x)
    data <- cbind(a,b)
    colnames(data) <- c('Y',"X")
    w <- lm(Y~X+0,data = data)
    return(w$coefficients)
  }

  interval<- function(lower_bound,upper_bound){
    interval_size = (upper_bound-lower_bound)/7
    interval_points <- c(lower_bound)
    for(i in (1:7)){
      interval_points <- c(interval_points,lower_bound+interval_size*i)
    }
    return(interval_points)
  }

  corrected_data<- data
  colnames(corrected_data)[1:2] <- c('Y','censoring_indicator')


  # number of sample
  n = dim(corrected_data)[1]
  # dimension
  p = dim(corrected_data)[2]-2


  censoring_indicator <- corrected_data$censoring_indicator
  variable_catch <- c()
  y <- corrected_data$Y


  #step 0.
  sum_of_every_fitted_value <- rep(0,times=n)
  y_star <- corrected_data$Y
  r <- y - sum_of_every_fitted_value
  r_star <- y_star - sum_of_every_fitted_value
  df3 <- data.frame(corrected_data$censoring_indicator,r_star,r)
  colnames(df3)[1] <- "censoring_indicator"
  survival_probability <- c()

  for (i in c(1:dim(df3)[1])){
    yy <- df3[df3$r<=df3$r[i],]
    u <- c(yy$r)
    data_producted <- 1
    for (j in c(1:length(u))){
      total_sum_of_denominator <- sum((df3$r>=u[j])*1)
      censoring_indicator_1<- df3[df3$censoring_indicator==1,]
      total_sum_of_numerator <- sum((censoring_indicator_1$r==u[j])*1)
      data_producted = data_producted * (1-total_sum_of_numerator/total_sum_of_denominator)
    }
    survival_probability[i] <- data_producted
  }


  survival_probability[which(survival_probability==0)] <- 0.000001


  sum_riemann_for_every_n <- c()
  upper = max(r)
  for (i in c(1:length(df3$r))){
    point <- interval(df3$r[i],upper)
    riemann_x <- c()
    data_producted <- 1
    for (j in c(1:length(point))){
      xx <- df3[df3$r<=point[j],]
      u <- c(xx$r)
      for (k in c(1:length(u))){
        total_sum_of_denominator <- sum((df3$r>=u[k])*1)
        censoring_indicator_1<- df3[df3$censoring_indicator==1,]
        total_sum_of_numerator <- sum((censoring_indicator_1$r==u[k])*1)
        data_producted = data_producted *  (1-total_sum_of_numerator/total_sum_of_denominator)
      }
      riemann_x[j]<- data_producted
    }

    riemann_minus <- c()
    for (l in c(1:length(riemann_x)-1)){
      riemann_minus[l] <- riemann_x[l+1]-riemann_x[l]
    }


    sum_integrate <-c(0)
    for (m in c(1:length(riemann_minus))){
      sum_integrate <- sum_integrate + point[m+1] * riemann_minus[m]
    }
    sum_riemann_for_every_n[i] <- sum_integrate
  }


  y_star_update <- c()
  for (i in c(1:length(y))){
    y_star_update[i] <- censoring_indicator[i] * y[i]+
      (1-censoring_indicator[i])*(sum_of_every_fitted_value[i] - sum_riemann_for_every_n[i]/survival_probability[i])
  }


  y_star <- y_star_update


  add_g_every_time <- data.frame()
  df <- as.data.frame(matrix(numeric(0),ncol = p, nrow = n))
  df[is.na(df)] <- 0
  for (i in c(1:p)){
    colnames(df)[i] <- i
  }


  iterations = 0
  stop_times = iter


  while (TRUE) {
    r_star <- y_star - sum_of_every_fitted_value
    residual<- c(0)
    add_f <- c()
    times <- c(0)
    variable <- c()


    for (i in c(1:p)){
      times = times + 1
      f_variables <- smooth.spline(x=corrected_data[,i+2],y=r_star,cv=FALSE,all.knots=c(0,0.2,0.4,0.6,0.8,1))
      if (residual==0){
        add_f <- f_variables
        residual <- f_variables$pen.crit
        variable <- times
      } else if(f_variables$pen.crit < residual){
        add_f <- f_variables
        variable <- times
        residual <- f_variables$pen.crit
      }
    }


    variable_catch <- c(variable_catch,variable)
    fit = fitted(add_f)
    fit[which(is.na(fit))]=0

    w <- W(r_star,fit)
    add_g <- as.data.frame(w * fit, ncol = 1)

    stop_value <- 1e-2
    number_of_added_value <- sum((abs(add_g) < stop_value)*1)

    if (iterations == stop_times){
      break
    }

    if (number_of_added_value != n && iterations!= stop_times){
      df[as.character(variable)] <- df[,variable] + add_g

      sum_of_every_fitted_value <- sum_of_every_fitted_value + w * fit
      r_star = y_star - sum_of_every_fitted_value
      r <- y - sum_of_every_fitted_value
      df1 <- data.frame(corrected_data$censoring_indicator,r_star,r)
      colnames(df1)[1] <- "censoring_indicator"
      survival_probability <- c()

      for (i in c(1:dim(df1)[1])){
        yy <- df1[df1$r<=df1$r[i],]
        u <- c(yy$r)
        data_producted <- 1
        for (j in c(1:length(u))){
          total_sum_of_denominator <- sum((df1$r>=u[j])*1)
          censoring_indicator_1<- df1[df1$censoring_indicator==1,]
          total_sum_of_numerator <- sum((censoring_indicator_1$r==u[j])*1)
          data_producted = data_producted * (1-total_sum_of_numerator/total_sum_of_denominator)
        }
        survival_probability[i] <- data_producted
      }

      survival_probability[which(survival_probability==0)] <- 0.000001

      sum_riemann_for_every_n <- c()
      upper = max(r)
      for (i in c(1:length(df1$r))){
        point <- interval(df1$r[i],upper)
        riemann_x <- c()
        data_producted <- 1
        for (j in c(1:length(point))){
          xx <- df1[df1$r<=point[j],]
          u <- c(xx$r)
          for (k in c(1:length(u))){
            total_sum_of_denominator <- sum((df1$r>=u[k])*1)
            total_sum_of_denominator
            censoring_indicator_1<- df1[df1$censoring_indicator==1,]
            total_sum_of_numerator <- sum((censoring_indicator_1$r==u[k])*1)
            data_producted = data_producted *  (1-total_sum_of_numerator/total_sum_of_denominator)
          }
          riemann_x[j]<- data_producted
        }

        riemann_minus <- c()
        for (l in c(1:length(riemann_x)-1)){
          riemann_minus[l] <- riemann_x[l+1]-riemann_x[l]
        }


        sum_integrate <-c(0)
        for (m in c(1:length(riemann_minus))){
          sum_integrate <- sum_integrate + point[m+1] * riemann_minus[m]
        }
        sum_riemann_for_every_n[i] <- sum_integrate
      }


      y_star_update <- c()

      for (i in c(1:length(y))){
        y_star_update[i] <- censoring_indicator[i] * y[i]+
          (1-censoring_indicator[i])*(sum_of_every_fitted_value[i] - sum_riemann_for_every_n[i]/survival_probability[i])
      }

      y_star <- y_star_update

      iterations = iterations + 1

    }else{
      break
    }
  }


  survival_data<- df
  predict_failure_time <- apply(survival_data,1,sum)
  sur_time <-apply(survival_data,1,sum)
  sur_time <-exp(sur_time)
  sur_time <-sort(sur_time)
  sur_time <-data.frame(sur_time)
  sur_time
  pro <- seq(1,dim(sur_time)[1])/dim(sur_time)[1]
  pro <- sort(pro, decreasing= TRUE)
  pro <- data.frame(pro)


  # survival probability
  ddf1 <- cbind(sur_time, pro)
  survival_curve <- ggplot(ddf1, aes(x=sur_time,y=pro))+geom_line()+
    theme_minimal(14)+
    labs(x="time", y="survival probability", title='survival curve')
  variable_catch <- variable_catch[1:10]
  variable_catch <- as.numeric(names(table(variable_catch)))
  variable_names <- c()
  for (i in c(1:length(variable_catch))){
    variable_names[i] <- colnames(corrected_data)[variable_catch[i]+2]
  }

  pictures <- list()
  for (i in c(1:length(variable_catch))){
    temp <- as.data.frame(cbind(corrected_data[,variable_catch[i]+2],df[,variable_catch[i]]))
    colnames(temp) <- c('x','y')
    temp  <- temp [order(temp$x),]
    pic <- ggplot(temp, aes(x=x,y=y))+geom_line()+
      theme_minimal(14)+
      labs(x="x", y="f", title=colnames(corrected_data)[variable_catch[i]+2])
    pictures[[i]] <- pic
  }
  names(pictures) <- variable_names
  results <-list(predict_failure_time=predict_failure_time, covariates = variable_names, function_forms = pictures,survival_curve=survival_curve)

  x = c()
  return(results)
}
