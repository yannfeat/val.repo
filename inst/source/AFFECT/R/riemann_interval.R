interval<- function(lower_bound, upper_bound){
  interval_size = (upper_bound-lower_bound)/6
  interval_points <- c(lower_bound)
  for(i in (1:6)){
    interval_points <- c(interval_points,lower_bound+interval_size*i)
  }
  return(interval_points)
}
