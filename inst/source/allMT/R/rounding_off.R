rounding_off <- function(col){
  # print(col)
  merged <- NULL
  # summary1 <- round(summary(col))

  summary1 <- summary(col)
  # print(summary(col))

  median <- summary1[3]
  iqr1 <- summary1[2]
  iqr3 <- summary1[5]

  # \\.[5,0]$ - The regular expression should be ended with a "5" and "0" (in that order), following a decimal point. ie: ".50"
  # If median value is "x.5", it will round up to the nearest integer, else will automatically round
  if(grepl("\\.[5,0]$",median)){
    median <- plyr::round_any(as.numeric(summary1[3]), 1, f=ceiling)
  } else{
    median <- round(median)
  }
  # If iqr1 value is "x.5", it will round up to the nearest integer.
  if(grepl("\\.[5,0]$",iqr1)){
    iqr1 <- plyr::round_any(as.numeric(summary1[2]), 1, f=ceiling)
  } else{
    iqr1 <- round(iqr1)
  }
  # If iqr3 value is "x.5", it will round up to the nearest integer.
  if(grepl("\\.[5,0]$",iqr3)){
    iqr3 <- plyr::round_any(as.numeric(summary1[5]), 1, f=ceiling)
  } else{
    iqr3 <- round(iqr3)
  }

  merged <- paste0(median, " [", iqr1, "-", iqr3, "]")

  return(merged)
}
