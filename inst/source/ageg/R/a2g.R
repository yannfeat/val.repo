
a2g <- function(ages,mydist){
  if(is.numeric(ages) & is.character(mydist)){
    distout <- data.frame( valin = ages, valout = rep(NA,length(ages))   )
    invisible(
      lapply( 1:nrow(distout),  function(x){
        lapply(1:length(mydist), function(y){
          if(grepl("\\-",mydist[y])){
            vallower <- as.numeric(strsplit(mydist[y],  "-")[[1]][1])
            valupper <- as.numeric(strsplit(mydist[y],  "-")[[1]][2])
            if((distout[x,1]>=vallower & distout[x,1]<=valupper) & !grepl("\\+",mydist[y])){
              distout[x,2] <<- mydist[y]
            }
          }
          if(  is.na(distout[x,2] ) & grepl("\\+",mydist[y])     ){
            if(     as.numeric(gsub("\\+","",mydist[y])) <=  distout[x,1]           ){
              distout[x,2] <<- mydist[y]
            }
          }
        })
      })
    )
    return(distout$valout)
  }else{message("Please ensure vector arguments ages (arg1) is of numeric class and mydist (arg2) is of character class")}
}
