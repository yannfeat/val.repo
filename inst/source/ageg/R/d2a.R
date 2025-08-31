
d2a <- function(bd){
  if((inherits(bd, "Date"))){
    tmpfunc <-  function(x){
      if(!is.na(x) | !is.na(x!="") | !is.na(as.character(x)!="NA")){
        as.numeric(
          gsub("[A-Za-z]|\\s+", "",
               floor(diff.Date(  c(  (x)  , as.Date(Sys.Date()) ) )
                     / 365)
          )
        )
      }else{return(NA)}
    }
    tmpres1 <-unlist(
      lapply( as.Date( bd, format = "%m/%d/%Y" ), tmpfunc  )
    )
    return(tmpres1)
  }else{message("Please ensure argument bd is a Date class object")}
}
