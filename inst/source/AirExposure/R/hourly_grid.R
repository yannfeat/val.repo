hourly_grid <-
function(hour_input, time_format,dir){
  input_hour <- as.POSIXct(strptime(hour_input, format = time_format))
  hour_exposure<- lubridate::hour(input_hour)
  exposure_day <- as.Date(input_hour)
  #Change to the directory where the grids are located.
  oldwd<-getwd()
  on.exit(setwd(oldwd))
  setwd(dir)
  # Only the shape files
  file_list <- dir(dir,pattern = ".shp")
  # The name of the shp file should be in the format %Y-%m-%d_%H%M
  table_files <-as.POSIXct(strptime( substr(file_list,1,15), format = "%Y-%m-%d_%H%M"))
  # Search file according to the day and hour
  searched_date <- which((as.Date(table_files)) == exposure_day)
  table_files <- table_files[searched_date] 
  searched_hour <- which((lubridate::hour(table_files))== hour_exposure)
  file <- table_files[searched_hour] 
  name_file<- paste(substr(file,1,10),"_",substr(file,12,13),substr(file,15,16),".shp",sep = "")
  return(name_file)
}
