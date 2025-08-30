#'Bare combine
#'
#'Simplify mode of create vector in RStudio
#'
#'@usage bare_combine()
#'@examples
#'\dontrun{bare_combine()}
#'@export
bare_combine <- function(){
  dados_documento = rstudioapi::getActiveDocumentContext()
  texto = dados_documento$selection[[1]]$text
  while(endsWith(texto,"\n")){
    texto = stringr::str_sub(texto,end=-2)
  }
  while(endsWith(texto," ")){
    texto = stringr::str_sub(texto,end=-1)
  }
  if(stringr::str_detect(texto,stringr::fixed("\n"))){
    texto = stringr::str_split(texto,stringr::fixed("\n"))[[1]]
    if(sum(stringr::str_detect(texto,stringr::fixed(";"))) > 0){
      texto = stringr::str_split(texto,stringr::fixed(";"))
    }
    else if(sum(stringr::str_detect(texto,stringr::fixed(","))) > 0){
      texto = stringr::str_split(texto,stringr::fixed(","))
    }
  }
  else if(stringr::str_detect(texto,stringr::fixed(";"))){
    texto = stringr::str_split(texto,stringr::fixed(";"))
  }
  else if(stringr::str_detect(texto,stringr::fixed(","))){
    texto = stringr::str_split(texto,stringr::fixed(","))
  }
  else if(stringr::str_detect(texto,stringr::fixed(" "))){
    texto = stringr::str_split(texto,stringr::fixed(" "))
  }
  texto = stringr::str_trim(purrr::as_vector(texto))
  df = data.frame(texto)
  df = dplyr::filter(df,texto != "")

  string = 'c("'
  for (i in 1:length(df$texto)){
    if(i!=1){
      string=stringr::str_c(string,', "')
    }
    string = stringr::str_c(string,df[i,1])
    string = stringr::str_c(string,'"')
  }
  string = stringr::str_c(string,")")
  rstudioapi::insertText(text = string, id = NULL)
}
