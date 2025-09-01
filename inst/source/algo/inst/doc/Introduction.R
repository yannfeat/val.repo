## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # install.packages("usethis")
#  
#  usethis::edit_r_environ()
#  

## ---- eval = FALSE------------------------------------------------------------
#  # These credentials are fakes and used  to give you an idea
#  
#  ALGOLIA_ID= LSKDFNSFSD
#  
#  ALGOLIA_KEY= 29453SKVNEV43T3G3KVEEV
#  
#  
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  # install.packages("remotes")
#  
#  remotes::install_github("feddelegrand7/algo")
#  
#  

## ---- eval = FALSE------------------------------------------------------------
#  
#  library(shiny)
#  library(algo)
#  
#  
#  ui <- fluidPage(
#  
#    use_algolia(),
#  
#  
#    textInput(inputId = "inp1", label = "Please introduce your address", width = "800px"),
#  
#  
#    algo(element = "#inp1", type = "address") # Don't forget to add the # to your ID
#  
#  )
#  
#  
#  server <- function(input, output) {
#  
#  
#  
#  
#  }
#  
#  shinyApp(ui, server)
#  

