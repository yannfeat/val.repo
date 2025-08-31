library(shiny)

shinyUI(fluidPage(
  # Application title
  headerPanel("The attributable fraction and the heritability"),
  
  sidebarLayout(
    sidebarPanel(
  selectInput("xaxis", "Choose x-axis", choices = unique(alternatives$xaxis)),
  selectInput("compare", "Show at several different values of:", choices= "", selected=""),
  
  uiOutput("Heritability_slider"),
  uiOutput("Prevalence_slider"),
  uiOutput("Target_slider"),
  uiOutput("Intervention_slider")
    ),
  
    
    # Show a plot of the AF and heritability
    mainPanel(h5("This app shows how the attributable fraction (AF) can be expressed as a function of the heritability, disease prevalence, target group size and intervention effect. For more information read 'On the relationship between the attributable fraction and the heritability' (Dahlqwist et. al)."),
              h1("Plot"),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      plotOutput("AFfunction"),

      h5("Note!"),
      h6("The lines represent the 0%, 25%, 50%, 75% and 100% percentiles of the range of the variable chosen from the tab 'Show at several different values of'."),
      
      helpText(   a("Code is available here",     href="https://github.com/ElisabethDahlqwist/AFheritability"))
    )
)
)
)
