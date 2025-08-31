library(shiny)

shinyServer(function(input, output, session) {
  
  observeEvent(
    input$xaxis,
  updateSelectInput(session, "compare", "Show at several different values of:",
                    choices = alternatives$compare[alternatives$xaxis == input$xaxis]))

  output$Heritability_slider <- renderUI({
    
    values <- value_maker(input$xaxis, input$compare)
    
    if(length(values$H_value)==1) sliderInput("Heritability", "Heritability", min=0, max = 1, value = values$H_value, step = 0.0001)
    else sliderInput("Heritability", "Heritability", min=0, max = 1, value = c(values$H_value[1], values$H_value[2]), step = 0.0001)
  })
  output$Prevalence_slider <- renderUI({
    
    values <- value_maker(input$xaxis, input$compare)
    
    if(length(values$P_value)==1) sliderInput("Prevalence", "Prevalence", min=0, max = 1, value = values$P_value, step = 0.000001)
    else sliderInput("Prevalence", "Prevalence", min=0, max = 1, value = c(values$P_value[1], values$P_value[2]), step = 0.000001)
  })
  output$Target_slider <- renderUI({
    
    values <- value_maker(input$xaxis, input$compare)
    
    if(length(values$T_value)==1) sliderInput("Target", "Proportion at highest genetic risk that are targeted by the intervention", min=0, max = 1, value = values$T_value, step = 0.0001)
    else sliderInput("Target", "Proportion at highest genetic risk that are targeted by the intervention", min=0, max = 1, value = c(values$T_value[1], values$T_value[2]), step = 0.0001)
  })
  output$Intervention_slider <- renderUI({
    
    values <- value_maker(input$xaxis, input$compare)
    
    if(length(values$I_value)==1) sliderInput("Intervention", "Intervention effect (i.e. standardized reduction in mean genetic risk)", min=0, max = 10, value = values$I_value, step = 0.1)
    else sliderInput("Intervention", "Intervention effect (i.e. standardized reduction in mean genetic risk)", min=0, max = 10, value = c(values$I_value[1], values$I_value[2]), step = 0.1)
  })
  
  output$AFfunction <- renderPlot({
    Prev <- input$Prevalence
    Her <- input$Heritability
    Tar <- input$Target
    Inter <- input$Intervention
    
    if(length(Her) > 1){
      Her <- seq(min(Her), max(Her), by= (max(Her)-min(Her))/10)
      if(input$compare == "Heritability") Her <- quantile(Her, probs=c(0,0.25, 0.5, 0.75, 1), names=F)
    } 
    if(length(Prev) > 1){
      Prev <- seq(min(Prev), max(Prev), by= (max(Prev)-min(Prev))/10)
      if(input$compare == "Prevalence") Prev <- quantile(Prev, probs=c(0,0.25, 0.5, 0.75, 1), names=F)
    } 
    if(length(Tar) > 1){
      Tar <- seq(min(Tar), max(Tar), by= (max(Tar)-min(Tar))/10)
      if(input$compare == "Target") Tar <- quantile(Tar, probs=c(0,0.25, 0.5, 0.75, 1), names=F)
    } 
    if(length(Inter) > 1){
      Inter <- seq(min(Inter), max(Inter), by= (max(Inter)-min(Inter))/10)
      if(input$compare == "Intervention") Inter <- quantile(Inter, probs=c(0,0.25, 0.5, 0.75, 1), names=F)
    } 
    
    AFfunction(Prevalence = Prev, Heritability = Her, Target = Tar, Intervention = Inter, xaxis = input$xaxis, compare = input$compare, yaxis = c(0, 0.16))
    })
  
})