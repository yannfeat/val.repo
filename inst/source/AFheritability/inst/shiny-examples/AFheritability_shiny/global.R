########### Functions for shiny app
alternatives <- data.frame(xaxis = c("Heritability", "Heritability", "Heritability", "Prevalence", "Prevalence", "Prevalence", "Target", "Target", "Target", "Intervention", "Intervention", "Intervention"),
                   compare = c("Prevalence", "Target", "Intervention", "Heritability", "Target", "Intervention", "Heritability", "Prevalence", "Intervention", "Heritability"  ,"Prevalence", "Target"),
                   row.names = NULL, stringsAsFactors = FALSE)

value_maker <- function(xaxis, compare){

  if(xaxis == "Heritability" && compare == "Prevalence") {
    H_value <- c(0, 1)
    P_value <- c(0.02,0.1)
    T_value <- 0.01
    I_value <- 1
  }
  if(xaxis == "Heritability" && compare == "Target") {
    H_value <- c(0, 1)
    P_value <- 0.5
    T_value <- c(0.01, 0.3)
    I_value <- 1
  }
  if(xaxis == "Heritability" && compare == "Intervention") {
    H_value <- c(0, 1)
    P_value <- 0.3
    T_value <- 0.05
    I_value <- c(1,5)
  }
  if(xaxis == "Prevalence" && compare == "Heritability") {
    H_value <- c(0.2, 0.6)
    P_value <- c(0, 1)
    T_value <- 0.05
    I_value <- 1
  }
  if(xaxis == "Prevalence" && compare == "Target") {
    H_value <- 0.3
    P_value <- c(0, 1)
    T_value <- c(0.05, 0.1)
    I_value <- 1
  }
  if(xaxis == "Prevalence" && compare == "Intervention") {
    H_value <- 0.3
    P_value <- c(0, 1)
    T_value <- 0.05
    I_value <- c(1,5)
  }
  if(xaxis == "Target" && compare == "Heritability") {
    H_value <- c(0.2, 0.6)
    P_value <- 0.3
    T_value <- c(0, 1)
    I_value <- 1
  }
  if(xaxis == "Target" && compare == "Prevalence") {
    H_value <- 0.5
    P_value <- c(0.00001,0.3)
    T_value <- c(0, 1)
    I_value <- 1
  }
  if(xaxis == "Target" && compare == "Intervention") {
    H_value <- 0.5
    P_value <- 0.3
    T_value <- c(0, 1)
    I_value <- c(1,3)
  }
  if(xaxis == "Intervention" && compare == "Heritability") {
    H_value <- c(0.2, 0.6)
    P_value <- 0.3
    T_value <- 0.05
    I_value <- c(0,10)
  }
  if(xaxis == "Intervention" && compare == "Prevalence") {
    H_value <- 0.5
    P_value <- c(0.00001,0.3)
    T_value <- 0.05
    I_value <- c(0,10)
  }
  if(xaxis == "Intervention" && compare == "Target") {
    H_value <- 0.3
    P_value <- 0.08
    T_value <- c(0.003, 0.05)
    I_value <- c(0,10)
  }

  value <- list(H_value = H_value, P_value = P_value, T_value = T_value, I_value = I_value)
  return(value)
}
