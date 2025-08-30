LRTest <- function(model1, model2){
  pvalue <- 1 - pchisq(abs(model1$resDeviance - model2$resDeviance), abs(model1$df - model2$df))
  cat("Likelihood ratio test\n")
  cat("Model 1: \n")
  print(model1$call[2][[1]])
  cat("Model 2: \n")
  print(model2$call[2][[1]])
  df <- c(model1$df, model2$df)
  loglik <- c(model1$resDeviance, model2$resDeviance)
  dif.df <- c(NA, abs(model2$df - model1$df))
  Chisq <- c(NA, round(abs(model2$resDeviance - model1$resDeviance), 3))
  p.value <-  c(NA, round(pvalue, 10))
  res <- as.data.frame(cbind(df, loglik, dif.df, Chisq, p.value))
  res[is.na(res)]   <- " "
  colnames(res) <- c("Res.df", "Res.Dev", "df", "Deviance", "Pr(>Chi)")
  return(res)
}
