#' Descriptive: Descriptive analysis (Two factors)
#'
#' @description It performs the descriptive analysis of an experiment with two factors of interest.
#' @author Gabriel Danilo Shimizu, \email{gabrield.shimizu@gmail.com}
#' @author Leandro Simoes Azeredo Goncalves
#' @author Rodrigo Yudi Palhaci Marubayashi
#' @param f1 Numeric or complex vector with factor 1 levels
#' @param f2 Numeric or complex vector with factor 2 levels
#' @param response Numerical vector containing the response of the experiment.
#' @param theme ggplot2 theme (\emph{default} is theme_classic())
#' @param ylab Variable response name (Accepts the \emph{expression}() function)
#' @param xlab x name (this argument uses the \emph{parse} function)
#' @keywords Descriptive
#' @keywords Experimental
#' @return The function returns exploratory measures of position and dispersion, such as mean, median, maximum, minimum, coefficient of variation, etc ...
#' @export
#' @examples
#' library(AgroR)
#' data(cloro)
#' output=with(cloro, desc2fat(f1,f2,resp))
#' output$plot_single
#' output$plot_interaction

######################################################################################
## Analise descritiva
######################################################################################

desc2fat=function(f1,
                  f2,
                  response,
                  ylab="Response",
                  xlab=c("F1","F2"),
                  theme=theme_classic()){
  ylab=parse(text=gsub(" ","~",ylab))
  xlab=c(gsub(" ","~",xlab[1]),gsub(" ","~",xlab[2]))
  requireNamespace("crayon")
  requireNamespace("ggplot2")
  f1=as.factor(f1)
  f2=as.factor(f2)
  #===========================
  # Geral
  #===========================

  Media = mean(response, na.rm=TRUE)
  Mediana = median(response, na.rm=TRUE)
  Minimo = min(response, na.rm=TRUE)
  Maximo = max(response, na.rm=TRUE)
  Variancia = var(response, na.rm=TRUE)
  Desvio = sd(response, na.rm=TRUE)
  CV = Desvio / Media * 100
  juntos=cbind(Media,
               Mediana,
               Minimo,
               Maximo,
               Variancia,
               Desvio,
               CV)
  Media = tapply(response, list(f1, f2), mean, na.rm=TRUE)
  Mediana = tapply(response, list(f1, f2), median, na.rm=TRUE)
  Minimo = tapply(response, list(f1, f2), min, na.rm=TRUE)
  Maximo = tapply(response, list(f1, f2), max, na.rm=TRUE)
  Variancia = tapply(response, list(f1, f2), var, na.rm=TRUE)
  Desvio = tapply(response, list(f1, f2), sd, na.rm=TRUE)
  CV = Desvio / Media * 100
  juntos1 = list(
    "Mean" = Media,
    "Median" = Mediana,
    "Min" = Minimo,
    "Max" = Maximo,
    "Variance" = Variancia,
    "SD"=Desvio,
    "CV(%)"=CV)

  #===========================
  # Fator 1
  #===========================
  dados=data.frame(f1,response)
  grafico=ggplot(dados,aes(x=f1,y=response))+
    geom_boxplot(aes(fill=f1, group=f1),show.legend = F)+
    ylab(parse(text=ylab))+theme+labs(fill=parse(text=xlab[1]),x=parse(text=xlab[1]))
  grafico=grafico+
    theme(text = element_text(size=12,color="black"),
          axis.title = element_text(size=12,color="black"),
          axis.text = element_text(size=12,color="black"))
  # grafico=as.list(grafico)
  Media = tapply(response, f1, mean, na.rm=TRUE)
  Mediana = tapply(response, f1, median, na.rm=TRUE)
  Minimo = tapply(response, f1, min, na.rm=TRUE)
  Maximo = tapply(response, f1, max, na.rm=TRUE)
  Variancia = tapply(response, f1, var, na.rm=TRUE)
  Desvio = tapply(response, f1, sd, na.rm=TRUE)
  CV = Desvio / Media * 100
  juntos2 = cbind(Media,
                  Mediana,
                  Minimo,
                  Maximo,
                  Variancia,
                  Desvio,
                  CV)
  colnames(juntos2)=c("Mean","Median","Min","Max","Variance","SD","CV(%)")
  #===========================
  # Fator 2
  #===========================
  dados=data.frame(f2,response)
  grafico1=ggplot(dados,aes(x=f2,y=response))+
    geom_boxplot(aes(fill=f2, group=f2),show.legend = F)+
    ylab(parse(text=ylab))+theme+labs(fill=parse(text=xlab[2]),x=parse(text=xlab[2]))
  grafico1=grafico1+
    theme(text = element_text(size=12,color="black"),
          axis.title = element_text(size=12,color="black"),
          axis.text = element_text(size=12,color="black"))
  # grafico1=as.list(grafico1)
  Media = tapply(response, f2, mean, na.rm=TRUE)
  Mediana = tapply(response, f2, median, na.rm=TRUE)
  Minimo = tapply(response, f2, min, na.rm=TRUE)
  Maximo = tapply(response, f2, max, na.rm=TRUE)
  Variancia = tapply(response, f2, var, na.rm=TRUE)
  Desvio = tapply(response, f2, sd, na.rm=TRUE)
  CV = Desvio / Media * 100
  juntos3=cbind(Media,
                Mediana,
                Minimo,
                Maximo,
                Variancia,
                Desvio,
                CV)
  colnames(juntos3)=c("Mean","Median","Min","Max","Variance","SD","CV(%)")
  #===========================
  # Fator 1 x Fator 2
  #===========================

  dados=data.frame(f1,f2,response)
  grafico2=ggplot(dados,aes(x=f1,y=response, fill=f2))+
    geom_boxplot()+
    ylab(parse(text=ylab))+theme+labs(fill=parse(text=xlab[2]))
  grafico2=grafico2+
    theme(text = element_text(size=12,color="black"),
          axis.title = element_text(size=12,color="black"),
          axis.text = element_text(size=12,color="black"))
  # grafico2=as.list(grafico2)

  #===========================
  # Interacao
  #===========================
  inter1=ggplot(dados,aes(x=f1,y=response, color=f2))+
    stat_summary(fun.data = mean_se)+stat_summary(aes(color=f2, group=f2),
                                geom="line", fun.data = mean_se)+
    ylab(parse(text=ylab))+theme+labs(color=parse(text=xlab[2]),x=parse(text=xlab[1]))
  inter1=inter1+
    theme(text = element_text(size=12,color="black"),
          axis.title = element_text(size=12,color="black"),
          axis.text = element_text(size=12,color="black"))
  inter2=ggplot(dados,aes(x=f2,y=response, color=f1))+
    stat_summary(fun.data = mean_se)+stat_summary(aes(color=f1, group=f1),
                                geom="line", fun.data = mean_se)+
    ylab(parse(text=ylab))+theme+labs(color=parse(text=xlab[1]),x=parse(text=xlab[2]))
  inter2=inter2+
    theme(text = element_text(size=12,color="black"),
          axis.title = element_text(size=12,color="black"),
          axis.text = element_text(size=12,color="black"))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(green(italic("general description")))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  print(juntos)
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(green(italic("Interaction")))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  print(juntos1)
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(green(italic("f1")))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  print(juntos2)
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  cat(green(italic("f2")))
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  print(juntos3)
  cat(green(bold("\n-----------------------------------------------------------------\n")))
  output=list(plot_single=cowplot::plot_grid(cowplot::plot_grid(grafico,grafico1),
                                             grafico2,ncol=1),
              plot_interaction=cowplot::plot_grid(inter1,inter2,ncol=2))
}

