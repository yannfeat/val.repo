#'@title Format an AHP table created by the general ahp() function
#'@name  formata_tabela2
#'
#'@author Lyncoln Oliveira
#'
#'@description Format an AHP table created by the general ahp() function
#'
#'@param tabela AHP table created by the general ahp() function
#'@param cores Color pattern to format the table. If "PADRAO" returns the color pattern (green, blue, green or blue); if "GRAY" returns the default gray color; if "WHITE" returns the table without colors
#'
#'@return Retorna uma tabela formatada com cores defundo responsivas as quantidades de prioridade dos elementos
#'
#'@importFrom formattable formatter style csscolor gradient area
#'
#'@export
#'
formata_tabela2 = function(tabela, cores = "PADRAO"){
  #require(formattable)
  if(cores[1] == "PADRAO"){ #Cores escolhidas utilizando a regra de harmonia de cores triade
    limiteInferiorCriterios = "#DeF7E9"
    limiteSuperiorCriterios = "#71CA97"
    limiteInferiorAlternativas = "#B6D4FF"
    limiteSuperiorAlternativas = "#0060D3"
    limiteInferiorCR = "#ff7f7f"
    limiteSuperiorCR = "#B0FFD5"
    cor_letra = "black"
  }
  if(cores[1] == "GRAY"){
    limiteInferiorCriterios = "#9e9e9e"
    limiteSuperiorCriterios = "#4f4f4f"
    limiteInferiorAlternativas = "#9e9e9e"
    limiteSuperiorAlternativas = "#4f4f4f"
    limiteInferiorCR = "#4f4f4f"
    limiteSuperiorCR = "#9e9e9e"
    cor_letra = "white"
  }
  if(cores[1] == "WHITE"){
    limiteInferiorCriterios = "#ffffff"
    limiteSuperiorCriterios = "#ffffff"
    limiteInferiorAlternativas = "#ffffff"
    limiteSuperiorAlternativas = "#ffffff"
    limiteInferiorCR = "#ffffff"
    limiteSuperiorCR = "#ffffff"
    cor_letra = "black"

  }

  numero_linhas = dim(tabela)[1]
  numero_colunas = dim(tabela)[2]


  tabela_porcento = transforma_tabela(tabela)
  maior_alternativa = round(max(100*as.numeric(unlist(lapply(tabela[1,3:(numero_colunas-1)],function(x) gsub("%","",x))))),2)

  formato = function(cor1,cor2){formatter(.tag = "span",
                                          style =x ~ style("background-color" =csscolor(gradient(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))), cor1, cor2)),
                                                                  "border-radius" = "4px",
                                                                  "color" = cor_letra,
                                                                  display = "block"))
  }

  formata_maior_alternativa = formatter("span",
                                        style = x ~ style("font-weight" = ifelse(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))) == maior_alternativa, "bold", NA),
                                                          "font-size" = ifelse(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))) == maior_alternativa, "130%", NA)))

  formato_CR = formatter(.tag = "span",
                         style =x ~ style("background-color" =ifelse(as.numeric(unlist(lapply(x,function(x) gsub("%","",x)))) >= 10,limiteInferiorCR,limiteSuperiorCR),
                                                 "border-radius" = "4px",
                                                 "color" = cor_letra,
                                                 display = "block"))

  tabela_formatada = formattable(tabela_porcento,
                                               align = c("l",rep("c", numero_colunas - 1)),
                                               list(
                                                 "Criteria" = formatter("span", style = ~ style(color = "grey",font.weight = "bold")),
                                                 area(row = 2:(numero_linhas), col = 2) ~ formato(limiteInferiorCriterios,limiteSuperiorCriterios),
                                                 area(row = 2:(numero_linhas), col = 3:(numero_colunas-1)) ~ formato(limiteInferiorAlternativas,limiteSuperiorAlternativas),
                                                 area(col = numero_colunas) ~ formato_CR,
                                                 area(row = 1, col = (3:numero_colunas-1)) ~ formata_maior_alternativa

                                                 )

                                               )


  return(tabela_formatada)
}
