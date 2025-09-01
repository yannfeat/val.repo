#'@title Calculates the AHP
#'@name ahp
#'@author Lyncoln Oliveira and Luciane Ferreira Alcoforado
#'@description Calculates AHP in a list of paired arrays or in a properly formatted excel worksheet stipend.

#'@param base List of paired arrays or excel path containing the properly formatted paired arrays.
#'@param mapeamento Vector containing the number of subscriptions of each criteria, from left to right. mapeamento = rep(0,n) n = number of criteria and no subcriteria;
#'mapeamento = c(1,2) for one subcriteria in criteria 1 anda two subcriteria in criteria 2. If in doubt, see the tutorial vignette.
#'@param nomes_alternativas Vector containing the names of the alternatives in your hierarchy,
#'if not filled returns a vector of LETTERS.
#'
#'@return Table containing the relationships of criteria, subscriptions (If any) and Alternatives Using the AHP system.
#'@import tibble
#'@import dplyr

#'
#'@examples
#'x=paste0(letters[3],1:5)
#'y=c(5,2,7,3,2)
#'m1=matrix_ahp(x,y)
#'x=paste0(letters[1],1:3)
#'y=c(4.4,5.2,3)
#'m2=matrix_ahp(x,y)
#'y=c(2,4,3)
#'m3=matrix_ahp(x,y)
#'y=c(4.9,5,3.3)
#'m4=matrix_ahp(x,y)
#'y=c(4.4,4.2,4.3)
#'m5=matrix_ahp(x,y)
#'y=c(5.4,5.2,5.7)
#'m6=matrix_ahp(x,y)
#'base=list(m1, m2, m3, m4, m5, m6)
#'mapeamento = rep(0,5)
#'nomes_alternativas = paste0(letters[1],1:3)
#'ahp(base,mapeamento, nomes_alternativas)
#'
#'#with subcriteria and 3 criteria and 2 alternatives
#'mapeamento = c(2,0,0) #2 subcriteria in criteria 1 and 0 subcriteria to others
#'x=paste0(letters[3],1:3) #3 criteria
#'y=c(5,2,7)
#'m1=matrix_ahp(x,y) #compare criteria
#'x=paste0(letters[4],1:2)
#'y=c(4,6)
#'m2=matrix_ahp(x,y) # 2 compare 2 subcriteria of criteria 1
#'x=paste0(letters[1],1:2)
#'y=c(2,4)
#'m3=matrix_ahp(x,y) #alternatives for subcriteria 1
#'y=c(4.9,5)
#'m4=matrix_ahp(x,y) #alternatives for subcriteria 2
#'y=c(4.4,4.2)
#'m5=matrix_ahp(x,y) #alternatives for criteria 2
#'y=c(5.4,5.2)
#'m6=matrix_ahp(x,y) ##alternatives for criteria 3

#'base=list(m1, m2, m3, m4, m5, m6)
#'
#'nomes_alternativas = paste0(letters[1],1:2)
#'ahp(base,mapeamento, nomes_alternativas)
#'
#'#Other mapeamento: criteria 2 with 2 subcriteria
#'
#'mapeamento = c(0,2,0)
#'nomes_alternativas = paste0(letters[1],1:2)
#'ahp(base,mapeamento, nomes_alternativas)
#'

#'@export
ahp = function(base,mapeamento,nomes_alternativas){

  preferencias = calcula_prioridades(base); preferencias
  objetivo = preferencias[1]; objetivo
  criterios = preferencias[2:(length(mapeamento)+1)]; criterios
  names(criterios) = paste0("C",1:length(mapeamento))
  alternativas = preferencias[(length(mapeamento) + 2):length(preferencias)]; alternativas
  matriz_criterios = base[2:(length(mapeamento)+1)]
  matriz_alternativas = base[(length(mapeamento) + 2):length(base)]

  #normalizando

  criterios_normalizados = c()
  for(i in 1:length(mapeamento)){
    criterios_normalizados[[i]] = criterios[[i]] * objetivo[[1]][i]
  }
  names(criterios_normalizados) = names(criterios); criterios_normalizados

  #Gerando nomes para a tabela

  nomes = c(paste0("---","Alternatives"))
  CR_saaty = c(CR(base[[1]]))
  aux = 1 # Me ajuda a me guiar pelas matrizes de subcriterios
  for(i in 1:length(mapeamento)){
    nomes = append(nomes, paste0("--",names(criterios[i])))
    CR_saaty = c(CR_saaty, CR(matriz_criterios[[i]]))
    for(j in 1:mapeamento[i]){
      if(mapeamento[i] == 0) break
      nomes = append(nomes, paste0("-",names(alternativas[aux])))
      CR_saaty = c(CR_saaty, CR(matriz_alternativas[[aux]]))
      aux = aux + 1
    }
  }
  nomes

  #Gerando coluna de pesos de critérios e subcritérios

  pesos = c(sum(objetivo[[1]]))
  for(i in 1:length(mapeamento)){
    pesos = append(pesos, objetivo[[1]][i])
    aux = 1
    for(j in 1:mapeamento[i]){
      if(mapeamento[i] == 0)break
      pesos = append(pesos,criterios_normalizados[[i]][aux])
      aux = aux+1
    }
  }
  pesos

  #testando primeira parte
  tabela = tibble::tibble(Criteria = nomes, Weights = pesos)
  #tabela


  #Aqui estou organizando a proporção de cada criterio por alternativas
  #Lyn: qtd_alternativas = length(alternativas[length(alternativas)][[1]]); qtd_alternativas
  #Lu:
  qtd_alternativas = length(nomes_alternativas)
  #Lyn:pesos_alternativas = list()
  #Criando a lista que serão preenchidas
  #for(i in 1:qtd_alternativas){
  #  pesos_alternativas[[i]] = 0
  #}
  #Lu: substituir código acima for por:
  pesos_alternativas = vector("list",qtd_alternativas)
  #names(pesos_alternativas) = LETTERS[1:qtd_alternativas]
  names(pesos_alternativas) = nomes_alternativas
  #pesos_alternativas


  aux = 1 #Navega entre a posição das matrizes de alternativas
  aux2 = 1 #Navega entra a posição do preenchimento das alternativas na nova lista ordenada por linha
  for(i in 1:length(mapeamento)){
    #Se não existir subcriterios:
    if(mapeamento[[i]] == 0 ){
      for(j in 1:length(criterios_normalizados[[i]])){
        pesos_alternativas[[j]][aux2] = criterios_normalizados[[i]][j]
      }
      aux2 = aux2 + 1
    }
    #Se existir subcritérios:
    else{
      for(j in 1:length(criterios_normalizados[[i]])){
        #print("----")
        #print(criterios_normalizados[[i]][j])
        #print(names(alternativas[aux]))
        for(k in 1:length(alternativas[aux])){
          #print(alternativas[[aux]])
          for(p in 1:qtd_alternativas) {
            #print(alternativas[[aux]][p])
            pesos_alternativas[[p]][aux2] = alternativas[[aux]][p]*criterios_normalizados[[i]][j]
          }
          aux2 = aux2 + 1
        }
        aux = aux +1
      }
    }

  }
  pesos_alternativas


  ##


  #Agora irei aplicar a soma de proporções dos critérios para sub critérios

  pesos_alternativas_organizados = pesos_alternativas

  for( i in 1:qtd_alternativas){
    inferior = 2
    superior = 0
    pesos_alternativas_organizados[[i]] = c(sum(pesos_alternativas_organizados[[i]]), pesos_alternativas_organizados[[i]])
    #print(names(pesos_alternativas[i]))
    vetor = c(pesos_alternativas_organizados[[i]][1])
    #print(pesos_alternativas_organizados[[i]])
    for(j in 1:length(mapeamento)){
      if(mapeamento[j] == 0 ) {
        vetor = c(vetor, pesos_alternativas_organizados[[i]][inferior])
        inferior = inferior + 1
      }
      else{
        superior = inferior + mapeamento[j] - 1
        #print("----")
        #print(pesos_alternativas_organizados[[i]][inferior:superior])
        valor = sum(pesos_alternativas_organizados[[i]][inferior:superior])
        vetor = c(vetor, valor, pesos_alternativas_organizados[[i]][inferior:superior])
        #print(valor)
        #print("----")
        inferior = superior + 1
      }

    }
    pesos_alternativas_organizados[[i]] = vetor
  }

  pesos_alternativas_organizados

  #CR_saaty = unlist(lapply(base, function(x) return(CR(x))),use.names = F); CR_saaty
  tabela = append(tabela,pesos_alternativas_organizados)
  tabela = append(tabela, list("CR"= CR_saaty))
  return(dplyr::as_tibble(tabela))
}
