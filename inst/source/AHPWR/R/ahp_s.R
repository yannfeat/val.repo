#'@title Calculates the AHP for criteria and subcriteria
#'@name ahp_s
#'@author Luciane Ferreira Alcoforado
#'@description Calculates AHP in a list of paired arrays or in a properly formatted excel worksheet stipend.

#'@param base List of paired arrays or excel path containing the properly formatted paired arrays.
#'@param map Vector containing the number of subscriptions of each criteria, from left to right. map = rep(0,n) n = number of criteria and no subcriteria;
#'mapeamento = c(1,2) for one subcriteria in criteria 1 and two subcriteria in criteria 2. If in doubt, see the tutorial vignette.
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
#'map = rep(0,5)
#'ahp_s(base,map)
#'
#'#with two subcriteria in criteria 1 and 2 alternatives
#'map = c(2,0,0)
#'x=paste0(letters[3],1:3) #3 criteria
#'y=c(5,2,7)
#'m1=matrix_ahp(x,y) # matrix compare three criteria
#'x=paste0("SC1",1:2)
#'y=c(4,6)
#'m2=matrix_ahp(x,y) # 2 matrix compare two subcriteria of criteria 1
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
#'
#'ahp_s(base,map)
#'
#'#Other mapeamento: criteria 2 with 2 subcriteria and 3 alternatives
#'
#'
#'map = c(2,2)
#'x=paste0(letters[3],1:2) #2 criteria
#'y=c(5,7)
#'m1=matrix_ahp(x,y) # matrix compare two criteria
#'x=paste0("SC1",1:2)
#'y=c(4,6)
#'m2=matrix_ahp(x,y) # matrix compare two subcriteria of criteria 1
#'x=paste0(letters[1],1:3)
#'y=c(2,4,5)
#'m3=matrix_ahp(x,y) #alternatives for subcriteria 1 - criteria 1
#'y=c(4.9,5, 2)
#'m4=matrix_ahp(x,y) #alternatives for subcriteria 2 - criteria 1
#'y=c(4.4,8, 6)
#'x=paste0("SC2",1:2)
#'m5=matrix_ahp(x,y) #matrix compare two subcriteria of criteria 2
#'y=c(5.4,5.2, 1)
#'x=paste0(letters[1],1:3)
#'m6=matrix_ahp(x,y) #alternatives for subcriteria 1 - criteria 2
#'y=c(9,5.2, 3)
#'m7=matrix_ahp(x,y) #alternatives for subcriteria 2 - criteria 2



#'base=list(m1, m2, m3, m4, m5, m6, m7)
#'
#'ahp_s(base,map)
#'
#'@export
ahp_s = function(base,map){

  n = ncol(base[[1]]) # #criteria
  m = dim(base[[length(base)]])[1] # #alternatives supondo ultima matriz contenha comparação de alternativas à luz de criterios/subcriterios
  k = sum(map) # #subcriteria map[i] is a number of subcriteria in i-criteria.
  x = sum(map)+length(map[map==0]) # is a number of matrix comparing alternatives
  y = length(map[map!=0]) # is a number of matrix comparing subcriteria
  if (length(base)!= (x+y+1)) stop("The number of comparison matrices does not confer!")
  if (length(map) != n) stop("the map length does not match the number of criteria")
  #if (sum(map)==0){ ver código do Lyncoln ou ignore essa lógica de separar}
  #tinha pensado em separar qdo não há subcritérios de quando há.
  preferencias = calcula_prioridades(base); preferencias #vetores prioridades de acordo com map
  objetivo = preferencias[1]; objetivo #peso dos critérios P(C)


  peso_criterio=matrix(nrow = n, ncol = m)
  peso_subcriterio = matrix(nrow=sum(map),ncol=m)
  #tem que ver uma sequencia para preferencia[] pegando somente criterios

  #nomeia criterios
  rownames(peso_criterio) = paste0("C",1:n)
  #nomeia alternativas
  colnames(peso_criterio) = paste0("A",1:m)

  #nomeia subcriterios para cada criterio, se houver
  if(sum(map>0)){
  seq=NULL
  mapc= map[map>0]
  for(i in seq_along(mapc)){
    seq = append(seq,1:mapc[i])} #índice de seq de subcriteria, inicia em 1 até n1 depois 1 até n2 e assim por diante
   rownames(peso_subcriterio) = paste0(rep(paste0("SC",which(map>0)),map[which(map>0)]),seq)
   colnames(peso_subcriterio) = paste0("A",1:m)}


aux = 2
  #cria matrizes de pesos dos critérios e subcritérios separadamente
  auxlinha=1 #controle linha da matriz peso_subcriterio
  for(k in seq_along(map)){
    if (map[k]==0) {peso_criterio[k,] = preferencias[[aux]]
    aux = aux+1
    auxlinha = k+1

    }
    if (map[k]>0) {

      peso_parcial_critk=matrix(nrow = map[k], ncol = m) #x = n. de subcriterios
      for(l in (1:map[k])){

      peso_parcial_critk[l,] = preferencias[[aux]][l]* preferencias[[(aux+l)]]

      }
      peso_subcriterio[auxlinha:(auxlinha+l-1),] = peso_parcial_critk
      auxlinha = auxlinha+l
      aux = aux+l+1
      peso_criterio[k,] = colSums(peso_parcial_critk)


    }

  }
  if(sum(map)==0){
  peso_alternativas = t(matrix(unlist(preferencias[2:(n+1)]),ncol=n)%*%preferencias[[1]])
  }
  if(sum(map)>0){
    peso_alternativas = preferencias[[1]]%*%peso_criterio
  }


#Faz a mesma coisa do código anterior só que cria uma tabela com pesos de criterios e subcriterios alternando entre si
#calcula tb os CR e inclui o peso total de cada criterio coluna 1 e o peso global das alternativas, linha1
CR_saaty = lapply(base,CR)
  aux = 2
  peso_criterio=matrix(nrow = n, ncol = m)
  peso_subcriterio = matrix(nrow=sum(map),ncol=m)
  tabela = matrix(nrow = n+sum(map)+1, ncol=m+2)
  tabela[1,] = c(1,peso_alternativas,CR(base[[1]]))
  l_tabela=2
  #cria matrizes de pesos dos critérios e subcritérios separadamente
  auxlinha=1 #controle linha da matriz peso_subcriterio
  for(k in seq_along(map)){
    if (map[k]==0) {peso_criterio[k,] = preferencias[[aux]]
    aux = aux+1 #controla posicao do vetor prioridade
    auxlinha = k+1 #controla número de linhas na tabela final
    tabela[l_tabela,]= c(preferencias[[1]][k],peso_criterio[k,]*objetivo[[1]][k], CR_saaty[[l_tabela]]) #modifiquei calculo peso_criterio[k,] multiplicando por P(Ck) (objetivo[[1]][k])
    l_tabela = l_tabela+1
    }
    if (map[k]>0) {

      peso_parcial_critk=matrix(nrow = map[k], ncol = m)
      for(l in (1:map[k])){

      peso_parcial_critk[l,] = preferencias[[aux]][l]* preferencias[[(aux+l)]]
      tabela[l_tabela,]=c(preferencias[[aux]][l],peso_parcial_critk[l,], CR_saaty[[l_tabela+1]]) #mudei aqui NA para preferencias[[aux]][l]

      l_tabela = l_tabela+1
      }
      peso_subcriterio[auxlinha:(auxlinha+l-1),] = peso_parcial_critk
      auxlinha = auxlinha+l
      aux = aux+l+1
      peso_criterio[k,] = colSums(peso_parcial_critk)*objetivo[[1]][k] #acrescentei *objetivo[[1]][k]
      tabela[l_tabela,]=c(preferencias[[1]][k],peso_criterio[k,],CR_saaty[[(l_tabela-map[k])]])

      l_tabela = l_tabela+1

    }

  }

  #Novo calculo do peso_criterio
peso_criterio1 = objetivo[[1]]*peso_criterio

#nomeia linhas da tabela contendo pesos de criterios e subcriterios
#uso seq criado anteriormente para numerar os subcritérios

  linha=1
  nome_tabela=NULL
  for(j in seq_along(map)){
    if(map[j]>0){
    for(i in 1:map[j]){
     nome_tabela=append(nome_tabela,paste0("--SC",j,i))}
    }
    nome_tabela=append(nome_tabela,paste0("-C",j))}

  rownames(tabela) = c("Alternatives->",nome_tabela)
  colnames(tabela) = c("Weithts",paste0("A",1:m), "CR")

  tabela #confere até aqui

  tabela1 = tibble::tibble(criteria=rownames(tabela), tibble::as_tibble(tabela))


return(dplyr::as_tibble(tabela1))
}
