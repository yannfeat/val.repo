## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
library(AHPWR)
library(kableExtra)

## -----------------------------------------------------------------------------
#generic, c= 4 criteria and a = 3 alternatives
flow_chart(names=NULL, c=4, a=3)

## -----------------------------------------------------------------------------
#generic, c= 4 criteria and a = 3 alternatives
p=flow_chart(names=NULL, c=4, a=3)
p+ggplot2::labs(title = "A tree level hierarchy", x="", y="")

## -----------------------------------------------------------------------------
#generic, c= 4 criteria and a = 3 alternatives
p=flow_chart(names=NULL, c=4, a=3)
p+ggplot2::labs(title = "A tree level hierarchy", x="", y="")+ggplot2::theme_void()

## -----------------------------------------------------------------------------
#generic, c= 4 criteria and a = 3 alternatives
goal = "Satisfation with House"
criterios = c("Size", "Age", "Yard", "Neighborhood" )
alternatives = c("house A", "house B", "house C")
names = c(goal, criterios, alternatives)
p=flow_chart(names, c=4, a=3)
p+ggplot2::labs(title = "A tree level hierarchy", x="", y="")+ggplot2::theme_void()

## ----echo=FALSE---------------------------------------------------------------
`Intensity of importance` = 1:9
Definicion = c("Equal Importance", 
               " Weak",
               "Moderate importance",
               "Moderate plus",
               "Strong importance",
               "Strong plus",
               "Very strong or demonstrated importance",
               "Very, very strong",
               "Extreme importance")
tab = data.frame(`Intensity of importance`, Definicion)
knitr::kable(tab,  caption = "Table 1: The fundamental Scale")

## -----------------------------------------------------------------------------
x = c("life cycle", "maintenance cost", "environmental impacts", "construction cost") #criteria
y = c(2,5,2,3) #weights
m1 = matrix_ahp(x,y)
m1

## -----------------------------------------------------------------------------
names(y) = x
table=tabela_holistica(pesos=y)
table
knitr::kable(table)

## -----------------------------------------------------------------------------
require(magrittr)
require(kableExtra)
knitr::kable(as.data.frame(table), align = 'c', digits = 2) %>%
  row_spec(1, italic = TRUE, background = 'gray') %>% 
  row_spec(2:5, color = 'black', background = 'yellow') %>%
  row_spec(6, underline = TRUE,  color = 'black',background = 'gray',bold = TRUE,) %>% 
   column_spec(6, background = 'gray')

## -----------------------------------------------------------------------------
x = c("bridge", "tunnel") #criteria life cycle
y = c(1,3) #weights
m2 = matrix_ahp(x,y)
m2

## -----------------------------------------------------------------------------
names(y) = x
table=tabela_holistica(pesos=y)
table

## -----------------------------------------------------------------------------
x = c("bridge", "tunnel") #criteria maintenance cost
y = c(1,4) #weights
m3 = matrix_ahp(x,y)
m3

## -----------------------------------------------------------------------------
names(y) = x
table=tabela_holistica(pesos=y)
table

## -----------------------------------------------------------------------------
x = c("bridge", "tunnel") #criteria environmental impacts
y = c(1,2) #weights
m4 = matrix_ahp(x,y)
m4

## -----------------------------------------------------------------------------
names(y) = x
table=tabela_holistica(pesos=y)
table

## -----------------------------------------------------------------------------
x = c("bridge", "tunnel") #criteria construction cost
y = c(5,3) #weights
m5 = matrix_ahp(x,y)
m5

## -----------------------------------------------------------------------------
names(y) = x
table=tabela_holistica(pesos=y)
table

## -----------------------------------------------------------------------------
#consistency index
CI(m1) 
CI(m2)
CI(m3)
CI(m4)
CI(m5)


## -----------------------------------------------------------------------------
#consistency ratio
CR(m1) 
CR(m2)
CR(m3)
CR(m4)
CR(m5)


## -----------------------------------------------------------------------------

lista = list(m1, m2, m3, m4, m5)
calcula_prioridades(lista)


## -----------------------------------------------------------------------------
lista
ahp_geral(lista)

## -----------------------------------------------------------------------------
x=paste0(letters[3],1:5) #criteria names C1, C2, ..., C5
y=c(5,2,7,3,2) #judgments
m1=matrix_ahp(x,y) 
x=paste0(letters[1],1:3) #alternatives names A1, A2, A3
y=c(4.4,5.2,3)
m2=matrix_ahp(x,y)
y=c(2,4,3)
m3=matrix_ahp(x,y)
y=c(4.9,5,3.3)
m4=matrix_ahp(x,y)
y=c(4.4,4.2,4.3)
m5=matrix_ahp(x,y)
y=c(5.4,5.2,5.7)
m6=matrix_ahp(x,y)
base=list(m1, m2, m3, m4, m5, m6)
base

calcula_prioridades(base) #fornece somente os vetores prioridades
lapply(base,tabela_holistica) #fornece uma tabela com a matriz de comparação o vetor prioridade e o CR.

ahp_geral(base)

## -----------------------------------------------------------------------------
table1 = ahp_geral(base)
transforma_tabela(table1)

## -----------------------------------------------------------------------------
formata_tabela(table1)
formata_tabela(table1, cores = "GRAY")
formata_tabela(table1, cores = "WHITE")


## -----------------------------------------------------------------------------
ranque(table1)

## -----------------------------------------------------------------------------
#two criteria, each with two subcriteria
map = c(2,2)
#x with names and y with holistic judgment
x=paste0(letters[3],1:2) #2 criteria
y=c(5,7)
m1=matrix_ahp(x,y) # matrix compare two criteria
x=paste0("SC1",1:2)
y=c(4,6)
m2=matrix_ahp(x,y) # matrix compare two subcriteria of criteria 1
x=paste0(letters[1],1:3)
y=c(2,4,5)
m3=matrix_ahp(x,y) #alternatives for subcriteria 1 - criteria 1
y=c(4.9,5, 2)
m4=matrix_ahp(x,y) #alternatives for subcriteria 2 - criteria 1
y=c(4.4,8, 6)
x=paste0("SC2",1:2)
m5=matrix_ahp(x,y) #matrix compare two subcriteria of criteria 2
y=c(5.4,5.2, 1)
x=paste0(letters[1],1:3)
m6=matrix_ahp(x,y) #alternatives for subcriteria 1 - criteria 2
y=c(9,5.2, 3)
m7=matrix_ahp(x,y) #alternatives for subcriteria 2 - criteria 2




base=list(m1, m2, m3, m4, m5, m6, m7)
base

## -----------------------------------------------------------------------------
#Priority vector and CR
#
calcula_prioridades(base) #fornece somente os vetores prioridades
lapply(base,tabela_holistica) #fornece uma tabela com a matriz de comparação o vetor prioridade e o CR.


ahp_s(base,map)

tb = ahp_s(base,map)

transforma_tabela(tb)

formata_tabela(tb)



## -----------------------------------------------------------------------------
p1=c(2,4,5,1,6,3) #holistcs weights for compare 6 criteria
p2=c(5, 4, 6, 7) #holistcs weights for compare 4 alternatives for criterion 1
p3=c(2, 8, 2, 7) #holistcs weights for compare 4 alternatives for criterion 2
p4=c(5, 1, 4, 1) #holistcs weights for compare 4 alternatives for criterion 3
p5=c(3.4, 4, 2, 3) #holistcs weights for compare 4 alternatives for criterion 4
p6=c(6, 4, 2, 2.5) #holistcs weights for compare 4 alternatives for criterion 5
p7=c(5, 3, 6, 1.8) #holistcs weights for compare 4 alternatives for criterion 6

x1=paste0("C",1:6)
x= paste0("A",1:4)

m1 = matrix_ahp(x1,p1)
m2 = matrix_ahp(x,p2)
m3 = matrix_ahp(x,p3)
m4 = matrix_ahp(x,p4)
m5 = matrix_ahp(x,p5)
m6 = matrix_ahp(x,p6)
m7 = matrix_ahp(x,p7)

base=list(m1,m2, m3, m4, m5, m6, m7)
formata_tabela(ahp_geral(base))
formata_tabela(ahp_s(base, map=c(0,0,0,0,0,0)))


## ----echo=FALSE---------------------------------------------------------------
#para checar o pacote
#devtools::check(args = c("--as-cran"), check_dir = dirname(getwd()))

