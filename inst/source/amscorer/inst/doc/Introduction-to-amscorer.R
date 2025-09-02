## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(amscorer)

## ----my_data_cci--------------------------------------------------------------
# Example data for CCI
set.seed(123)
n <- 10
my_data <- data.frame(
  age = sample(30:90, n, replace = TRUE), # age
  mi = sample(0:1, n, replace = TRUE), # Myocardial infraction
  chf = sample(0:1, n, replace = TRUE), # Congestive heart failure
  pvd = sample(0:1, n, replace = TRUE), # preripheral vascular disease
  cevd = sample(0:1, n, replace = TRUE), # Cerebrovascular accident or Transient ischemic attack
  dementia = sample(0:1, n, replace = TRUE), # Dematia
  cpd = sample(0:1, n, replace = TRUE),# Chronic obstructive pulmonary disease
  ctd = sample(0:1, n, replace = TRUE),# Connective tissue disease
  pud = c(sample(0:1, (n-1), replace = TRUE) , NA), # peptide ulcer disease
  liver_disease = sample(0:2, n, replace = TRUE), #Liver disease(None,Mild,Moderate to severe)
  diabetes_mellitus = sample(0:2, n, replace = TRUE),#Diabetes(None,uncomplicated,End-organ)
  hp = sample(0:1, n, replace = TRUE), # Hemipledia
  ckd = sample(0:1, n, replace = TRUE), #Moderate to severe Chronic kidney disease
  solid_tumor = sample(0:2, n, replace = TRUE), #Solid tumor(None,Localized,Metastatic)
  leuk = sample(0:1, n, replace = TRUE), # Leukemia
  lym =  c(sample(0:1, (n-2), replace = TRUE) , c(NA , NA)), # Lymphoma
  aids = sample(0:1, n, replace = TRUE) # AIDS
)

## -----------------------------------------------------------------------------
amscorer::cci(my_data ,replace_na_with_zero = FALSE) 

## -----------------------------------------------------------------------------
amscorer::cci(my_data ,replace_na_with_zero = FALSE)$cci_score  

## ----my_data_EPICES-----------------------------------------------------------
# Example data for EPICES
my_data <- data.frame(
  epices_1 = c(1, 0, 1),
  epices_2 = c(0, 1, 1),
  epices_3 = c(0, 0, 0),
  epices_4 = c(1, 0, 0),
  epices_5 = c(0, 1, 0),
  epices_6 = c(1, 0, 1),
  epices_7 = c(0, 1, 0),
  epices_8 = c(0, 0, 1),
  epices_9 = c(1, 1, 0),
  epices_10 = c(0, 0, 1),
  epices_11 = c(1, 0, NA)
)

## -----------------------------------------------------------------------------
amscorer::epices_score(my_data ,prefix = "epices",replace_na_with_zero = FALSE)  

## -----------------------------------------------------------------------------
amscorer::epices_score(my_data ,prefix = "epices",replace_na_with_zero = FALSE)$epices_score

## -----------------------------------------------------------------------------
amscorer::epices_score(my_data ,prefix = "epices",replace_na_with_zero = TRUE)$epices_score

## ----my_data_MELD_3-----------------------------------------------------------
# Example data for MELD 3.0
my_data <- data.frame(
  Sex = c(1, 0, 1),
  Creatinine = c(1.5, 2.0, 3.1),
  Bilirubin = c(1.2, 2.5, 1.8),
  INR = c(1.1, 1.4, 2.0),
  Sodium = c(135, 130, 140),
  Albumin = c(3.0, 2.5, 3.5)
)

## -----------------------------------------------------------------------------
amscorer::meld_3(my_data) 

## ----my_data_MELD_pre_2016----------------------------------------------------
my_data <- data.frame(
  Hemodialysis = c(0, 0, 1),
  Creatinine = c(1.2, 0.9, 1.5),
  Bilirubin = c(0.7, 1.1, 0.9),
  INR = c(1.0, 1.2, 1) 
)

## -----------------------------------------------------------------------------
amscorer::meld_pre_2016(my_data) 

## ----my_data_MELD_NA----------------------------------------------------------
my_data <- data.frame(
  Creatinine = c(1.2, 2.5, 3),
  Bilirubin = c(0.5, 1.0, 2.1),
  INR = c(1.1, 1.5, 1.8),
  Sodium = c(136, 140, 145),
  Hemodialysis = c(0, 1, 0)
)

## -----------------------------------------------------------------------------
amscorer::meld_NA(my_data) 

## ----my_data_io_DFRS----------------------------------------------------------
my_data <- data.frame(
  ID = 1:4,
  BMI = c(20, 25, 30, 10),
  PT = c(5, 43, 1, 20),
  PD_size = c(100, 0, 1, 19),
  OP_time = c(500, 20, 605, 600),
  Texture = c(0, 1, 0, 1)
)

## -----------------------------------------------------------------------------
amscorer::io_DFRS(my_data)

## ----my_data_preop_DFRS-------------------------------------------------------
my_data <- data.frame(
  ID = 1:4,
  PT = c(5, 43, 3, 4),
  PD_size = c(25, 5, 4, 19)
)

## -----------------------------------------------------------------------------
amscorer::preop_DFRS(my_data)

## ----my_data_a_FRS------------------------------------------------------------
my_data <- data.frame(
  ID = 1:4,
  Texture = c(0, 1, 0,0),
  BMI = c(22, 25, 30 , 20),
  PD_size = c(5, 10, 1,2)
)

## -----------------------------------------------------------------------------
amscorer::a_FRS(my_data)

## ----my_data_ua_FRS-----------------------------------------------------------
my_data <- data.frame(
  ID = 1:4,
  Sex = c(0, 0, 1,1),
  Texture = c(0, 1, 0,0),
  BMI = c(22, 25, 30 , 20),
  PD_size = c(5, 10, 1,2)
)

## -----------------------------------------------------------------------------
amscorer::ua_FRS(my_data)

