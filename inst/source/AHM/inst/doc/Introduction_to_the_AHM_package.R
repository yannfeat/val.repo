## ---- echo = F, eval = TRUE, message = F, error = F----------------------
library(AHM)
library(mixexp)
if (0) {
  library(devtools); load_all()
} 

## ---- echo = TRUE, eval = T----------------------------------------------
data("coating")
dat = coating
h_tmp = 1.1

x = dat[,c("c1","c2","x11","x12","x21","x22")]
y = dat[,ncol(dat)]
ptm <- proc.time()
out = ahm (y, x, num_major = 2, dist_minor = c(2,2),
                   type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfold = NULL,
               mapping_type = c("power"), powerh = h_tmp,
                   rep_gcv=100)    
proc.time() - ptm 
summary(out)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  powerh_path = round(seq(0.001,2,length.out =15),3)
#  
#  res = cv.ahm (y, x, powerh_path=powerh_path, metric = "mse", num_major=2, dist_minor=c(2,2), type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfolds=NULL,     mapping_type = c("power"), rep_gcv=100)
#  
#  object = res$metric_mse

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  data("pringles_fat")
#  data_fat = pringles_fat
#  h_tmp = 1.3
#  
#  x = data_fat[,c("c1","c2","c3","x11","x12","x21","x22")]
#  y = data_fat[,1]
#  ptm <- proc.time()
#  out = ahm (y, x, num_major = 3, dist_minor = c(2,2,1),
#                     type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfold = NULL,
#                 mapping_type = c("power"), powerh = h_tmp,
#                     rep_gcv=100)
#  proc.time() - ptm

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  summary(out)
#  
#  coefficients = coef(out)
#  fitted = predict(out, x)
#  

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  data("pringles_hardness")
#  dat = pringles_hardness
#  h_tmp = 1.3
#  
#  x = dat[,c("c1","c2","c3","x11","x12","x21","x22")]
#  y = dat[,1]
#  ptm <- proc.time()
#  out = ahm (y, x, num_major = 3, dist_minor = c(2,2,1),
#                     type = "weak", alpha=0, lambda_seq=seq(0,5,0.01), nfold = NULL,
#                 mapping_type = c("power"), powerh = h_tmp,
#                     rep_gcv=100)
#  proc.time() - ptm
#  summary(out)

