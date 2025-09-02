#Testing the weighted slice computations
n <- 10
Delta <- 2

#Case 1
lambda1 <- 1/4
testfct_exp <- function(x){trawl_Exp(x,lambda1)}
slices_test <- ComputeSliceSizes(n, Delta, testfct_exp)

p <-function(x){sin(2*pi*x)}


weights <-numeric(n+1)
for(i in 1:(n+1)){
  weights[i]<-p(i*Delta)
}

weightedtrawlsets <- AddWeightedSlices(slices_test,weights)
weightedtrawlsets_Rcpp <- AddWeightedSlices_Rcpp(slices_test,weights)
### Check dimensions of output
expect_equal(length(weightedtrawlsets),n+1,tolerance=0.0001)
expect_equal(length(weightedtrawlsets_Rcpp),n+1,tolerance=0.0001)
##Check that both additions lead same result
for(i in 1:n){
  expect_equal(weightedtrawlsets[i],weightedtrawlsets_Rcpp[i],tolerance=0.0001)
}

#Case 2
delta  <- 1/4
gamma <- 2
testfct_supIG <- function(x){trawl_supIG(x,delta, gamma)}
slices_test <- ComputeSliceSizes(n, Delta, testfct_supIG)

p <-function(x){sin(x)*exp(-0.1*x)}


weights <-numeric(n+1)
for(i in 1:(n+1)){
  weights[i]<-p(i*Delta)
}

weightedtrawlsets <- AddWeightedSlices(slices_test,weights)
weightedtrawlsets_Rcpp <- AddWeightedSlices_Rcpp(slices_test,weights)
### Check dimensions of output
expect_equal(length(weightedtrawlsets),n+1,tolerance=0.0001)
expect_equal(length(weightedtrawlsets_Rcpp),n+1,tolerance=0.0001)
##Check that both additions lead same result
for(i in 1:n){
  expect_equal(weightedtrawlsets[i],weightedtrawlsets_Rcpp[i],tolerance=0.0001)
}
