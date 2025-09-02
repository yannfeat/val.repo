#Slice computations

n <- 10
Delta <- 2
lambda1 <- 1/4
testfct_exp <- function(x){trawl_Exp(x,lambda1)}

########
slices_test <- ComputeSliceSizes(n, Delta, testfct_exp)
trawlsets <- AddSlices(slices_test)
trawlsets_fast <- AddSlices_Rcpp(slices_test)
### Check dimensions of output
expect_equal(length(slices_test[1,]),n+1,tolerance=0.0001)#number of columns
expect_equal(length(slices_test[,1]),n+1,tolerance=0.0001)#number of rows
expect_equal(length(trawlsets),n+1,tolerance=0.0001)
expect_equal(length(trawlsets_fast),n+1,tolerance=0.0001)

###Check that slow and fast addition result in the same result
expect_equal(trawlsets[1],trawlsets_fast[1],tolerance=0.0001)
expect_equal(base::length(trawlsets),base::length(trawlsets_fast))
###Check that all elements in the vector are identical
for(i in 2:n){
  expect_equal(trawlsets_fast[i],trawlsets_fast[i-1],tolerance=0.0001)
  expect_equal(trawlsets[i],trawlsets[i-1],tolerance=0.0001)
}
###Check correctness of slices (leb(A))
expect_equal(trawlsets[1],1/lambda1,tolerance=0.0001)
expect_equal(trawlsets_fast[1],1/lambda1,tolerance=0.0001)
######

###Check that 1-weighted slices are the same as uneighted slices
weights <-numeric(n+1)+1
weightedtrawlsets <- AddWeightedSlices_Rcpp(slices_test, weights)
expect_equal(weightedtrawlsets[1],trawlsets_fast[1],tolerance=0.0001)
expect_equal(weightedtrawlsets[10],trawlsets_fast[10],tolerance=0.0001)
expect_equal(weightedtrawlsets[50],trawlsets_fast[50],tolerance=0.0001)
