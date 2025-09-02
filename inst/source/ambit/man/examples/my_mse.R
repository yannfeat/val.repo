#Simulate two vectors of i.i.d.~standard normal data
set.seed(456)
x <- rnorm(100)
y <- rnorm(100)
#Compute the mean squared error between both vectors
my_mse(x,y)
