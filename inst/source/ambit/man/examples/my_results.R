#Simulate i.i.d.~standard normal data
set.seed(456)
data <- rnorm(10000)
#Display the sample mean, standard deviation and coverage probabilities:
my_results(data)
