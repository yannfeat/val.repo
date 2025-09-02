my_p <- function(x){sin(x)}
my_g <- function(x){exp(-x)}
kernel_generic_trawl_acf(my_p, my_g, 1)

kernel_generic_trawl_acf(my_p, my_g, c(1,2,3,4))
