## -----------------------------------------------------------------------------
#| include: false
library(allocation)
set.seed(1234)


## -----------------------------------------------------------------------------
#| message: false
#| warning: false
library(Rmpfr)


## -----------------------------------------------------------------------------
#| echo: false
print_interface = function(f) {
	fname = deparse(substitute(f))
	cat(fname, "=\n")
	str(f, give.attr = F, nchar.max = 4096, width = 80)
}


## -----------------------------------------------------------------------------
#| echo: false
print_interface(allocate_neyman)
print_interface(allocate_fixn)
print_interface(allocate_prec)


## -----------------------------------------------------------------------------
print_interface(allocation_control)


## -----------------------------------------------------------------------------
#| eval: false
# out = allocate_fixn(n0, N, S)
# allocation(out)  ## Extract allocation (n[1], ..., n[H]).
# print(out)       ## Print table with allocation and other information.


## -----------------------------------------------------------------------------
N = c(47, 61, 41)
S = sqrt(c(100, 36, 16))
lo = c(1,2,3)
hi = c(5,6,4)
n0 = 10

out1 = allocate_fixn(n0, N, S, lo, hi)
print(out1)


## -----------------------------------------------------------------------------
#| eval: false
# out1 = allocate_fixn(v0, N, S, lo, hi, control = allocation_control(verbose = TRUE))


## -----------------------------------------------------------------------------
out2 = allocate_neyman(n0, N, S)
print(out2)


## -----------------------------------------------------------------------------
print(out2, control = allocation_control(digits = 2))


## -----------------------------------------------------------------------------
allocation(out1)  ## allocate_fixn result 
allocation(out2)  ## allocate_neyman result


## -----------------------------------------------------------------------------
H = 10
v0 = mpfr(388910760, 256)^2
N = c(819, 672, 358, 196, 135, 83, 53, 40, 35, 13)
lo = c(3, 3, 3, 3, 3, 3, 3, 3, 3, 13)
S = c(330000, 518000, 488000, 634000, 1126000, 2244000, 2468000, 5869000,
	29334000, 1233311000)

print(data.frame(N, S, lo))
out1 = allocate_prec(v0, N, S, lo)
print(out1)


## -----------------------------------------------------------------------------
#| eval: false
# out1 = allocate_prec(v0, N, S, lo, control = allocation_control(verbose = TRUE))


## -----------------------------------------------------------------------------
cv = 0.042
rev = mpfr(9259780000, 256)
n = sum(N[-10] * S[-10])^2 / ((cv * rev)^2 + sum(N[-10] * S[-10]^2))
out2 = allocate_neyman(n, N[-10], S[-10])
print(out2)


## -----------------------------------------------------------------------------
allocation(out1)  ## allocate_prec result
allocation(out2)  ## allocate_neyman result

