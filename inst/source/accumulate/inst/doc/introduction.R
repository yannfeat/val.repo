# load_package
library(accumulate)
# check the package version
packageVersion("accumulate")

# loading_data
data(producers)
head(producers)

# first_example
a <- accumulate(producers
              , collapse = sbi*size ~ sbi
              , test = min_records(5)
              , fun  = mean, na.rm=TRUE)
head(round(a))

# cumulate_formula
a <- cumulate(producers, collapse = sbi*size ~ sbi
      , test = function(d) nrow(d) >= 5
      , mu_industrial = mean(industrial, na.rm=TRUE)
      , sd_industrial = sd(industrial, na.rm=TRUE))

head(round(a))

# derive_sbi_levels
producers$sbi3 <- substr(producers$sbi,1,3)
producers$sbi2 <- substr(producers$sbi,1,2)
head(producers,3)

# accumulate_formula
a <- accumulate(producers, collapse = sbi*size ~ sbi + sbi3 + sbi2
               , test = min_records(5), fun = mean, na.rm=TRUE)
head(round(a))

# dataframe_construction
sbi <- unique(producers$sbi)
csh <- csh_from_digits(sbi)
names(csh)[1] <- "sbi"
head(csh)

# dataframe_cumulate
a <- cumulate(producers, collapse = csh, test = function(d) nrow(d) >= 5
       , mu_total = mean(total, na.rm=TRUE)
       , sd_total = sd(total, na.rm=TRUE))
head(a)

# helpers
# load the data again to loose columns 'sbi2' and 'sbi3' and work
# with the original data.
data(producers)

# 1. using a helper function
a <- accumulate(producers, collapse = sbi*size ~ sbi
               , test = min_records(5)
               , fun  = mean, na.rm=TRUE)

# 2. using a 'validator' object
rules <- validate::validator(nrow(.) >= 5)
a <- accumulate(producers, collapse = sbi*size ~ sbi
               , test = from_validator(rules)
               , fun  = mean, na.rm=TRUE)

# 3. using a custom function
a <- accumulate(producers, collapse=sbi*size ~ sbi
               , test = function(d) nrow(d) >= 5
               , fun  = mean, na.rm=TRUE)

# complex
a <- cumulate(producers, collapse = sbi*size ~ sbi
                       , test = min_complete(5, c("other_income","trade"))
                       , model = lm(other_income ~ trade)
                       , mean_other = mean(other_income, na.rm=TRUE))

head(a)

# objlist
a$model[[1]]
a$model[[2]]

# smoketest1
my_test <- function(d) sum(other != 0) > 3
smoke_test(producers, my_test)

# smoketest2
my_test <- function(d) sum(d$other != 0) > 3
smoke_test(producers, my_test)

# smoketest3
my_test <- function(d) sum(d$other != 0,na.rm=TRUE) > 3
smoke_test(producers, my_test)

