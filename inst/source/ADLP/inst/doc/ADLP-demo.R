## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
 )


## ----setup--------------------------------------------------------------------
library(ADLP)
if (!require(SynthETIC)) install.packages("SynthETIC")
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

# Import dummy claims dataset
claims_dataset <- claims_dataset <- ADLP::test_claims_dataset

head(claims_dataset)

## -----------------------------------------------------------------------------

# Included train/validation splitting function
train_val <- ADLP::train_val_split_method1(
    df = claims_dataset,
    tri.size = 40,
    val_ratio = 0.3,
    test = TRUE
)

train_data <- train_val$train
valid_data <- train_val$valid
insample_data <- rbind(train_data, valid_data)
test_data <- train_val$test

print("Number of observations in each row")
print(nrow(train_data))
print(nrow(valid_data))
print(nrow(test_data))

print("Approximation for val_ratio")
print(nrow(valid_data)/(nrow(insample_data)))


## -----------------------------------------------------------------------------
base_model1 <- glm(formula = claims~factor(dev),
                   family=gaussian(link = "identity"), data=train_data)

tau <- 1e-8
base_model2 <- glm(formula = (claims+tau)~calendar,
                   family=Gamma(link="inverse"), data=train_data)

base_model3 <- glm(formula = claims~factor(origin),
                   family=gaussian(link = "identity"), data=train_data)

base_model1_full <- update(base_model1, data = insample_data)
base_model2_full <- update(base_model2, data = insample_data)
base_model3_full <- update(base_model3, data = insample_data)

## -----------------------------------------------------------------------------
################################################################################
# Normal distribution densities and functions
dens_normal <- function(y, model, newdata){
    pred_model <- predict(model, newdata=newdata, type="response", se.fit=TRUE)
    mu <- pred_model$fit
    sigma <- pred_model$residual.scale
    return(dnorm(x=y, mean=mu, sd=sigma))
}

cdf_normal<-function(y, model, newdata){
    pred_model <- predict(model, newdata=newdata, type="response", se.fit=TRUE)
    mu <- pred_model$fit
    sigma <- pred_model$residual.scale
    return(pnorm(q=y, mean=mu, sd=sigma))
}

mu_normal<-function(model, newdata){
    mu <- predict(model, newdata=newdata, type="response")
    mu <- pmax(mu, 0)
    return(mu)
}

sim_normal<-function(model, newdata){
    pred_model <- predict(model, newdata=newdata, type="response", se.fit=TRUE)
    mu <- pred_model$fit
    sigma <- pred_model$residual.scale
    
    sim <- rnorm(length(mu), mean=mu, sd=sigma)
    sim <- pmax(sim, 0)
    return(sim)
}

################################################################################
# Gamma distribution densities and functions
dens_gamma <- function(y, model, newdata){
    pred_model <- predict(model, newdata=newdata, type="response", se.fit=TRUE)
    mu <- pred_model$fit
    sigma <- pred_model$residual.scale
    return(dgamma(x=y, shape = 1/sigma, scale=(mu*sigma)^2))
}

cdf_gamma <- function(y, model, newdata){
    pred_model <- predict(model, newdata=newdata, type="response", se.fit=TRUE)
    mu <- pred_model$fit
    sigma <- pred_model$residual.scale
    return(pgamma(q=y, shape = 1/sigma, scale=(mu*sigma)^2))
}

mu_gamma <- function(model, newdata, tau){
    mu <- predict(model, newdata=newdata, type="response")
    mu <- pmax(mu - tau, 0)
    return(mu)
}

sim_gamma <- function(model, newdata, tau){
    pred_model <- predict(model, newdata=newdata, type="response", se.fit=TRUE)
    mu <- pred_model$fit
    sigma <- pred_model$residual.scale
    
    sim <- rgamma(length(mu), shape = 1/sigma, scale=(mu*sigma)^2)
    sim <- pmax(sim - tau, 0)
    return(sim)
}


## -----------------------------------------------------------------------------
# Constructing ADLP component classes

base_component1 = adlp_component(
    model_train = base_model1, 
    model_full = base_model1_full, 
    calc_dens = dens_normal,
    calc_cdf = cdf_normal,
    calc_mu = mu_normal,
    sim_fun = sim_normal
)

base_component2 = adlp_component(
    model_train = base_model2, 
    model_full = base_model2_full, 
    calc_dens = dens_gamma,
    calc_cdf = cdf_gamma,
    calc_mu = mu_gamma,
    sim_fun = sim_gamma,
    tau = tau
)

base_component3 = adlp_component(
    model_train = base_model3, 
    model_full = base_model3_full, 
    calc_dens = dens_normal,
    calc_cdf = cdf_normal,
    calc_mu = mu_normal,
    sim_fun = sim_normal
)

components <- adlp_components(
    base1 = base_component1,
    base2 = base_component2,
    base3 = base_component3
)


## ---- error = T---------------------------------------------------------------
# Fitting ADLP class
fit1 <- adlp(
    components_lst = components,
    newdata = valid_data,
    partition_func = ADLP::adlp_partition_none
)

fit2 <- adlp(
    components_lst = components,
    newdata = valid_data,
    partition_func = ADLP::adlp_partition_ap,
    tri.size = 40,
    size = 3,
    weights = c(3, 1, 2)
)


fit1
fit2

## -----------------------------------------------------------------------------
# Log Score
ensemble_logS <- adlp_logS(fit1, test_data, model = "full")

# Cumulative Ranked Probability Score
ensemble_CRPS <- adlp_CRPS(fit1, test_data, response_name = "claims", model = "full", sample_n = 1000)

# Predictions
ensemble_means <- predict(fit1, test_data)

# Simulations
ensemble_simulate <- adlp_simulate(100, fit1, test_data)

boxplot(ensemble_logS$dens_val, main = "Log Score")
boxplot(ensemble_CRPS$ensemble_crps, main = "Cumulative Ranked Probability Score")
boxplot(ensemble_means$ensemble_mu, main = "Predictions")
boxplot(ensemble_simulate$simulation, main = "Simulations")

## -----------------------------------------------------------------------------
# Defining own partition function
user_defined_partition <- function(df) {
    return (list(
        subset1 = df[(as.numeric(as.character(df$origin)) >= 1) & (as.numeric(as.character(df$origin)) <= 15), ],
        subset2 = df[(as.numeric(as.character(df$origin)) >= 16) & (as.numeric(as.character(df$origin)) <= 40), ]
    ))
}

# Fitting ADLP class
fit3 <- adlp(
    components_lst = components,
    newdata = valid_data,
    partition_func = user_defined_partition
)

fit3

## -----------------------------------------------------------------------------
show_mse <- function(data) {
    print("----------------------------")
    print("Base models 1, 2, 3")
    print(sum((predict(base_model1_full, data, type = "response") - data$claims)^2))
    print(sum((predict(base_model2_full, data, type = "response") - data$claims)^2))
    print(sum((predict(base_model3_full, data, type = "response") - data$claims)^2))
    
    print("ADLP ensembles 1, 2, 3")
    print(sum((predict(fit1, data)$ensemble_mu - data$claims)^2))
    print(sum((predict(fit2, data)$ensemble_mu - data$claims)^2))
    print(sum((predict(fit3, data)$ensemble_mu - data$claims)^2))
    print("----------------------------")
}

show_mse(train_data)
show_mse(valid_data)
show_mse(test_data)

