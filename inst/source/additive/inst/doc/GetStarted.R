## ----results='hide', message=FALSE, warning=FALSE-----------------------------
library(additive)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
library(recipes)
library(workflows)

## -----------------------------------------------------------------------------
set.seed(2020)
dat <- gamSim(1, n = 400, dist = "normal", scale = 2)

## -----------------------------------------------------------------------------
test_recipe <- dat |>
  recipe() |>
  update_role(y, new_role = "outcome") |>
  update_role(x0, x1, x2, x3, new_role = "predictor") |>
  step_normalize(all_numeric_predictors())

## -----------------------------------------------------------------------------
print(test_recipe)

## -----------------------------------------------------------------------------
test_model <- additive(
    family = gaussian(),
    method = "REML"
  ) |>
  set_engine("mgcv") |>
  set_mode("regression")

## -----------------------------------------------------------------------------
print(test_model)

## -----------------------------------------------------------------------------
test_model <- test_model |>
  update(family = gaussian())

## -----------------------------------------------------------------------------
test_workflow <- workflow() |>
  add_recipe(test_recipe) |>
  add_model(
    spec = test_model,
    formula = y ~ s(x0) + s(x1) + s(x2) + s(x3)
  )

## -----------------------------------------------------------------------------
print(test_workflow)

## ----results='hide', echo = FALSE---------------------------------------------
run_on_linux <- grepl("linux", R.Version()$os, ignore.case = TRUE)

## ----results='hide', eval = run_on_linux--------------------------------------
test_workflow_fit <- test_workflow |>
  fit(data = dat)

## ----eval = run_on_linux------------------------------------------------------
print(test_workflow_fit)

## ----eval = run_on_linux------------------------------------------------------
test_fit <- test_workflow_fit |>
  extract_fit_parsnip()

## ----eval = run_on_linux------------------------------------------------------
gam_fit <- test_workflow_fit |>
  extract_fit_engine()

## ----eval = run_on_linux------------------------------------------------------
class(gam_fit)

## -----------------------------------------------------------------------------
newdata <- dat[1:5, ]

## ----eval = run_on_linux------------------------------------------------------
test_workflow_fit |>
  predict(
    new_data = newdata,
    type = "conf_int",
    level = 0.95
  )

## ----eval = run_on_linux------------------------------------------------------
test_workflow_fit |>
  predict(
    new_data = newdata,
    type = "conf_int",
    level = 0.95,
    std_error = TRUE
  )

