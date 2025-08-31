## ----eval=FALSE, include=TRUE-------------------------------------------------
# # install.packages("AgePopDenom")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("truenomad/AgePopDenom")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# library(AgePopDenom)

## ----eval=FALSE, include=TRUE-------------------------------------------------
# init(
#   r_script_name = "full_pipeline.R",
#   cpp_script_name = "model.cpp"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# download_dhs_datasets(
#   country_codes = c("GMB"),
#   email = "my_email@example.com",
#   project = "Population project"
# )
# 
# process_dhs_data()

## ----eval=FALSE, include=TRUE-------------------------------------------------
# download_shapefile("GMB")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# download_pop_rasters("GMB")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# extract_afurextent()

## ----eval=FALSE, include=TRUE-------------------------------------------------
# run_full_workflow("GMB")

## ----eval=FALSE, include=TRUE-------------------------------------------------
# fit_spatial_model(
#   country_code,
#   data,
#   scale_outcome = "log_scale",
#   shape_outcome = "log_shape",
#   covariates = "urban",
#   cpp_script_name = "02_scripts/model",
#   output_dir = "03_outputs/3a_model_outputs"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# control_params = list(
#   trace = 3,        # Higher values show more optimization details
#   maxit = 2000,     # Increase for complex spatial structures
#   abs.tol = 1e-10,  # Stricter convergence criteria
#   rel.tol = 1e-8    # Relative convergence tolerance
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# fit_spatial_model(
#   data = survey_data,
#   scale_outcome = "log_scale",
#   shape_outcome = "log_shape",
#   covariates = "urban",
#   cpp_script_name = "02_scripts/model",
#   manual_params = list(
#     beta1 = c(0.5, -0.3),
#     beta2 = c(0.2, 0.1),
#     gamma = 0.8,
#     log_sigma2 = log(0.5),
#     log_phi = log(100),
#     log_tau2_1 = log(0.1)
#   ),
#   control_params = list(
#     trace = 3,
#     maxit = 2000,
#     abs.tol = 1e-10
#   )
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_variogram_plot(
#   age_param_data,
#   fit_vario,
#   country_code,
#   scale_outcome = "log_scale",
#   output_dir = "03_outputs/3b_visualizations",
#   width = 12,
#   height = 9,
#   png_resolution = 300
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# create_prediction_data(
#   country_code,
#   country_shape,
#   pop_raster,
#   ur_raster,
#   adm2_shape,
#   cell_size = 5000,
#   ignore_cache = FALSE,
#   output_dir = "03_outputs/3a_model_outputs"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_gamma_predictions(
#   country_code,
#   age_param_data,
#   model_params,
#   predictor_data,
#   shapefile,
#   cell_size = 5000,
#   n_sim = 5000,
#   ignore_cache = FALSE,
#   output_dir = "03_outputs/3a_model_outputs"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_gamma_raster_plot(
#   predictor_data,
#   pred_list,
#   country_code,
#   output_dir = "03_outputs/3b_visualizations",
#   save_raster = TRUE
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_age_pop_table(
#   predictor_data,
#   scale_pred,
#   shape_pred,
#   country_code,
#   age_range = c(0, 99),
#   age_interval = 1,
#   ignore_cache = FALSE,
#   output_dir = "03_outputs/3c_table_outputs"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# generate_age_pyramid_plot(
#   dataset,
#   country_code,
#   output_dir = "03_outputs/3b_visualizations"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# process_final_population_data(
#   input_dir = "03_outputs/3c_table_outputs",
#   excel_output_file = "03_outputs/3d_compiled_results/age_pop_denom_compiled.xlsx"
# )

## ----eval=FALSE, include=TRUE-------------------------------------------------
# # Get Package ------------------------------------------------------------------
# 
# # install package
# install.packages("AgePopDenom")
# 
# # Working directory ------------------------------------------------------------
# 
# # set working directory based on script location if not using an R project
# # this ensures relative paths work as expected outside .Rproj environments
# 
# # get script path
# current_file_path <- AgePopDenom::get_current_script_path()
# script_dir <- dirname(current_file_path)
# 
# # set working directory to script directory if not already in a project
# if (!any(grepl("\\.Rproj$", list.files(script_dir)))) {
#   setwd(script_dir)
#   message(
#     "no .Rproj file found. using script directory as working directory: ",
#     script_dir
#   )
# } else {
#   message(".Rproj file detected. assuming project is correctly set up.")
# }
# 
# # Create directory and file Structure ------------------------------------------
# 
# AgePopDenom::init(
#   r_script_name = "full_pipeline.R",
#   cpp_script_name = "model.cpp",
#   open_r_script = FALSE
# )
# 
# # Gather and process datasets --------------------------------------------------
# 
# # set up country code
# cntry_code = "GMB"
# country = "Gambia"
# country_code_dhs = "GM"
# 
# # Simulate and save processed survey dataset for Gambia
# AgePopDenom::simulate_dummy_dhs_pr(
#   country = country,
#   country_code_iso3 = cntry_code,
#   country_code_dhs = country_code_dhs,
#   year_of_survey = 2024,
#   output_path = here::here(
#     "01_data",
#     "1a_survey_data",
#     "processed",
#     "dhs_pr_records_combined.rds"
#   )
# )
# 
# # download shapefiles
# AgePopDenom::download_shapefile(cntry_code)
# 
# # download population rasters from worldpop
# AgePopDenom::download_pop_rasters(cntry_code)
# 
# # wxtract urban extent raster
# AgePopDenom::extract_afurextent()
# 
# # Run models and get outputs ---------------------------------------------------
# 
# # run the full model workflow
# AgePopDenom::run_full_workflow(cntry_code)

