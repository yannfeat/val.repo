## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", width = 68)

## ----logo, echo=FALSE, warning=FALSE, message=FALSE---------------------------
biocopy_colors <- c("#958BB2", "#C61E19", "#99CFE9", "#A2C510", "#FBB800")

library(ggplot2)
library(dplyr)
if (!rlang::is_installed("htmltools")) install.packages("htmltools")

## ----load, message=FALSE------------------------------------------------------
library(anabel)
packageVersion("anabel")

## ----dataset_normal-----------------------------------------------------------
data("SCA_dataset")
data("MCK_dataset")
data("SCK_dataset")

## ----help_pages, eval=FALSE---------------------------------------------------
# help(package = "anabel")
# ?SCA_dataset
# ?MCK_dataset
# ?SCK_dataset

## ----func, eval=FALSE---------------------------------------------------------
# ?convert_toMolar() # show help page
# ?run_anabel() # show help page

## ----convert_2molar-----------------------------------------------------------
# one value in case of SCA method
ac <- convert_toMolar(val = 50, unit = "nM")
# vector in case of SCK and MCK methods
ac_mck <- convert_toMolar(val = c(50, 16.7, 5.56, 1.85, 6.17e-1), unit = "nM")
ac_sck <- convert_toMolar(val = c(6.17e-1, 1.85, 5.56, 16.7, 50), unit = "nM")

## ----models, echo=FALSE, eval=FALSE-------------------------------------------
# htmltools::img(
#   src = knitr::image_uri("vignettes/strategies.png"),
#   alt = "models",
#   style = "padding:10px;width:100%; border:0"
# )

## ----sca_input, echo=FALSE, fig.dim=c(5,3), fig.align='center'----------------
myTable <- data.frame(
  Curve = paste("Sample.", LETTERS[1:3]),
  Ka = c(1e+6, 1e+6, 1e+6),
  Kd = c(1e-2, 5e-2, 1e-3),
  Conc = rep("50nM", 3),
  tass = rep(50, 3),
  tdiss = rep(200, 3)
)
myTable$Expected_KD <- myTable$Kd / myTable$Ka

kableExtra::kable(myTable) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F)

## ----sca_rev, echo=FALSE, fig.dim=c(5,3), fig.align='center'------------------
ggplot(SCA_dataset, aes(x = Time)) +
  geom_point(aes(y = Sample.A), col = "#A2C510") +
  geom_vline(xintercept = 50, linetype = 2) +
  geom_vline(xintercept = 200, linetype = 2) +
  theme_minimal()

## ----sca----------------------------------------------------------------------
sca_rslt <- run_anabel(SCA_dataset, tass = 50, tdiss = 200, conc = ac)

## ----sca_kntk, echo=FALSE-----------------------------------------------------
knitr::kable(sca_rslt$kinetics) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  kableExtra::scroll_box(width = "100%")

## ----sca_rslt, fig.align='center', fig.dim=c(8,4)-----------------------------
ggplot(sca_rslt$fit_data, aes(x = Time)) +
  geom_point(aes(y = Response), col = "#A2C510") +
  geom_path(aes(y = fit)) +
  facet_wrap(~Name, ncol = 2, scales = "free") +
  theme_light()

## ----params_mck, out.width="30%", echo=FALSE,message=F------------------------
myTable <- data.frame(
  "tass" = 45, "tdiss" = 145,
  "Kass" = "1e+7nM", "Kdiss" = "1e-2", "KD" = 1e-2 / 1e+7,
  "Conc" = "50, 16.7, 5.56, 1.85, 6.17e-1"
)

kableExtra::kable(myTable) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)

## ----mck_prev, echo=FALSE, fig.dim=c(5.5,3), fig.align='center', warning=FALSE, message=FALSE----
temp <- MCK_dataset %>% tidyr::pivot_longer(!Time, names_to = "conc", values_to = "Responce")
temp$analyte <- gsub("Conc\\.+", "", temp$conc)
ggplot(temp, aes(x = Time)) +
  geom_point(aes(y = Responce, col = analyte)) +
  geom_vline(xintercept = 50, linetype = 2) +
  geom_vline(xintercept = 150, linetype = 2) +
  theme_light() +
  scale_color_manual(values = biocopy_colors) +
  theme(legend.position = "bottom")

## ----mck----------------------------------------------------------------------
mck_rslt <- run_anabel(MCK_dataset, tass = 45, tdiss = 145, conc = ac_mck, method = "MCK")

## ----mck kntks, echo=FALSE----------------------------------------------------
knitr::kable(mck_rslt$kinetics) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  kableExtra::scroll_box(width = "100%")

## ----mck_rslts, fig.align='center', fig.dim=c(7,3)----------------------------
ggplot(mck_rslt$fit_data, aes(x = Time, group = Name)) +
  geom_point(aes(y = Response), col = "#A2C510") +
  geom_path(aes(y = fit)) +
  theme_light()

## ----params_sck, out.width="20%", echo=FALSE,message=F------------------------
myTable <- data.frame(
  Param = c("Conc", "tass", "tdiss"),
  Step1 = c(6.17e-1, 35, 145),
  Step2 = c(1.85, 205, 315),
  Step3 = c(5.56, 375, 485),
  Step4 = c(16.7, 545, 655),
  Step5 = c(50, 715, 825)
)

kableExtra::kable(myTable) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = FALSE)

## ----sck_prev, echo=FALSE, fig.dim=c(8,3), fig.align='center'-----------------
ggplot(SCK_dataset, aes(x = Time)) +
  geom_point(aes(y = Sample.A), size = 1, col = "#3373A1") +
  geom_vline(xintercept = c(35, 375, 715), linetype = 2, linewidth = 1, col = "#F08000") + # ta
  geom_vline(xintercept = c(145, 485, 825), linetype = 2, linewidth = 1, col = "#F08000") + # td
  geom_vline(xintercept = c(205, 545), linetype = 2, linewidth = 1, col = "#A2C510") + # ta
  geom_vline(xintercept = c(315, 655), linetype = 2, linewidth = 1, col = "#A2C510") + # td
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(SCK_dataset$Time), 150))

## ----sck----------------------------------------------------------------------
sck_rslt <- run_anabel(SCK_dataset,
  tass = c(35, 205, 375, 545, 715),
  tdiss = c(145, 315, 485, 655, 825), conc = ac_sck, method = "SCK"
)

## ----sck_kntks, echo =FALSE---------------------------------------------------
knitr::kable(sck_rslt$kinetics) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F) %>%
  kableExtra::scroll_box(width = "100%")

## ----sck_rslt, fig.align='center', fig.dim=c(8,3)-----------------------------
ggplot(sck_rslt$fit_data, aes(x = Time)) +
  geom_point(aes(y = Response), col = "#A2C510") +
  geom_path(aes(y = fit)) +
  facet_wrap(~Name, ncol = 2) +
  theme_light()

## ----dataset_cor--------------------------------------------------------------
data("MCK_dataset_drift") # multi cycle kinetics experiment with baseline drift
data("SCA_dataset_drift") # single curve analysis with baseline drift
data("SCK_dataset_decay") # single cycle kinetics with exponentional decay

## ----sca_drift_1, echo=FALSE, fig.dim=c(9,4), fig.align='center'--------------
df <- tidyr::pivot_longer(SCA_dataset_drift, cols = contains("Sample"))
ggplot(df, aes(Time, value)) +
  geom_point(aes(col = name), size = 0.5) +
  geom_vline(xintercept = c(50, 200), linetype = 2, linewidth = 0.5) +
  theme_light() +
  labs(y = "Response") +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(0, 1000, 100)) +
  scale_color_manual(values = biocopy_colors) +
  facet_wrap(~name, ncol = 2, scales = "free_y") +
  theme(legend.position = "none") +
  ggtitle("Five SCA sensograms with linear drift = -0.019")

## ----sca_drift, fig.align='center', fig.dim=c(9,4)----------------------------
sca_rslt_drift <- run_anabel(SCA_dataset_drift, tass = 50, tdiss = 200, conc = ac, drift = TRUE)

ggplot(sca_rslt_drift$fit_data, aes(x = Time)) +
  geom_point(aes(y = Response), col = "#A2C510") +
  geom_path(aes(y = fit)) +
  facet_wrap(~Name, ncol = 2) +
  theme_light()

## ----mck_drift, fig.align='center', fig.dim=c(8,3)----------------------------
mck_rslt_drift <- run_anabel(MCK_dataset_drift, tass = 45, tdiss = 145, conc = ac_mck, drift = TRUE, method = "MCK")

ggplot(mck_rslt_drift$fit_data, aes(x = Time, group = Name)) +
  geom_point(aes(y = Response), col = "#A2C510") +
  geom_path(aes(y = fit)) +
  theme_light() +
  ggtitle("MCK five sensogram with linear drift = -0.01")

## ----sck_decay, echo=FALSE, fig.dim=c(8,3), fig.align='center'----------------
df <- tidyr::pivot_longer(SCK_dataset_decay, cols = contains("Sample"))
ggplot(df, aes(Time, value)) +
  geom_point(size = 0.2, col = "#3373A1") +
  geom_vline(xintercept = c(50, 390, 730), linetype = 2, linewidth = 1, col = "#F08000") + # ta
  geom_vline(xintercept = c(150, 490, 830), linetype = 2, linewidth = 1, col = "#F08000") + # td
  geom_vline(xintercept = c(220, 560), linetype = 2, linewidth = 1, col = "#A2C510") + # ta
  geom_vline(xintercept = c(320, 660), linetype = 2, linewidth = 1, col = "#A2C510") + # td
  theme(legend.position = "none") +
  facet_wrap(~name, ncol = 2) +
  theme_light() +
  ggtitle("Five SCK sensograms with exponential decay")

## ----sck_decay_rslts, fig.align='center', fig.dim=c(8,3)----------------------
sck_rslt_decay <- run_anabel(SCK_dataset_decay,
  tass = c(35, 205, 375, 545, 715),
  tdiss = c(145, 315, 485, 655, 825),
  conc = ac_sck, method = "SCK", decay = TRUE
)

ggplot(sck_rslt_decay$fit_data, aes(x = Time)) +
  geom_point(aes(y = Response), col = "#A2C510") +
  geom_path(aes(y = fit)) +
  facet_wrap(~Name, ncol = 2) +
  theme_light()

## ----ff, eval=FALSE-----------------------------------------------------------
# # call anabel in debug mode with sca data set
# my_data <- run_anabel(SCA_dataset, tass = 50, tdiss = 200, conc = ac, debug_mode = TRUE)
# init_df <- my_data$init_df
# 
# # extract information of the first curve (Sample.A)
# response <- init_df$Response[1] %>%
#   strsplit(",") %>%
#   unlist() %>%
#   as.numeric()
# 
# # create a temp data frame containing both original value 'Value' and the estimated one 'Response'
# sampleA_df <- data.frame(
#   Time = SCA_dataset$Time, Value = SCA_dataset$Sample.A,
#   Response = response
# )
# 
# # Generate the plot associated with this curve
# ggplot(sampleA_df, aes(x = Time)) +
#   geom_point(aes(y = Value), col = "#A2C510", size = 0.5) +
#   geom_line(aes(y = Response)) +
#   theme_light()

## -----------------------------------------------------------------------------
# sessionInfo()

