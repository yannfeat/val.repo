## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(admiral)
library(admiraldev)
library(rlang)

## ----message=FALSE------------------------------------------------------------
library(admiral)
library(pharmaversesdtm, warn.conflicts = FALSE)
library(admiralpeds)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(rlang)
library(stringr)

## ----eval=TRUE----------------------------------------------------------------
who_bmi_for_age_boys <- admiralpeds::who_bmi_for_age_boys
who_bmi_for_age_girls <- admiralpeds::who_bmi_for_age_girls
cdc_bmiage <- admiralpeds::cdc_bmiage

who_bmi_for_age <- who_bmi_for_age_boys %>%
  mutate(SEX = "M") %>%
  bind_rows(who_bmi_for_age_girls %>%
    mutate(SEX = "F")) %>%
  # Keep patients < 2 yrs old
  filter(Day < 730.5) %>%
  rename(AGE = Day) %>%
  # AGEU is added in metadata, required for derive_params_growth_age()
  mutate(AGEU = "DAYS") %>%
  arrange(AGE, SEX)

cdc_bmi_for_age <- cdc_bmiage %>%
  mutate(
    SEX = case_when(
      SEX == 1 ~ "M",
      SEX == 2 ~ "F",
      TRUE ~ NA_character_
    ),
    # Ensure first that Age unit is "DAYS"
    AGE = round(AGE * 30.4375),
    AGEU = "DAYS"
  ) %>%
  # Interpolate the AGE by SEX so that we get CDC metadata by day instead of
  # month in the same way as WHO metadata
  derive_interp_records(
    by_vars = exprs(SEX),
    parameter = "BMI"
  ) %>%
  # Keep patients >= 2 yrs till 20 yrs - Remove duplicates for 730 Days old which
  # must come from WHO metadata only
  filter(AGE >= 730.5 & AGE <= 7305) %>%
  arrange(AGE, SEX)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
who_bmi_for_age <- who_bmi_for_age %>%
  select(AGE, AGEU, SEX, L, M, S)

dataset_vignette(
  who_bmi_for_age,
  filter = AGE < 20
)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
cdc_bmi_for_age <- cdc_bmi_for_age %>%
  select(AGE, AGEU, SEX, L, M, S, P95, Sigma)

dataset_vignette(
  cdc_bmi_for_age,
  filter = AGE < 750
)

## ----eval=TRUE----------------------------------------------------------------
who_wt_for_lgth_boys <- admiralpeds::who_wt_for_lgth_boys
who_wt_for_lgth_girls <- admiralpeds::who_wt_for_lgth_girls

who_wt_for_lgth <- who_wt_for_lgth_boys %>%
  mutate(SEX = "M") %>%
  bind_rows(who_wt_for_lgth_girls %>%
    mutate(SEX = "F")) %>%
  mutate(HEIGHT_LENGTHU = "cm") %>%
  rename(HEIGHT_LENGTH = Length)

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  who_wt_for_lgth,
  filter = HEIGHT_LENGTH == 65
)

## ----echo=FALSE, message=FALSE------------------------------------------------
vs_peds <- pharmaversesdtm::vs_peds
adsl_peds <- admiralpeds::adsl_peds

vs <- convert_blanks_to_na(vs_peds)
adsl <- adsl_peds %>% select(-DOMAIN)

vs <- filter(vs, USUBJID %in% c("01-701-1023"))

param_lookup <- tibble::tribble(
  ~VSTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "WEIGHT", "WEIGHT", "Weight (kg)", 1,
  "HEIGHT", "HEIGHT", "Height (cm)", 2,
  "BMI", "BMI", "Body Mass Index(kg/m^2)", 3,
  "HDCIRC", "HDCIRC", "Head Circumference (cm)", 4,
  NA_character_, "WGTASDS", "Weight-for-age z-score", 5,
  NA_character_, "WGTAPCTL", "Weight-for-age percentile", 6,
  NA_character_, "HGTSDS", "Height-for-age z-score", 7,
  NA_character_, "HGTPCTL", "Height-for-age percentile", 8,
  NA_character_, "BMISDS", "BMI-for-age z-score", 9,
  NA_character_, "BMIPCTL", "BMI-for-age percentile", 10,
  NA_character_, "HDCSDS", "Head Circumference-for-age z-score", 11,
  NA_character_, "HDCPCTL", "Head Circumference-for-age percentile", 12,
  NA_character_, "WGTHSDS", "Weight-for-length/height Z-Score", 13,
  NA_character_, "WGTHPCTL", "Weight-for-length/height Percentile", 14
)
attr(param_lookup$VSTESTCD, "label") <- "Vital Signs Test Short Name"

adsl_vars <- exprs(SEX, BRTHDTC, TRTSDT, TRTEDT, TRT01A, TRT01P)

advs <- vs %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "BRTH",
    dtc = BRTHDTC
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = VSDTC
  ) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

advs <- advs %>%
  derive_vars_merged_lookup(
    dataset_add = param_lookup %>% filter(!is.na(VSTESTCD)),
    new_vars = exprs(PARAMCD),
    by_vars = exprs(VSTESTCD)
  ) %>%
  mutate(AVAL = VSSTRESN)

advs <- advs %>%
  mutate(
    ATPTN = VSTPTNUM,
    ATPT = VSTPT,
    AVISIT = case_when(
      str_detect(VISIT, "UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "SCREENING 1" ~ "-1",
      VISIT == "BASELINE" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", "")),
      TRUE ~ NA_character_
    ))
  )

## ----eval=TRUE----------------------------------------------------------------
# Calculate Current Analysis Age AAGECUR and unit AAGECURU
advs <- advs %>%
  derive_vars_duration(
    new_var = AAGECUR,
    new_var_unit = AAGECURU,
    start_date = BRTHDT,
    end_date = ADT
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
dataset_vignette(
  advs,
  display_vars = exprs(USUBJID, BRTHDT, ADT, AAGECUR, AAGECURU)
)

## ----eval=TRUE----------------------------------------------------------------
# Derive Current HEIGHT/LENGTH at each time point Temporary variable
advs <- advs %>%
  derive_vars_merged(
    dataset_add = advs,
    by_vars = c(get_admiral_option("subject_keys"), exprs(AVISIT)),
    filter_add = PARAMCD == "HEIGHT" & toupper(VSSTRESU) == "CM",
    new_vars = exprs(HGTTMP = AVAL, HGTTMPU = VSSTRESU)
  )

## ----eval=TRUE----------------------------------------------------------------
## Derive Anthropometric indicators (Z-Scores/Percentiles-for-Age) based on Standard Growth Charts
## For BMI by Age
advs <- advs %>%
  slice_derivation(
    derivation = derive_params_growth_age,
    args = params(
      sex = SEX,
      age = AAGECUR,
      age_unit = AAGECURU,
      parameter = VSTESTCD == "BMI",
      analysis_var = AVAL,
      set_values_to_sds = exprs(
        PARAMCD = "BMISDS",
        PARAM = "BMI-for-age z-score"
      ),
      set_values_to_pctl = exprs(
        PARAMCD = "BMIPCTL",
        PARAM = "BMI-for-age percentile"
      )
    ),
    derivation_slice(
      filter = AAGECUR < 730.5,
      args = params(
        who_correction = TRUE,
        meta_criteria = who_bmi_for_age
      )
    ),
    derivation_slice(
      filter = AAGECUR >= 730.5,
      args = params(
        bmi_cdc_correction = TRUE,
        meta_criteria = cdc_bmi_for_age
      )
    )
  )

## ----eval=TRUE, echo=FALSE----------------------------------------------------
advs_display <- advs %>% select(USUBJID, AAGECUR, AAGECURU, PARAMCD, PARAM, AVAL)

dataset_vignette(
  advs_display,
  filter = PARAMCD %in% c("BMISDS", "BMIPCTL")
)

## ----eval=FALSE---------------------------------------------------------------
# advs <- advs %>%
#   restrict_derivation(
#     derivation = derive_params_growth_height,
#     args = params(
#       sex = SEX,
#       height = HGTTMP,
#       height_unit = HGTTMPU,
#       meta_criteria = who_wt_for_lgth,
#       parameter = VSTESTCD == "WEIGHT",
#       analysis_var = AVAL,
#       who_correction = TRUE,
#       set_values_to_sds = exprs(
#         PARAMCD = "WGTHSDS",
#         PARAM = "Weight-for-length/height Z-Score"
#       ),
#       set_values_to_pctl = exprs(
#         PARAMCD = "WGTHPCTL",
#         PARAM = "Weight-for-length/height Percentile"
#       )
#     ),
#     filter = AAGECUR < 730.5
#   )

