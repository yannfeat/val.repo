# Copyright (C) 2025  Stefan Kraemer
#   
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation as version 3 of the License
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.



#' Simulated data for SCA method.
#'
#' A simulated data containing interaction information of three binding curves all generated with concentration 5e-08,
#'
#' @format A data frame with 453 rows and four variables:
#' \describe{
#'   \item{Time}{time points of the binding interaction from start till the experiment's end}
#'    \item{Sample.A}{sample one with Ka = 1e+7nM, Kd = 1e-2}
#'    \item{Sample.B}{sample two with Ka = 1e+6nM, Kd = 5e-2}
#'    \item{Sample.C}{sample four with Ka = 1e+6nM, Kd = 1e-3}
#' }
#' @source \url{https://apps.cytivalifesciences.com/spr/}
#' @usage data(SCA_dataset)
"SCA_dataset"

#' Simulated data for SCA method with linear drift.
#'
#' A simulated data containing interaction information of three binding curves all generated with concentration 5e-08,
#' baseline drift = -0.019
#' @format A data frame with 453 rows and four variables:
#' \describe{
#'   \item{Time}{time points of the binding interaction from start till the experiment's end}
#'    \item{Sample.A}{sample one with Ka = 1e+7nM, Kd = 1e-2}
#'    \item{Sample.B}{sample two with Ka = 1e+6nM, Kd = 5e-2}
#'    \item{Sample.C}{sample four with Ka = 1e+6nM, Kd = 1e-3}
#' }
#' @source \url{https://apps.cytivalifesciences.com/spr/}
#' @usage data(SCA_dataset)
"SCA_dataset_drift"

#' Simulated data of different binding curves for SCK method.
#'
#' A dataset contains one binding curve with 5 titrations-series (5 injection-series), as follows:
#' tass: 50, 220, 390, 560, 730;
#' tdiss: 150, 320, 490, 660, 830;
#' conc: 6.17e-10 1.85e-09 5.56e-09 1.67e-08 5.00e-08 M
#'
#' @format A data frame with 1091 rows and 6 variables:
#' \describe{
#'   \item{Time}{time points of the binding interaction from start to end}
#'   \item{Sample.A}{sample containing 5 titerations with Ka = 1e+6nM, Kd = 1e-2}
#' }
#' @source \url{https://apps.cytivalifesciences.com/spr/}
#' @usage data(SCK_dataset)
"SCK_dataset"

#' Simulated data of different binding curves for SCK method with exponential decay.
#'
#' A dataset contains one binding curve with 5 titrations-series (5 injection-series), as follows:
#' tass: 50, 220, 390, 560, 730;
#' tdiss: 150, 320, 490, 660, 830;
#' conc: 6.17e-10 1.85e-09 5.56e-09 1.67e-08 5.00e-08 M
#'
#' @format A data frame with 1091 rows and 6 variables:
#' \describe{
#'   \item{Time}{time points of the binding interaction from start to end}
#'   \item{Sample.A}{sample containing 5 titerations with Ka = 1e+6nM, Kd = 1e-2}
#' }
#' @source \url{https://apps.cytivalifesciences.com/spr/}
#' @usage data(SCK_dataset)
"SCK_dataset_decay"

#' Simulated data of binding curve for MCK.
#'
#' A dataset containing 5 different binding curves of different analyte concentrations.
#' Ka = 1e+7nM, Kd = 1e-2
#'
#' @format A data frame with 403 rows and 6 variables:
#' \describe{
#'   \item{Time}{time points of the binding interaction from start to end}
#'   \item{Conc..50.nM.}{binding curve generated with analyte concentration = 50nM}
#'   \item{Conc..16.7.nM.}{binding curve generated with analyte concentration = 16.7nM}
#'   \item{Conc..5.56.nM.}{binding curve generated with analyte concentration = 5.56nM}
#'   \item{Conc..1.85.nM.}{binding curve generated with analyte concentration = 1.85nM}
#'   \item{Conc..6.17e.1.nM.}{binding curve generated with analyte concentration = 0.617nM}
#' }
#' @source \url{https://apps.cytivalifesciences.com/spr/}
#' @usage data(MCK_dataset)
"MCK_dataset"

#' Simulated data of binding curve for MCK with linear drift.
#'
#' A dataset containing 5 different binding curves of different analyte concentrations with induced baseline drift = -0.01.
#' Ka = 1e+7nM, Kd = 1e-2
#'
#' @format A data frame with 403 rows and 6 variables:
#' \describe{
#'   \item{Time}{time points of the binding interaction from start to end}
#'   \item{Conc..50.nM.}{binding curve generated with analyte concentration = 50nM}
#'   \item{Conc..16.7.nM.}{binding curve generated with analyte concentration = 16.7nM}
#'   \item{Conc..5.56.nM.}{binding curve generated with analyte concentration = 5.56nM}
#'   \item{Conc..1.85.nM.}{binding curve generated with analyte concentration = 1.85nM}
#'   \item{Conc..6.17e.1.nM.}{binding curve generated with analyte concentration = 0.617nM}
#' }
#' @source \url{https://apps.cytivalifesciences.com/spr/}
#' @usage data(MCK_dataset)
"MCK_dataset_drift"
