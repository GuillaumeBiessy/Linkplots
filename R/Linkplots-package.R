#' Linkplots: LinkPact styled plots.
#'
#' Predefined plot styles for common type of plots.

#' @import ggplot2
#' @import ggridges
#' @import tidyr
#' @import dplyr
#' @import purrr
#' @importFrom rlang .data
#'
#' @docType package
#' @name Linkplots-package
NULL

#' Immobilier
#'
#' Dataset from the Kaggle Competition Website
#'
#' @format Many Variables Regarding Real Estates Transactions
"immo"

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## and data.table
# if (getRversion() >= "4.1")  {
#   gv <- c()
#   utils::globalVariables(gv)
# }

# library(devtools)

# License
# use_gpl3_license()

# # Packages
# use_package("ggplot2")
# use_package("grDevices")
# use_package("lubridate")
# use_package("dplyr")
# use_package("ggridges")
# use_package("tidyr")
# use_package("purrr")
# use_package("rlang")
# use_package("scales")

# # Tests----
# use_test("theme")
# use_test("plots")

# # Data----
# use_data_raw("immo")

# # Patchnotes----
# use_news_md()
