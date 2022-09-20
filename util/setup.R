# Based on a code sample at
# https://vbaliga.github.io/posts/2019-04-28-verify-that-r-packages-are-installed-and-loaded/,
# copyright Vikram B. Baliga (vbaliga@zoology.ubc.ca), and provided under the
# Creative Commons CC BY-NC 4.0 license
# (https://creativecommons.org/licenses/by-nc/4.0/).

packages = c(
  "jsonlite",
  "readxl",
  "data.table",
  "assertthat",
  "dplyr",
  "magrittr",
  "ggplot2",
  "scales",
  "methods",
  "truncnorm",
  "tibble",
  "testthat",
  "roxytest",
  "readr",
  "tidyr",
  "installr",
  "withr",
  "testthat",
  "roxytest",
  "knitr",
  "rmarkdown",
  "validate",
  "kableExtra",
  "stringr"
  )

package.check <- lapply(
  packages,
  FUN = function(name) {
    if (!require(name, character.only = TRUE)) {
      install.packages(name, dependencies = TRUE)
      library(name, character.only = TRUE)
    }
  }
)

# Pandoc is required to generate validation reports: https://pandoc.org/installing.html
installr::install.pandoc(to_restart=FALSE)
