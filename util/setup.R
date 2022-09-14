# Code based on example at https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/

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
  #------------
  "readr",
  "tidyr",
  "installr"
  )

package.check <- lapply(
  packages,
  FUN = function(name) {
    if (!require(name, character.only = TRUE)) {
      install.packages(name, dependencies = TRUE)2
      
      library(name, character.only = TRUE)
    }
  }
)

# Pandoc is required to generate validation reports: https://pandoc.org/installing.html
installr::install.pandoc(to_restart=FALSE)