# Code based on example at https://vbaliga.github.io/verify-that-r-packages-are-installed-and-loaded/

packages = c(
  "jsonlite",
  "readxl",
  "data.table",
  "assertthat",
  "dplyr",
  "magrittr",
  #------------
  "readr",
  "testthat",
  "tidyr",
  "ggplot2",
  "scales"
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
