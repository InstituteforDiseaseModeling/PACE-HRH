# PACE-HRH - Population-Aware Capacity Estimator for Human Resources for Health

The PACE-HRH model simulates the healthcare needs of a given population so you
can more effectively plan what healthcare worker resources will be necessary
to provide a wide range of healthcare services. The model is configured via Excel
sheets and run via an R script. 

## Installation

1.  Clone the repository.
2.  In RStudio, open the "Run simulations.R" script. 
3.  From the **Tools** menu, select **Install Packages ...**.
4.  Under **Install from:**, select the most recent version of the PACE-HRH package binary zip file.
5.  Select the installation location and then click **Install**. 
6.  You may see a banner at the top of the screen that indicates some dependent packages that are 
    required. Select **Yes** to enable installation.

## Documentation

Complete documentation, including R code reference and getting started guidance, is available at

https://institutefordiseasemodeling.github.io/PACE-HRH/articles/pacehrh.html

## Contributing Guidelines

If you're interested in contributing to this repository, please refer to the guidelines at 

https://github.com/InstituteforDiseaseModeling/PACE-HRH/blob/main/.github/CONTRIBUTING.md

## Package Dependencies

PACE-HRH depends on the following user-installed R packages: jsonlite, readxl, data.table, assertthat, dplyr, magrittr, ggplot2, scales, methods, truncnorm, tibble, testthat, roxytest, readr, tidyr, installr, withr, knitr, rmarkdown, validate, kableExtra, stringr, pandoc. The script util\setup.R can be used to install these packages.

## Disclaimer
The code in this repository was developed by IDM to support our research into healthcare system capacity. We’ve made it publicly available under the MIT License to provide others with a better understanding of our research and an opportunity to build upon it for their own work. We make no representations that the code works as intended or that we will provide support, address issues that are found, or accept pull requests. You are welcome to create your own fork and modify the code to suit your own modeling needs as contemplated under the MIT License.

