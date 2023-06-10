# Input Validation

Author: [Meikang Wu](mailto:meikang.wu@gatesfoundation.org)

## Introduction

Validating input data is important before running the simulation; Input data should comply with the required format, range, or constraints, incorrect or inconsistent data can cause processing errors, which may or may not be handled by the PACEHRH app. We have created basic validation suites based on the package [Validate](https://cran.r-project.org/web/packages/validate/index.html), and it can be extended by adding custom checks as needed.

## Implementation Details

### Usage

The validation report template can be rendered by running the code below, it will use `config/model_inputs.xlsx` as input, generate `validation_report.html` in the working directory and put all intermediate files in the log folder:

```         
rmarkdown::render(
          input = "config/validation/validation_report.Rmd",
          output_format = "html_document",
          output_dir = getwd(),
          params=list(inputFile="config/model_inputs.xlsx", outputDir="log")
)
                  
```

### Validation Rules

Currently, all the sheet-specific validation rules are in the rules folder. The name of the rule has postfix that matches the sheet name in the input spreadsheet. For example, [rules_TotalPop.yaml](./rules/rules_TotalPop.yaml) will be used to check the `TotalPop` sheet. Rules are written in yaml formats and must have an expression defined for each rule. see [metadata documentaion](https://cran.r-project.org/web/packages/validate/vignettes/cookbook.html#82_Metadata_in_text_files:_YAML) for more details. The Rules we have implemented for the default PACEHRH model is described [here](https://institutefordiseasemodeling.github.io/PACE-HRH/articles/input-validation.html).

### Custom Checks

If the Rules are more complex and involve more than two sheets, you can add custom functions (defined in a R script with prefix `check_`). The function must take exactly one parameter `inputFile` and return three things in a list: Description, Severity and Violation data (tabular format). For example: in [check_seasonality.R](./check_seasonality.R), we want to check that tasks listed in seasonality offset sheet actually appear in the TaskValues sheet. This will require a join operation between both sheets. so we add a custom function to find the rows that violating this assumption. We need to also add a clear description about the nature of this violation, and set the severity to either: error, warning or info, based on what the consequences can result from this violation.

Alternatively, you can also call a function `.custom_check_write_result` (defined in [ValidateInput.R](./ValidateInput.R)) yourself in the custom function. This will write your violation results to the report. For example: [check_cadre_allocation.R](./check_cadre_allocation.R) shows a complicated check which includes different violations based on various assumptions. If you don't want the function to be executed as checks, use `.` as prefix in the function name, this is a convention to signal that the function is private or internal.

### Custom Plots

Sometimes, a picture is worth more than a thousand words, plotting the raw data such as seasonality can show us if the input data is as expected. You can add plotting functions (defined in a R script with prefix `plot_`). For example: [plot_seasonality.R](./plot_seasonality.R). The function must take exactly two parameters: `inputFile` and `custom_dir`, it should generate a plot from `inputFile` and save it in `custom_dir`. All the images saved in the `custom_dir` will be added to the reports for reviewing.

### Issues

If you find any issues in running or adding validation functions, please log an issue [here](https://github.com/InstituteforDiseaseModeling/PACE-HRH/issues) with details
