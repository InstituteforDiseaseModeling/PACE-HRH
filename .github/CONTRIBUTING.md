# Contributing to the PACE-HRH Project

## Code Style

NOTE: As of July 2023 PACE-HRH is in the process of cleaning up code to meet these style guidelines.

PACE-HRH follows [https://style.tidyverse.org/](the tidyverse style guide), with the following exceptions:

#### Names of variables, objects, functions, and function parameters

PACE-HRH uses variations on camel case (e.g. variableName or ObjectName) for names. PACE-HRH code does not use snake case (variable_name) or dotted case (variable.name)

* Public functions and objects (public names that would appear in the package reference and be visible to the pacehrh:: operator) use camel case with a leading upper case letter. Example: CheckInputExcelFileFormat()

* Internal functions use camel case with a leading lower case letter. Example: loadTable()

* Function parameters use camel case with a leading lower case letter. Example: .getMinMaxRates <- function(initRates, limits) { ... }

* Internal functions intended to be scoped to one source file may have a leading period: Example: .getRatesLimits() (R doesn't actually support private scoping visibility, but we follow this convention to give a hint to a function's intended scope. These "private" functions will still be visible to anybody using the pacehrh::: operator.)

#### Line lengths

PACE-HRH style allows for 120-character lines (instead of the tidverse default of 80).

#### return() statements

The tidyverse style guide recommends avoiding return() statements at the end of functions and instead relying on R's native behavior of returning the value of the last evaluated expression. We prefer using return() even at the end of functions because it makes the behavior of the code more explicit, especially for less experienced readers of the code. 

#### Using lintr::lint() to check code style

The following __lintr__ command line checks for correctly formatted code:

```
lint(
  "<r-source-code-file-path>",
  linters = linters_with_defaults(
    line_length_linter = line_length_linter(length = 120L),
    object_name_linter = object_name_linter(styles = c("camelCase", "CamelCase", "symbols"))
  )
)
```
Example:

```
lint(
  "R/pace_rates_matrix.R",
  linters = linters_with_defaults(
    line_length_linter = line_length_linter(length = 120L),
    object_name_linter = object_name_linter(styles = c("camelCase", "CamelCase", "symbols"))
  )
)
```

Hint: The RStudio _Code/Reformat Code_ command produces formatted code that is close to the tidyverse standard. Run _Code/Reformat Code_ to get your code style into the right ballpark before using the lint() function to find the detailed problem areas.



<<< WIP >>>
