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

#### Using styler and lintr to get code style right

The R packages [https://styler.r-lib.org/](styler) and [https://lintr.r-lib.org/](lintr) are the core tools for managing code style.

The styler package reformats code according to whatever style rules have been set up.

The lintr package checks for code style problems. 

Linting configuration is controlled by the .lint file in the package root directory. lintr's default configuration is based on the tidyverse style, so the pacehrh .lint file invokes the default configuration with the exceptions noted previously.

The following __lintr__ command line checks for correctly formatted code:

With the .lintr file in place, the command to lint a file is:

```
lint("<r-source-code-file-path>")
```

For example:

```
lint("R/pace_rates_matrix.R")
```

The long form of the lint() command duplicating the behavior of the .lint file is:

```
lint(
  "<r-source-code-file-path>",
  linters = linters_with_defaults(
    line_length_linter = line_length_linter(length = 120L),
    object_name_linter = object_name_linter(styles = c("camelCase", "CamelCase", "symbols"))
  )
)
```

Like __styler__, the RStudio _Code/Reformat Code_ command also modifies code to bring it close to the tidyverse standard. Using __styler__ has the advantage that both __styler__ and __lintr__ install commands into the RStudio _Addins_ list.

<img src="rstudio-addins-menu.png" width="200" height="300" />

#### Disabling linting

There are some annoying glitches with linting. A common one is the "no visible binding for variable 'variableName'" message that pops up when ggplot and data.table are used in "normal" ways. The underlying cause is that ggplot and data.table take advantage of R's meta-language features to implement simple and expressive function call syntax, but linters can't know that ggplot and data.table know what to do, and raise a warning. The best approach here is to disable linting for specific blocks of code. (Turning off the object_usage_linter component is a bad idea because several other useful checks would be turned off.)

__lintr__ supports code comment macros to disable linting for specific blocks of code. In this example, 

```
  # nolint start

  tokens <- data.table::tstrsplit(dt[, Category], tokenSeparator)
  dt[, Year := as.numeric(tokens[[1]])]
  dt[, CadreMember := tokens[[2]]]

  # Remove unnecessary columns
  dt <- dt[CadreMember != "Total"]
  dt <- dt[CadreMember != "Unassigned"]

  # Cast to the desired final format
  dt <-
    data.table::dcast(
      dt,
      Indicator + Year ~ CadreMember,
      value.var = "allocation",
      fill = 0
    )

  # nolint end

```
