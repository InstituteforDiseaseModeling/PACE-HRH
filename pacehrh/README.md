# The PACE-HRH Package

This README describes the features and functions of the PACE-HRH package.

## Sample

```
# Load the pacehrh library
library(pacehrh)

# [OPTIONAL] Turn on tracing if you want the package to tell you more
# about what it's doing and any problems it encounters

# pacehrh::Trace(TRUE)

# Initialize the package
pacehrh::InitializePopulation()
pacehrh::InitializeScenarios()
pacehrh::InitializeStochasticParameters()
pacehrh::InitializeSeasonality()

pacehrh::SetGlobalStartEndYears(2020, 2040)

# Set the name of a scenario to run. The scenario must be in the list defined
# in the input data spreadsheet.
scenario <- "ScenarioA"
num_trials <- 10
run_id <- 1

# Run the scenario
results <-
  pacehrh::RunExperiments(scenarioName = scenario,
                       trials = num_trials)

# Save the results to a CSV file
pacehrh::SaveSuiteResults(results, "results.csv", scenario, run_number)
```

## Global configuration

The term global configuration refers to system-wide variables set up
when the PACE-HRH package is started. The most important of these is the location of the 
__input data spreadsheet__ - the Excel spreadsheet that defines experiment scenarios, 
baseline population data, clinical task descriptions, health-worker cadres, etc. 

The file _globalconfig.json_ contains declarations for global configuration 
variables. The package reads _globalconfig.json_ from the R working directory (`getwd()`)
when the package is initialized using any of the `Initialize___` functions.

The package sets the variable GPE$globalConfigLoaded to TRUE once _globalconfig.json_
has been read.

```
> pacehrh:::GPE$globalConfigLoaded
[1] TRUE
```

### The default global configuration file

If the package can't find a _globalconfig.json_ it will use the following
default values.

```
{
  "configDirectoryLocation" : "./config",
  "inputExcelFile" : "model_inputs.xlsx",
  "suiteRngSeed" : 12345,
  "startYear" : 2020,
  "endYear" : 2040
}
```

If you want to override these defaults by writing your own _globalconfig.json_ file,
we've provided a sample in the _util_ directory of the GitHub repository.

### Global configuration variables

- _configDirectoryLocation_: The location of the directory where configuration
data is to be found. The default is a _config_ sub-directory of the working directory.
- _inputExcelFile_: The name of the input data spreadsheet file. The default value
is "model_inputs.xlsx". The default setup directs the package to look for the
input data spreadsheet file at "config/model_inputs.xlsx".
- _suiteRngSeed_: The PACE-HRH package makes extensive use of random number
generation. Before each suite of stochastic trials, the system calls the 
R function `set.seed()` with a random number seed value. The system uses
a seed value provided by the user in the `seed =` parameter of the `RunExperiments()`
function, or this global configuration value.
- _startYear_: Starting year for simulations. Default = 2020.
- _endYear_: Ending year for simulations. Default = 2040. Note: Users can also
use the `SetGlobalStartEndYears()` function to set start and end years.

### Setting global configuration variables programmatically

Users can set the global configuration programmatically with the `setGlobalConfig()`
function. 

## Terminology

- _Trial_: A single prediction of healthcare task impacts. PACE-HRH 
performs several stochasically varied trials to measure potential
variation in the predictions.
- _Experiment_: synonym for _trial_.
- _Suite_: A set of stochastic trials based on a scenario.
- _Scenario_: A set of configuration parameters for a suite. Scenarios are defined by entries
in the _Scenario_ sheet of the input data spreadsheet.

## Environments

The PACE-HRH package uses three R environments.

__pacehrh:::globalPackageEnvironment__ (alias pacehrh:::GPE) stores configuration 
information for the entire package.

__pacehrh:::baseValuesEnvironment__ (alias pacehrh:::BVE) stores the base values 
for stochastic trials.

__pacehrh:::experimentValuesEnvironment__ (alias pacehrh:::EXP) stores the actual values, 
after applying stochastic variation, used in each trial.

At the start of a suite of trials (`RunExperiments()`), 
baseline information is loaded into the BVE environment. For each trial in the suite, 
stochastic variations are applied to values in the 
BVE, and the new values are saved in the EXP environment. The trial is then run 
based on the values in the EXP environment.
 
You can view the current contents of any of the environments with R's
`ls.str()` command.

## Links

For more information on the RStudio working directory,
see [RStudio support](https://support.rstudio.com/hc/en-us/articles/200711843-Working-Directories-and-Workspaces-in-the-RStudio-IDE).

