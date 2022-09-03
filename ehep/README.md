# The PACE-HRH Package

This README describes the features and functions of the PACE-HRH package.

## Global Configuration

Global configuration refers to system-wide variables set up
when the package is started. The most important of these is the location of the 
__input data spreadsheet__ - the Excel spreadsheet that defines experiment scenarios, 
baseline population data, clinical task descriptions, health-worker cadres, etc. 

Global configuration variables are declared
in a file called _globalconfig.json_. This
file is read from the working directory (as reported by `getwd()`). 

Once global configuration is completed, the variable 
GPE$globalConfigLoaded is set to TRUE.

```
> ehep:::GPE$globalConfigLoaded
[1] TRUE
```

### Default Global Configuration File

```
{
  "configDirectoryLocation" : "./config",
  "inputExcelFile" : "R Model Inputs.xlsx",
  "suiteRngSeed" : 12345
}
```

Global configuration can be set programmatically with the `setGlobalConfig()`
function. 

## Terminology


- _Trial_: TBD
- _Experiment_: synonym for _trial_.
- _Suite_: A set of stochastic trials based on a scenario.
- _Scenario_: TBD

## Environments

The PACE-HRH package uses three R environments.

__ehep:::globalPackageEnvironment__ (alias ehep:::GPE) stores configuration 
information for the entire package.

__ehep:::baseValuesEnvironment__ (alias ehep:::BVE) stores the base values 
for stochastic trials.

__ehep:::experimentValuesEnvironment__ (alias ehep:::EXP) stores the actual values, 
after applying stochastic variation, used in each trial.

At the start of a suite of trials (`RunExperiments()`), 
baseline information is loaded into the BVE environment. For each trial in the suite, 
stochastic variations are applied to values in the 
BVE, and the new values are saved in the EXP environment. The trial is then run 
based on the values in the EXP environment.
 
You can view the current contents of any of the environments with R's
`ls.str()` command.





# IGNORE BELOW HERE - OLD CONTENT




The InitializeXXXX() functions depend on knowing where to find the input Excel
file, so each point to a function .checkAndLoadGlobalConfig() to confirm that
global configuration has been set. A useful side-effect is that the
InitializeXXXX() functions can be called in any order.


## Installation

### Load the ehep package binary zip file

1. In RStudio, from the __Tools__ menu, select __Install Packages ...__.
2. Under __Install from:__, select __Package Archive File (.zip, .tar.gz)__.
3. Under __Package archive__, select the version of the ehep package binary 
   zip file you want to install. 
4. Select the installation location and then click __Install__. 
5. If you suspect you're missing a dependent package, from the __File__ menu 
   select __Open File..__. From 
   the repository, select _ehep/util/setup.R_.
6. Click __Run__ to install all dependencies.

### Point ehep to the R model input spreadsheet

___You need to do this before you try to run anything.___

By default, ehep looks for a model input file called _R Model Inputs.xlsx_ 
in a subdirectory of your R working directory called _config_.

If you want to override those default input values, you can point to a
different model input Excel file. To do this, copy the
sample _globalconfig.json_ file in the _util_ directory to your R working
directory, editing the file to set both the location and name of the model
input Excel file. For more information on the RStudio working directory,
see [RStudio support](https://support.rstudio.com/hc/en-us/articles/200711843-Working-Directories-and-Workspaces-in-the-RStudio-IDE).

## Sample

```
# Load the ehep library
library(ehep)

# [OPTIONAL] Turn on tracing if you want ehep to tell you more
# about what it's doing and any problems it encounters

# ehep::Trace(TRUE)

# Initialize EHEP
ehep::InitializePopulation()
ehep::InitializeHealthcareTasks()
ehep::InitializeScenarios()
ehep::InitializeStochasticParameters()
ehep::InitializeSeasonality()

# Set the name of a scenario to run. The scenario must be in the list defined
# in the model input spreadsheet
scenario <- "ScenarioA"
num_trials <- 5
run_number <- 1

# Run the scenario
results <-
  ehep::RunExperiments(scenarioName = scenario,
                       trials = num_trials,
                       debug = FALSE)

# Save the results to a CSV file
ehep::SaveSuiteResults(results, "results.csv", scenario, run_number)
```
## Global parameters

_DO NOT USE THIS FUNCTION_
_Two problems. (1) Insufficiently tested. (2) Changing the start year without
changing the starting population pyramid will create inconsistent results._

The __SetGlobalStartEndYears()__ function can be used to set the start and end 
years for simulation.

```
ehep::SetGlobalStartEndYears(2025, 2030)
```

## Population pyramids

The `ComputeDemographicsProjection()` function uses an initial population 
pyramid, fertility rates, and mortality rates - all loaded through a call to 
`InitializePopulation()` to build the following data structure:

```
> str(demographics)
List of 21
 $ 2020:'data.frame':	101 obs. of  3 variables:
  ..$ Range : chr [1:101] "<1" "1" "2" "3" ...
  ..$ Female: num [1:101] 1713885 1684060 1654236 1624411 1594586 ...
  ..$ Male  : num [1:101] 1768045 1736053 1704061 1672069 1640077 ...
 $ 2021:'data.frame':	101 obs. of  3 variables:
  ..$ Range : chr [1:101] "<1" "1" "2" "3" ...
  ..$ Female: num [1:101] 1588622 1653639 1678479 1648754 1619027 ...
  ..$ Male  : num [1:101] 1588622 1705895 1730299 1698413 1666527 ...
 $ 2022:'data.frame':	101 obs. of  3 variables:
  ..$ Range : chr [1:101] "<1" "1" "2" "3" ...
  ..$ Female: num [1:101] 1600107 1534842 1648406 1673168 1643537 ...
  ..$ Male  : num [1:101] 1600107 1534842 1700497 1724824 1693039 ...
  
...

 $ 2039:'data.frame':	101 obs. of  3 variables:
  ..$ Range : chr [1:101] "<1" "1" "2" "3" ...
  ..$ Female: num [1:101] 1621472 1600858 1604485 1607340 1609403 ...
  ..$ Male  : num [1:101] 1621472 1600858 1604485 1607340 1609403 ...
 $ 2040:'data.frame':	101 obs. of  3 variables:
  ..$ Range : chr [1:101] "<1" "1" "2" "3" ...
  ..$ Female: num [1:101] 1610656 1593593 1598655 1602277 1605128 ...
  ..$ Male  : num [1:101] 1610656 1593593 1598655 1602277 1605128 ...
```
Each entry in the top-level list is a DataFrame of population data for
females and males, stratified by age in years.

