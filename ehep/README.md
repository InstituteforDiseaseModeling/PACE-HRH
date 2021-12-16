# Ethiopia Health Extension Program Capacity Modelling Package

## Installation

### Load the ehep package binary zip file

If you're using RStudio, select __Install Packages ...__ from the __Tools__ menu. 
Keeping the _Install dependencies_ option
clicked should cause RStudio to download any other packages ehep requires.

If you suspect you're missing a dependent package, run the _setup.R_ script 
found in the _util_ subdirectory of the repository.

### Point ehep to the R model input spreadsheet

___You need to do this before you try to run anything.___

By default, ehep looks for a model input file called _R Model Inputs.xlsx_ in a
subdirectory of your R working directory called _config_.

You can overwrite these assumptions by putting a _globalconfig.json_ file in
your R working directory. There's a sample _globalconfig.json file_ in the
repository's _util_ directory. You can edit it to set both the location and 
name of the model input Excel file.

## Sample

```
# Load the ehep library
library(ehep)

# [OPTIONAL] Turn on tracing if you want ehep to tell you more
# about what it's doing and any problems it encounters

ehep::Trace(TRUE)

# Initialize population information using data from an Excel spreadsheet.
# By default, ehep looks for the file "./config/R Model Inputs.xlsx".
# The location of the Excel model inputs file can be controlled through
# a configuration file, globalconfig.json. ehep will look for globalconfig.json 
# in the current working directory.

ehep::InitializePopulation()

# Use the population information loaded by InitializePopulation() to compute
# a family of population pyramids, one for each year of the study range.
# Note: ehep::ComputeDemographicsProjection(debug = TRUE) will return fertility
# and mortality rate information along with the population pyramids.

demographics <- ehep::ComputeDemographicsProjection()

# Load and pre-process information about healthcare tasks.

tasks <- ehep::InitializeHealthcareTasks()

```

## The Global Package Environment

ehep stores configuration information and computed values in its global package 
environment. You can view the current contents of the global package 
environment with R's `ls.str()` command.

The following is an example of the data that can be recovered by interrogating
`ehep:::globalPackageEnvironment`. 

```
> ls.str(ehep:::globalPackageEnvironment)
age_max :  num 100
age_min :  num 0
endYear :  num 2040
fertilityRates : Classes ‘data.table’ and 'data.frame':	21 obs. of  5 variables:
 $ Year                : int  2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 ...
 $ AnnualBirthRate15_19: num  0.0617 0.0601 0.0585 0.0569 0.0554 ...
 $ AnnualBirthRate20_29: num  0.125 0.122 0.119 0.116 0.113 ...
 $ AnnualBirthRate30_39: num  0.125 0.122 0.119 0.116 0.113 ...
 $ AnnualBirthRate40_49: num  0.125 0.122 0.119 0.116 0.113 ...
globalConfigLoaded :  logi TRUE
initialPopulation : tibble [101 x 4] (S3: tbl_df/tbl/data.frame)
inputExcelFile :  chr "./config/R Model Inputs.xlsx"
mortalityRates : Classes ‘data.table’ and 'data.frame':	21 obs. of  9 variables:
 $ Year            : int  2020 2021 2022 2023 2024 2025 2026 2027 2028 2029 ...
 $ MortalityInfants: num  35.2 33.9 32.6 31.4 30.2 ...
 $ Mortality1.4    : num  3.31 3.16 3.02 2.88 2.75 ...
 $ Mortality5.9    : num  1.123 1.069 1.017 0.968 0.921 ...
 $ Mortality10.14  : num  0.923 0.888 0.853 0.82 0.789 ...
 $ Mortality15.19  : num  1.54 1.49 1.44 1.39 1.34 ...
 $ Mortality20.24  : num  1.7 1.68 1.66 1.64 1.62 ...
 $ MortalityAdultF : num  3.93 3.84 3.75 3.66 3.57 ...
 $ MortalityAdultM : num  5.09 5 4.91 4.82 4.73 ...
populationChangeParameters : List of 2
 $ initValues : Named num [1:15] 4.0485 30 0.1247 0.0617 0.1247 ...
 $ changeRates: Named num [1:15] 0.976 30 0.976 0.973 0.976 ...
ratio_females_at_birth :  num 0.5
ratio_males_at_birth :  num 0.5
startYear :  num 2020
traceState :  logi TRUE
years :  num [1:21] 2020 2021 2022 2023 2024 ...
```

## Population Pyramids

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
Each entry in the top-level list is a dataframe of population data for
females and males, stratified by age in years.

## globalconfig.json

The file `globalconfig.json` defines certain global setting for ehep.
Importantly, this includes the location of the model input Excel file.

```
{
  "configDirectoryLocation" : "./config",
  "inputExcelFile" : "R Model Inputs.xlsx"
}
```

