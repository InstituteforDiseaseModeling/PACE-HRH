# Ethiopia Health Extension Program Capacity Modelling Package

Sample:

```
# Load the ehep library
library(ehep)

# [OPTIONAL] Turn on tracing if you want ehep to tell you more
# about what it's doing and any problems it encounters
ehep::Trace(TRUE)

# Initial population information using data from an Excel spreadsheet.
# By default, ehep looks for the file "./config/R Model Inputs.xlsx".
# The location of the Excel model inputs file can be controlled through
# a configuration file, config.json. ehep will look for config.json in
# the current working directory.
ehep::InitializePopulation()

# ehep stores configuration information and computed values in its global 
# package environment. You can view the current contents of the global 
# package environment with R's ls.str command.
ls.str(ehep:::globalPackageEnvironment)
```

The following is an example of the data that can be recovered by interrogating
`ehep:::globalPackageEnvironment`

```
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


