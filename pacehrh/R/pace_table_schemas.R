# Table metadata consists of a list with the following entries:
#
# rcols : vector of names of required columns
# rtypes : vector of types of required columns
# ocols : [optional] vector of names of optional columns
# otypes : [optional] vector of types of optional columns
# kcols : [optional] vector of names of columns that must have values
# cols : vector of names of ALL allowed columns
# types : vector of types of ALL allowed columns

# ------------------------------------------------------------------------------
#
# SCENARIOS TABLE
#
# ------------------------------------------------------------------------------

.scenarioColumnNames <-
  c(
    "UniqueID",
    "WeeksPerYr",
    "HrsPerWeek",
    "BaselinePop",
    "o_PopGrowth",
    "o_Fertility_decr",
    "o_MHIVTB_decr",
    "o_ChildDis_decr",
    "sheet_TaskValues",
    "sheet_PopValues",
    "sheet_SeasonalityCurves",
    "sheet_Cadre",
    "sheet_Coverage"
  )

.scenarioColumnTypes <-
  c(
    "character",
    "double",
    "double",
    "double",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "character",
    "character",
    "character",
    "character"
  )

.scenarioColumnNamesOptional <-
  c(
    "DeliveryModel"
  )

.scenarioColumnTypesOptional <-
  c(
    "character"
  )

.scenarioMetaData <-
  list(
    rcols = .scenarioColumnNames,
    rtypes = .scenarioColumnTypes,
    ocols = .scenarioColumnNamesOptional,
    otypes = .scenarioColumnTypesOptional,
    cols = c(.scenarioColumnNames, .scenarioColumnNamesOptional),
    types = c(.scenarioColumnTypes, .scenarioColumnTypesOptional)
  )

# ------------------------------------------------------------------------------
#
# SEASONALITY OFFSETS TABLE
#
# ------------------------------------------------------------------------------

.seasonalityOffsetColumnNames <-
  c(
    "Task",
    "Description",
    "Curve",
    "Offset1",
    "Offset2",
    "Offset3",
    "Offset4",
    "Offset5",
    "Offset6"
  )

.seasonalityOffsetColumnTypes <-
  c(
    "character",
    "character",
    "character",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double"
  )

.seasonalityOffsetKeyColumns <-
  c(
    "Task",
    "Curve",
    "Offset1"
  )

.seasonalityOffsetMetaData <-
  list(
    rcols = .seasonalityOffsetColumnNames,
    rtypes = .seasonalityOffsetColumnTypes,
    cols = .seasonalityOffsetColumnNames,
    types = .seasonalityOffsetColumnTypes,
    kcols = .seasonalityOffsetKeyColumns
  )

# ------------------------------------------------------------------------------
#
# POPULATION CHANGE RATES TABLE
#
# ------------------------------------------------------------------------------

.populationChangeRateColNames <-
  c(
    "Description",
    "Label",
    "Type",
    "Sex",
    "BandStart",
    "BandEnd",
    "InitValue",
    "ChangeRate"
  )

.populationChangeRateColTypes <-
  c(
    "character",
    "character",
    "character",
    "character",
    "double",
    "double",
    "double",
    "double"
  )

.populationChangeRateKeyColumns <-
  c(
    "Type",
    "Sex"
  )

.populationChangeRateMetaData <-
  list(
    rcols = .populationChangeRateColNames,
    rtypes = .populationChangeRateColTypes,
    cols = .populationChangeRateColNames,
    types = .populationChangeRateColTypes,
    kcols = .populationChangeRateKeyColumns
  )

# ------------------------------------------------------------------------------
#
# CHANGE RATE LIMITS TABLE
#
# ------------------------------------------------------------------------------

.changeRateLimitsColumnNames <-
  c(
    "RateCategory",
    "Min",
    "Max"
  )

.changeRateLimitsColumnTypes <-
  c(
    "character",
    "double",
    "double"
  )

.changeRateLimitsKeyColumns <-
  c(
    "RateCategory"
  )

.changeRateLimitsMetaData <-
  list(
    rcols = .changeRateLimitsColumnNames,
    rtypes = .changeRateLimitsColumnTypes,
    cols = .changeRateLimitsColumnNames,
    types = .changeRateLimitsColumnTypes,
    kcols = .changeRateLimitsKeyColumns
  )

# ------------------------------------------------------------------------------
#
# TASK VALUES TABLE
#
# ------------------------------------------------------------------------------

.taskValuesColumnNames <-
  c(
    "Indicator",
    "CommonName",
    "ClinicalOrNon",
    "ClinicalCat",
    "ServiceCat",
    "RelevantPop",
    "StartingRateInPop",
    "RateMultiplier",
    "AnnualDeltaRatio",
    "NumContactsPerUnit",
    "NumContactsAnnual",
    "MinsPerContact",
    "HoursPerWeek"
  )

.taskValuesColumnTypes <-
  c(
    "character",
    "character",
    "character",
    "character",
    "character",
    "character",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double"
  )

.taskValuesColumnNamesOptional <-
  c(
    "RateType",
    "MultiplierReason"
  )

.taskValuesColumnTypesOptional <-
  c(
    "character",
    "character"
  )

.taskValuesKeyColumns <-
  c(
    "Indicator"
  )

.taskValuesMetaData <-
  list(
    rcols = .taskValuesColumnNames,
    rtypes = .taskValuesColumnTypes,
    ocols = .taskValuesColumnNamesOptional,
    otypes = .taskValuesColumnTypesOptional,
    cols = c(.taskValuesColumnNames, .taskValuesColumnNamesOptional),
    types = c(.taskValuesColumnTypes, .taskValuesColumnTypesOptional),
    kcols = .taskValuesKeyColumns
  )

# ------------------------------------------------------------------------------
#
# SEASONALITY CURVES TABLE
#
# ------------------------------------------------------------------------------

.seasonlityCurvesColumnNames <-
  c(
    "Month"
  )

.scenarioColumnTypes <-
  c(
    "character"
  )

# ------------------------------------------------------------------------------
#
# STOCHASTIC PARAMETERS TABLE
#
# ------------------------------------------------------------------------------

.stochasticParametersColNames <-
  c(
    "Value",
    "p",
    "q"
  )

.stochasticParametersColTypes <-
  c(
    "character",
    "double",
    "double"
  )

.stochasticParametersKeyCols <-
  c(
    "Value",
    "p"
  )

.stochasticParametersMetaData <-
  list(
    rcols = .stochasticParametersColNames,
    rtypes = .stochasticParametersColTypes,
    cols = .stochasticParametersColNames,
    types = .stochasticParametersColTypes,
    kcols = .stochasticParametersKeyCols
  )

# ------------------------------------------------------------------------------
#
# CADRE ROLES TABLE
#
# ------------------------------------------------------------------------------

.cadreRolesColumnNames <-
  c(
    "ScenarioID",
    "RoleID",
    "RoleDescription",
    "OverheadHoursPerWeek",
    "StartYear",
    "EndYear"
  )

.cadreRolesColumnTypes <-
  c(
    "character",
    "character",
    "character",
    "double",
    "double",
    "double"
  )

.cadreRolesKeyColumns <-
  c(
    "ScenarioID",
    "RoleID"
  )

.cadreRolesMetaData <-
  list(
    rcols = .cadreRolesColumnNames,
    rtypes = .cadreRolesColumnTypes,
    cols = .cadreRolesColumnNames,
    types = .cadreRolesColumnTypes,
    kcols = .cadreRolesKeyColumns
  )

# ------------------------------------------------------------------------------
#
# INITIAL POPULATION TABLE
#
# ------------------------------------------------------------------------------

.populationColumnNames <-
  c(
    "Age",
    "Male",
    "Female"
  )

.populationColumnTypes <-
  c(
    "character",
    "double",
    "double"
  )

.populationKeyColumns <-
  c(
    "Age",
    "Male",
    "Female"
  )

.populationMetaData <-
  list(
    rcols = .populationColumnNames,
    rtypes = .populationColumnTypes,
    cols = .populationColumnNames,
    types = .populationColumnTypes,
    kcols = .populationKeyColumns
  )
