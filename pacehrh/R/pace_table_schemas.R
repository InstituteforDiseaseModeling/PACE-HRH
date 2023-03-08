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
    "sheet_Cadre"
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

.populationChangeRateColumnNames <-
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

.populationChangeRateColumnTypes <-
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

.populationChangeRateColumnMetaData <-
  list(
    rcols = .populationChangeRateColumnNames,
    rtypes = .populationChangeRateColumnTypes,
    cols = .populationChangeRateColumnNames,
    types = .populationChangeRateColumnTypes,
    kcols = .populationChangeRateKeyColumns
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

# .scenarioColumnNamesOptional <-
#   c(
#     "DeliveryModel"
#   )
# 
# .scenarioColumnTypesOptional <-
#   c(
#     "character"
#   )
# 
# .scenarioMetaData <-
#   list(
#     rcols = .scenarioColumnNames,
#     rtypes = .scenarioColumnTypes,
#     ocols = .scenarioColumnNamesOptional,
#     otypes = .scenarioColumnTypesOptional,
#     cols = c(.scenarioColumnNames, .scenarioColumnNamesOptional),
#     types = c(.scenarioColumnTypes, .scenarioColumnTypesOptional)
#   )

# ------------------------------------------------------------------------------
#
# STOCHASTIC PARAMETERS TABLE
#
# ------------------------------------------------------------------------------

.stochasticParametersColumnNames <-
  c(
    "Value",
    "p",
    "q"
  )

.stochasticParametersColumnTypes <-
  c(
    "character",
    "double",
    "double"
  )

.stochasticParametersKeyColumns <-
  c(
    "Value",
    "p"
  )

.stochasticParametersMetaData <-
  list(
    rcols = .stochasticParametersColumnNames,
    rtypes = .stochasticParametersColumnTypes,
    cols = .stochasticParametersColumnNames,
    types = .stochasticParametersColumnTypes,
    kcols = .stochasticParametersKeyColumns
  )
