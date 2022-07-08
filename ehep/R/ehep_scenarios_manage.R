.scenarioColumnNames <-
  c(
    "UniqueID",
    "WeeksPerYr",
    "HrsPerWeek",
    "BaselinePop",
    "PopType",
    "o_PopGrowth",
    "o_Seasonality",
    "o_Fertility_decr",
    "o_MHIVTB_decr",
    "o_ChildDis_decr",
    "sheet_TaskValues",
    "sheet_PopValues",
    "sheet_SeasonalityCurves",
    "sheet_Cadre",
    "DeliveryModel",
    "Geography_dontedit"
  )

.scenarioColumnTypes <-
  c(
    "character",
    "double",
    "double",
    "double",
    "character",
    "logical",
    "logical",
    "logical",
    "logical",
    "logical",
    "character",
    "character",
    "character",
    "character",
    "character",
    "character"
  )

.scenarioMetaData <- list(cols = .scenarioColumnNames, types = .scenarioColumnTypes)

#' Create A New Scenarios Table
#'
#' @return Blank scenarios table
#' @export
#' @md
#'
CreateScenariosTable <- function() {
  l <- lapply(1:length(.scenarioColumnNames), function(i) {
    return(vector(mode = .scenarioColumnTypes[i]))
  })

  names(l) <- .scenarioColumnNames

  tbl <- do.call(tibble::tibble, l)

  return(tbl)
}

#' Add A New Scenario To A Scenarios Table
#'
#' @param tbl Scenarios table
#' @param UniqueID Scenario name. Must be unique.
#' @param ... Field values (set Details)
#'
#' @details Allowed fields (in the ... arguments):
#'
#' * __WeeksPerYr__ (double)
#' * __HrsPerWeek__ (double)
#' * __BaselinePop__ (double)
#' * __PopType__ (character)
#' * __o_PopGrowth__ (logical)
#' * __o_Seasonality__ (logical)
#' * __o_Fertility_decr__ (logical)
#' * __o_MHIVTB_decr__ (logical)
#' * __o_ChildDis_decr__ (logical)
#' * __sheet_TaskValues__ (character)
#' * __sheet_PopValues__ (character)
#' * __sheet_SeasonalityCurves__ (character)
#' * __sheet_sheet_Cadre__ (character)
#' * __DeliveryModel__ (character)
#' * __Geography_dontedit__ (character)
#'
#' @return Updated scenarios table
#' @export
#' @md
#'
AddScenario <-
  function(tbl, UniqueID, ...) {
    if (missing(tbl)){
      traceMessage("Missing required tbl parameter in call to AddScenario()")
      return(NULL)
    }

    if (missing(UniqueID)){
      traceMessage("Missing required UniqueID parameter in call to AddScenario()")
      return(tbl)
    }

    if (!.isValidID(UniqueID)){
      traceMessage("Invalid scenario ID in call to AddScenario()")
      return(tbl)
    }

    n <- which(tbl$UniqueID == UniqueID)

    if (length(n) > 0){
      traceMessage(paste("AddScenario(): Scenario ", UniqueID, " already exists", sep = ""))
      return(tbl)
    }

    # TODO: insert type checking (prevent columns being coerced to characters)

    args <- list(`.data` = tbl, UniqueID = UniqueID, ...)

    newtbl <- tryCatch({
      do.call(tibble::add_row, args)
    },
    warning = function(war) {
      traceMessage(paste("WARNING:", war))
    },
    error = function(err) {
      traceMessage(paste("ERROR:", err))
    },
    finally = {

    })

    if (is.null(newtbl)){
      return(tbl)
    } else {
      return(newtbl)
    }
  }

#' Delete A Scenario From A Scenarios Table
#'
#' @param tbl Scenarios table
#' @param UniqueID Scenario name. Must be unique.
#'
#' @return Updated scenarios table
#' @export
#' @md
#'
DeleteScenario <-
  function(tbl, UniqueID) {
    if (missing(tbl)){
      traceMessage("Missing required tbl parameter in call to DeleteScenario()")
      return(NULL)
    }

    if (missing(UniqueID)){
      traceMessage("Missing required UniqueID parameter in call to DeleteScenario()")
      return(tbl)
    }

    if (!.isValidID(UniqueID)){
      traceMessage("Invalid scenario ID in call to DeleteScenario()")
      return(tbl)
    }

    n <- which(tbl$UniqueID == UniqueID)

    if (length(n) == 0){
      return(tbl)
    }

    return(tbl[-n,])
  }

#' Update A Scenario Record
#'
#' @param tbl Scenarios table
#' @param UniqueID Scenario name
#' @param ... Field values (set Details)
#'
#' @details Allowed fields (in the ... arguments):
#'
#' * __WeeksPerYr__ (double)
#' * __HrsPerWeek__ (double)
#' * __BaselinePop__ (double)
#' * __PopType__ (character)
#' * __o_PopGrowth__ (logical)
#' * __o_Seasonality__ (logical)
#' * __o_Fertility_decr__ (logical)
#' * __o_MHIVTB_decr__ (logical)
#' * __o_ChildDis_decr__ (logical)
#' * __sheet_TaskValues__ (character)
#' * __sheet_PopValues__ (character)
#' * __sheet_SeasonalityCurves__ (character)
#' * __sheet_sheet_Cadre__ (character)
#' * __DeliveryModel__ (character)
#' * __Geography_dontedit__ (character)
#'
#' @return Updated scenarios table
#' @export
#' @md
#'
UpdateScenario <-
  function(tbl, UniqueID, ...) {
    if (missing(tbl)){
      traceMessage("Missing required tbl parameter in call to UpdateScenario()")
      return(NULL)
    }

    if (missing(UniqueID)){
      traceMessage("Missing required UniqueID parameter in call to UpdateScenario()")
      return(tbl)
    }

    if (!.isValidID(UniqueID)){
      traceMessage("Invalid scenario ID in call to UpdateScenario()")
      return(tbl)
    }

    rowNum <- which(tbl$UniqueID == UniqueID)

    if (length(rowNum) == 0){
      traceMessage(paste("UpdateScenario(): Scenario ", UniqueID, " doesn't already exist", sep = ""))
      return(tbl)
    }

    # TODO: insert type checking (prevent columns being coerced to characters)

    # It should never happen that there are two records with the same UniqueID
    # value, but in case it does happen we use the first match.
    rowNum <- rowNum[1]

    # Write correctly labeled field values to the appropriate record
    args <- list(...)
    argNames <- names(args)
    cols <- names(tbl)
    for (i in seq_along(args)){
      argName <- argNames[i]
      argValue <- args[[i]]
      if (argName %in% cols){
        tbl[rowNum,argName] <- argValue
      }
    }

    return(tbl)
  }

#' Read A Scenario Record
#'
#' @param tbl Scenarios table
#' @param UniqueID Scenario name
#'
#' @return Scenario record (as a tibble)
#' @export
#' @md
#'
ReadScenario <-
  function(tbl, UniqueID) {
    if (missing(tbl)){
      traceMessage("Missing required tbl parameter in call to ReadScenario()")
      return(NULL)
    }

    if (missing(UniqueID)){
      traceMessage("Missing required UniqueID parameter in call to ReadScenario()")
      return(NULL)
    }

    if (!.isValidID(UniqueID)){
      traceMessage("Invalid scenario ID in call to ReadScenario()")
      return(NULL)
    }

    rowNum <- which(tbl$UniqueID == UniqueID)

    if (length(rowNum) == 0){
      traceMessage(paste("ReadScenario(): Scenario ", UniqueID, " not found", sep = ""))
      return(NULL)
    }

    # TODO: insert type checking (prevent columns being coerced to characters)

    # It should never happen that there are two records with the same UniqueID
    # value, but in case it does happen we use the first match.
    rowNum <- rowNum[1]

    return(tbl[rowNum,])
  }


.isValidID <- function(ID) {
  if (is.null(ID)) {
    return(FALSE)
  }

  if (is.na(ID)) {
    return(FALSE)
  }

  if (typeof(ID) != "character") {
    return(FALSE)
  }

  if (nchar(trimws(ID)) == 0) {
    return(FALSE)
  }

  return(TRUE)
}


