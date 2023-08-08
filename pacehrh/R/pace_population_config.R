#' Load Initial Population
#'
#' Read the initial population pyramid from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return Tibble with three population pyramid fields:
#' \code{Female}, \code{Male} and \code{Total}
#'
#' @noRd
loadInitialPopulation <- function(sheetName = .defaultPopSheet) {
  traceMessage(paste0("Loading initial population sheet ", sheetName))

  popData <- readSheet(sheetName = sheetName)

  if (!is.null(popData)) {
    popData <- validateTableAgainstSchema(popData, .populationMetaData)
  }

  if (is.null(popData)) {
    return(NULL)
  }

  return(.createPopulationTreeDf(GPE$ages, popData$Female, popData$Male))
}

.createPopulationTreeDf <- function(ages = 0:100L,
                                    femalePop = rep_len(0.0, length(ages)),
                                    malePop = rep_len(0.0, length(ages))) {
  if (!.validPopTreeParams(ages, femalePop, malePop)) {
    warning("Invalid parameters passed to .createPopulationTreeDf")
    return(NULL)
  }

  if (GPE$roundingLaw != "none") {
    femalePop <- round(femalePop, 0)
    malePop <- round(malePop, 0)
  }

  return(
    tibble::tibble(
      Age = ages,
      Female = femalePop,
      Male = malePop,
      Total = femalePop + malePop
    )
  )
}

.validPopTreeParams <- function(...) {
  params <- rlang::dots_list(..., .named = TRUE)

  if (any(sapply(params, is.null))) {
    return(FALSE)
  }

  if (any(sapply(params, length) == 0)) {
    return(FALSE)
  }

  reflen <- length(params$ages)

  if (any(sapply(params, length) != reflen)) {
    return(FALSE)
  }

  return(TRUE)
}

.popLabelRawColumns <-
  c("Relevant Population Labels", "Male", "Female", "Starting Age", "Ending Age")

.popLabelColumns <-
  c("Labels", "Male", "Female", "Start", "End")

loadPopulationLabels <- function(sheetName = .defaultPopLabelSheet) {
  traceMessage(paste0("Loading population labels sheet ", sheetName))

  df <- tryCatch(
    {
      readxl::read_xlsx(GPE$inputExcelFile,
        sheet = sheetName,
        .name_repair = "minimal"
      )
    },
    error = function(e) {
      return(NULL)
    },
    finally = {
    }
  )

  if (is.null(df)) {
    warning("Could not read population labels lookup sheet")
    return(NULL)
  }

  if (!all(.popLabelRawColumns %in% colnames(df))) {
    str <- paste0(
      "Invalid columns in population labels lookup sheet\n",
      "Expected: ", paste0(.popLabelRawColumns, collapse = ", ")
    )
    warning(str)
    return(NULL)
  }

  # When the read fails, BVE$populationLabels stays NULL, which eventually
  # triggers a fatal error when RunExperiments() is called.

  # Filter the columns, and rename
  df <- df[, .popLabelRawColumns]
  names(df) <- .popLabelColumns

  # Remove blank columns
  df <- df[, apply(df, 2, function(x) {
    any(!is.na(x))
  })]
  # Remove blank rows
  df <- df[apply(df, 1, function(x) {
    any(!is.na(x))
  }), ]

  # Turn linter off around data.table code

  # nolint start
  dt <- data.table::setDT(df)
  data.table::setkey(dt, Labels)
  # nolint end

  return(dt)
}

#' Initialize Population Data
#'
#' Load basic population information into the global package environment,
#' from which it can be used for later processing.
#'
#' @param popSheet Sheet name from the model input Excel file
#'
#' @return NULL (invisible)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pacehrh::InitializePopulation()
#' }
InitializePopulation <- function(popSheet = .defaultPopSheet) {
  .checkAndLoadGlobalConfig()

  BVE$initialPopulation <- NULL
  BVE$populationLabels <- NULL
  BVE$populationRangesTable <- NULL

  BVE$initialPopulation <- loadInitialPopulation(sheetName = popSheet)
  BVE$populationLabels <- loadPopulationLabels()
  BVE$populationRangesTable <- .computePopulationRanges(BVE$populationLabels)

  return(invisible(NULL))
}

.computePopulationRanges <- function(lt) {
  return(list(
    Female = .computePopulationRangesBySex(lt, "f"),
    Male = .computePopulationRangesBySex(lt, "m")
  ))
}

.computePopulationRangesBySex <- function(lt, sex = "m") {
  if (is.null(lt)) {
    return(NULL)
  }

  if (!(tolower(sex) %in% c("m", "f"))) {
    traceMessage(paste0("Invalid sex (", sex, ") provided to .computePopulationRangesBySex()"))
    sex <- "m"
  }

  #   l <- lapply(1:NROW(lt), function(i) {
  l <- lapply(seq_len(NROW(lt)), function(i) {
    v <- numeric(length(GPE$ages))

    if (sex == "m" && (lt$Male[i] == FALSE)) {
      return(v)
    }

    if (sex == "f" && (lt$Female[i] == FALSE)) {
      return(v)
    }

    start <- lt$Start[i]
    if (is.na(start)) {
      start <- GPE$ageMin
    }

    end <- lt$End[i]
    if (is.na(end)) {
      end <- GPE$ageMax
    }

    v[(start + 1):(end + 1)] <- 1

    return(v)
  })

  m <- do.call(rbind, l)

  # Turn linter off for data.table code

  # nolint start
  rownames(m) <- lt[, Labels]
  # nolint end
  colnames(m) <- GPE$ages

  return(m)
}
