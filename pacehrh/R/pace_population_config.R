#' Load Initial Population
#'
#' Read the initial population pyramid from the model inputs Excel file. The
#' name and location of the model inputs Excel file is loaded from the global
#' configuration JSON file.
#'
#' @param sheetName Sheet name from the model input Excel file
#'
#' @return List with three \code{PopulationPyramid} objects:
#' \code{female}, \code{male} and \code{total}
#'
loadInitialPopulation <- function(sheetName = "TotalPop"){
  popData <- readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName)

  assertthat::has_name(popData, "Age")
  assertthat::has_name(popData, "Male")
  assertthat::has_name(popData, "Female")

  # For consistency we use the integer range 0:100 to label age buckets.
  assertthat::assert_that(length(popData$Male) == length(popData$Female))
  assertthat::assert_that(length(popData$Male) == length(popData$Age))
  assertthat::assert_that(length(popData$Age) == length(GPE$ages))

  male <- PopulationPyramid()
  female <- PopulationPyramid()
  total <- PopulationPyramid()

  male <- setFromVector(male, round(popData$Male, 0))
  female <- setFromVector(female, round(popData$Female, 0))
  total <- setFromVector(total,
                         round(popData$Male, 0) + round(popData$Female, 0))

  return(list(age = GPE$ages,
              female = female,
              male = male,
              total = total))
}

.popLabelRawColumns <-
  c("Relevant Population Labels", "Male", "Female", "Starting Age", "Ending Age")

.popLabelColumns <-
  c("Labels", "Male", "Female", "Start", "End")

loadPopulationLabels <- function(sheetName = "Lookup"){
  df <- tryCatch({
      readxl::read_xlsx(GPE$inputExcelFile,
                        sheet = sheetName,
                        .name_repair = "minimal")
  },
  error = function(e){
    return(NULL)
  },
  finally = {
  })

  if (is.null(df)){
    warning("Could not read population labels lookup sheet")
    return(NULL)
  }

  if (!all(.popLabelRawColumns %in% colnames(df))){
    str <- paste0("Invalid columns in population labels lookup sheet\n",
                  "Expected: ", paste0(.popLabelRawColumns, collapse = ", "))
    warning(str)
    return(NULL)
  }

  # When the read fails, BVE$populationLabels stays NULL, which eventually
  # triggers a fatal error when RunExperiments() is called.

  # Filter the columns, and rename
  df <- df[,.popLabelRawColumns]
  names(df) <- .popLabelColumns

  # Remove blank columns
  df <- df[,apply(df,2,function(x){any(!is.na(x))})]
  # Remove blank rows
  df <- df[apply(df,1,function(x){any(!is.na(x))}),]

  dt <- data.table::setDT(df)
  data.table::setkey(dt, Labels)

  return(dt)
}

#' Initialize Population Data
#'
#' Load basic population information into the global package environment,
#' from which it can be used for later processing.
#'
#' @return NULL (invisible)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' pacehrh::InitializePopulation()
#' }
InitializePopulation <- function(){
  .checkAndLoadGlobalConfig()

  BVE$initialPopulation <- NULL
  BVE$populationLabels <- NULL
  BVE$populationRangesTable <- NULL

  BVE$initialPopulation <- loadInitialPopulation()
  BVE$populationLabels <- loadPopulationLabels()
  BVE$populationRangesTable <- .computePopulationRanges(BVE$populationLabels)

  return(invisible(NULL))
}

.computePopulationRanges <- function(lt){
  return(list(Female = .computePopulationRangesBySex(lt, "f"),
              Male = .computePopulationRangesBySex(lt, "m")))
}

.computePopulationRangesBySex <- function(lt, sex = "m"){
  if (is.null(lt)){
    return(NULL)
  }

  if (!(tolower(sex) %in% c("m", "f"))){
    traceMessage(paste0("Invalid sex (", sex, ") provided to .computePopulationRangesBySex()"))
    sex <- "m"
  }

  l <- lapply(1:NROW(lt), function(i){
    v <- numeric(length(GPE$ages))

    if (sex == "m" && (lt$Male[i] == FALSE)){
      return(v)
    }

    if (sex == "f" && (lt$Female[i] == FALSE)){
      return(v)
    }

    start <- lt$Start[i]
    if (is.na(start)){
      start <- GPE$ageMin
    }

    end <- lt$End[i]
    if (is.na(end)){
      end <- GPE$ageMax
    }

    v[(start+1):(end+1)] <- 1

    return(v)
  })

  m <- do.call(rbind, l)

  rownames(m) <- lt[,Labels]
  colnames(m) <- GPE$ages

  return(m)
}
