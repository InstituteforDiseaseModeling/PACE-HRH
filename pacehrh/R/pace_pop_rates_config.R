.computeFullPopRates <- function(p){
  assertthat::assert_that(nrow(p) > 0)

  x <- rep_len(0.0, length(GPE$ages))
  y <- rep_len(1.0, length(GPE$ages))

  p$BandStart[is.na(p$BandStart)] <- GPE$ageMin
  p$BandEnd[is.na(p$BandEnd)] <- GPE$ageMax
  p$InitValue[is.na(p$InitValue)] <- 0.0
  p$ChangeRate[is.na(p$ChangeRate)] <- 1.0

  p <- p[order(p$BandStart),]

  for (i in seq_along(p$BandStart)){
    start <- p$BandStart[i] + 1
    end <- p$BandEnd[i] + 1
    x[start:end] <- p$InitValue[i]
    y[start:end] <- p$ChangeRate[i]
  }

  return(list(initValues = x, changeRates = y))
}

.computeBandedPopulationRates <- function(p){
  assertthat::assert_that(nrow(p) > 0)

  p <- p[order(p$BandEnd),]
  breaks <- p$BandEnd[1:nrow(p) - 1]

  return(
    list(
      breaks = breaks,
      expansionMatrix = .generateExpansionMatrix(GPE$ages, breaks),
      initValues = p$InitValue,
      changeRates = p$ChangeRate,
      labels = p$Label
    )
  )
}

# TODO: instead of hard-coding this, make it a defaulted parameter to
# loadPopulationChangeRates(). Then the user could, if they wanted, implement
# many more kinds of population strata.

.initPopulationChangeRates <- function(){
  return(list(
    femaleFertility = list(type = "Fertility", sex = "F"),
    maleFertility = list(type = "Fertility", sex = "M"),
    femaleMortality = list(type = "Mortality", sex = "F"),
    maleMortality = list(type = "Mortality", sex = "M")
  ))
}

# The full population rates table comprises separate tables for females and
# males, fertility and mortality. This function splits up the full table
# into the four sub-tables, and does some data tidying and data sanity-
# checking along the way.

.splitPopulationRatesTable <- function(prt){
  # prt = 'population rates table'

  populationChangeRates <- .initPopulationChangeRates()

  l <- lapply(populationChangeRates, function(x){
    p <- prt[prt$Type == x$type & prt$Sex == x$sex,]

    p$BandStart[is.na(p$BandStart)] <- GPE$ageMin
    p$BandEnd[is.na(p$BandEnd)] <- GPE$ageMax
    p$InitValue[is.na(p$InitValue)] <- 0.0
    p$ChangeRate[is.na(p$ChangeRate)] <- 1.0

    if (.checkPopRates(p, x$type, x$sex) == TRUE){
      x$prt <- p
    } else {
      x$prt <- NULL
    }

    return(x)
  })

  return(l)
}

loadPopulationChangeRates <- function(sheetName = .defaultPopulationRatesSheet){
  traceMessage(paste0("Loading population change rates sheet ", sheetName))
  
  popValues <- readxl::read_xlsx(GPE$inputExcelFile, sheet = sheetName)
  popValues <- validateTableAgainstSchema(popValues, .populationChangeRateColumnMetaData)
  
  if (is.null(popValues)){
    return(NULL)
  }
  
  popValues <- .splitPopulationRatesTable(popValues)

  popValues <- lapply(popValues, function(p){
    if (!is.null(p$prt)){
      p$fullRates <- .computeFullPopRates(p$prt)
      p$bandedRates <- .computeBandedPopulationRates(p$prt)
    }

    return(p)
  })

  return(popValues)
}
