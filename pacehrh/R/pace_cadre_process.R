computeCadreData <- function(scenario = NULL, roles = NULL) {
  if (is.null(scenario)) {
    return(NULL)
  }

  if (is.null(roles)) {
    return(NULL)
  }

  # Filter to the cadre roles for the specified scenario. Return immediately if there are none.
  roles <- roles[roles$ScenarioID == scenario$UniqueID, ]

  if (nrow(roles) == 0) {
    return(NULL)
  }

  # Beginning and ending years in the experiment range
  minYear <- BVE$years[1]
  maxYear <- BVE$years[length(BVE$years)]

  # Create matrix of annual values
  annualValuesMatrix <- matrix(data = 0, nrow = nrow(roles), ncol = length(BVE$years))

  for (i in seq_along(roles$ScenarioID)) {
    if (is.na(roles$StartYear[i])){
      startYear <- minYear
    } else {
      startYear <- max(roles$StartYear[i], minYear)
    }

    if (is.na(roles$EndYear[i])) {
      endYear <- maxYear
    } else {
      endYear <- min(roles$EndYear[i], maxYear)
    }

    overheadTime <- roles$OverheadHoursPerWeek[i] * 60.0 * scenario$WeeksPerYr
    annualValuesMatrix[i, (startYear - minYear + 1):(endYear - minYear + 1)] <- overheadTime
  }

  dimnames(annualValuesMatrix) <- list("Role" = roles$RoleID, "Year" = BVE$years)

  # Convert matrix of annual values to matrix of monthly manuals.
  # A more elaborate version of this computation is done for seasonality
  # processing, but here we assume that (1) overhead values apply to the entire
  # date range, and (2) overhead values aren't affected by month, so the
  # seasonality curve is flat.

  curve <- c(1,1,1,1,1,1,1,1,1,1,1,1) / 12

  monthlyValuesMatrix <- matrix(data = 0, nrow = nrow(roles), ncol = (length(BVE$years) * length(curve)))

  for (i in seq_along(roles$ScenarioID)) {
    annual <- annualValuesMatrix[i,]
    monthly <- as.vector(matrix(data = curve, ncol = 1) %*% matrix(data = annual, nrow = 1))
    monthlyValuesMatrix[i,] <- monthly
  }

  monthStrs <- as.character(1:12)
  s <- sapply(as.character(BVE$years), function(year) {
    paste0(year, ".", monthStrs)
  })

  dimnames(monthlyValuesMatrix) <- list("Role" = roles$RoleID, "Month" = as.vector(s))

  return(list(annualOverheads = annualValuesMatrix, monthlyOverheads = monthlyValuesMatrix))
}
