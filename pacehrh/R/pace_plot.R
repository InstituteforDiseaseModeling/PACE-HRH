#' Plot A Single Population Curve
#'
#' @param pop Population values as a numeric vector
#' @param xaxis Vector of age values for the x axis
#' @param color Plot color
#' @param title Plot title
#'
#' @return ggplot grob
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 ggtitle
#'
#' @examples
#' \dontrun{
#' library(pacehrh)
#'
#' pacehrh::InitializePopulation()
#' pacehrh::InitializeScenarios()
#' pacehrh::InitializeStochasticParameters()
#' pacehrh::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   pacehrh::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#'
#' pacehrh::PlotPopulationCurve(results[[26]]$Population[["2025"]]$Male)
#' }
#'
PlotPopulationCurve <- function(pop, xaxis = NULL, color = .colorM, title = ""){

  if (is.null(xaxis)){
    # seq_along produces 1 ... n, but population curves usually start at 0, so
    # reset the x values to start at 0.
    xaxis <- seq_along(pop) - 1
  }

  assertthat::assert_that(length(pop) == length(xaxis))

  df <- data.frame(Age = xaxis, Population = pop)

  g <- ggplot2::ggplot(data = df)
  g <- g + geom_line(aes(x = Age, y = Population), color = color)
  g <- g + geom_point(aes(x = Age, y = Population), color = color, shape = 0)
  g <- g + scale_y_continuous(labels = scales::comma)
  g <- g + ggtitle(title)

  return(g)
}

.validResultsParams <- function(results, trial, year){
  if (is.null(results)){
    return(FALSE)
  }

  if ((year %in% GPE$years) != TRUE){
    return(FALSE)
  }

  if (trial < 1 | trial > length(results)){
    return(FALSE)
  }

  return(TRUE)
}

#' Plot A Single Population Curve From A Results List
#'
#' More intuitive interface to [PlotPopulationCurve()]
#'
#' @param results Results list (as returned by \code{RunExperiments()})
#' @param trial Trial number (index into the results list)
#' @param year Year in trial timeseries to plot
#' @param sex Sex to plot (from c("m", "M", "f", "F"), defaults to female)
#' @param ... Parameters passed to [PlotPopulationCurve()]
#'
#' @md
#'
#' @return ggplot grob, or NULL on error
#' @export
#'
#' @examples
#' \dontrun{
#' # These two function calls produce the same output:
#' pacehrh::PlotResultsPopulationCurve(results, trial = 26, year = 2025, sex = "m")
#' pacehrh::PlotPopulationCurve(results[[26]]$Population[["2025"]]$Male)
#' }
PlotResultsPopulationCurve <- function(results, trial = 1, year = 2020, sex = "f", ...){
  if (!.validResultsParams(results, trial, year)){
    return(NULL)
  }

  if ((tolower(sex) %in% c("m", "f")) == FALSE){
    sex <- "f"
  }

  args <- list(...)

  # Use the color argument if it's provided, otherwise use the standard
  # male/female colors
  if (("color" %in% names(args)) != TRUE){
    args$color <- ifelse(tolower(sex) == "m", .colorM, .colorF)
  }

  demographics <- results[[trial]]$Population[[as.character(year)]]

  if (sex == "f"){
    pop <- demographics$Female
  } else {
    pop <- demographics$Male
  }

  args$pop <- pop

  return(do.call(PlotPopulationCurve, args))
}

#' Plot A Family Of Population Curves
#'
#' @param ... Population vectors
#' @param xaxis Vector of age values for the x axis
#' @param colors Vector of colors for each population line
#' @param shapes Vector of shapes for each population point
#'
#' @return ggplot grob
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_y_continuous
#'
#' @examples
#' \dontrun{
#' g <- pacehrh::PlotPopulationCurves(
#'   results[[26]]$Population[['2020']]$Male,
#'   results[[26]]$Population[['2021']]$Male,
#'   results[[26]]$Population[['2022']]$Male,
#'   results[[26]]$Population[['2023']]$Male,
#'   results[[26]]$Population[['2024']]$Male,
#'   results[[26]]$Population[['2025']]$Male,
#'   xaxis = pacehrh:::GPE$ages,
#'   colors = c("royalblue4", "royalblue3", "royalblue2", "royalblue1"),
#'   shapes = c(0, 1, 2, 5)
#' )
#' print(g)
#' }
PlotPopulationCurves <- function(... , xaxis = NULL, colors = NULL, shapes = NULL){
  pops <- list(...)

  assertthat::assert_that(length(pops) > 0)

  if (is.null(xaxis)){
    xaxis <- seq_along(pops[[1]]) - 1
  }

  # Check that all the population vectors have the same length, and that
  # length is the same as the xaxis
  lengths <- sapply(pops, length)
  assertthat::assert_that(length(unique(lengths)) == 1)
  assertthat::assert_that(unique(lengths)[1] == length(xaxis))

  if (is.null(colors)){
    colors = c("blue", "red")
  }

  if (is.null(shapes)){
    shapes = c(0)
  }

  colorIter <- iter(colors, recycle = TRUE)
  shapeIter <- iter(shapes, recycle = TRUE)

  g <- ggplot2::ggplot(data = NULL)

  nul <- lapply(pops, function(pop){
    color <- nextElem(colorIter)
    shape <- nextElem(shapeIter)

    g <<- g + geom_line(aes(x = xaxis, y = pop), color = color)
    g <<- g + geom_point(aes(x = xaxis, y = pop), color = color, shape = shape)
    return(1)
  })

  g <- g + xlab("Age") + ylab("Population")
  g <- g + scale_y_continuous(labels = scales::comma)

  return(g)
}

#' Convert Population Pyramids Into One Dataframe
#'
#' Rearrange the demographics structure from a list of dataframes with combined
#' male/female population data into one long, skinny dataframe with Year and
#' Gender fields.
#'
#' @param pops List of population pyramids for several years
#'
#' @return Long skinny dataframe of population information
gatherPopulation <- function(pops){
  if (is.null(pops) | length(pops) == 0){
    return(NULL)
  }

  years <- names(pops)

  rangeVector <- GPE$ages
  rangeSize <- length(rangeVector)
  yearVector <- vector(mode = "numeric", length = rangeSize)
  genderVectorMale <- vector(mode = "character", length = rangeSize)
  genderVectorFemale <- vector(mode = "character", length = rangeSize)
  genderVectorTotal <- vector(mode = "character", length = rangeSize)
  genderVectorMale <- "M"
  genderVectorFemale <- "F"
  genderVectorTotal <- "FM"

  x <- lapply(years, FUN = function(year){
    pop <- pops[[year]]

    yearVector <- as.numeric(year)

    assertthat::are_equal(rangeSize, length(pop$Female))
    assertthat::are_equal(rangeSize, length(pop$Male))

    df.f <- data.frame(Year = yearVector,
                       Gender = genderVectorFemale,
                       Age = rangeVector,
                       Population = pop$Female)

    df.m <- data.frame(Year = yearVector,
                       Gender = genderVectorMale,
                       Age = rangeVector,
                       Population = pop$Male)

    df.fm <- data.frame(Year = yearVector,
                       Gender = genderVectorTotal,
                       Age = rangeVector,
                       Population = pop$Female + pop$Male)

    return(data.table::rbindlist(list(df.f, df.m, df.fm)))
  })

  return(data.table::rbindlist(x))
}

#' Plot A Single Population Pyramid
#'
#' @param df Long, skinny dataframe of population data
#' @param year Year to graph
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 annotation_custom
#' @importFrom scales label_comma
#' @importFrom grid grobTree
#' @importFrom grid textGrob
#' @importFrom grid gpar
#'
#' @return Graphics grob
#' @export
#'
#' @examples
#' \dontrun{
#' df <- pacehrh:::gatherPopulation(results[[1]]$Population)
#' g <- pacehrh::PlotPyramid(df, 2021)
#' print(g)
#' }
PlotPyramid <- function(df, year){
  df <- df[df$Year == year,]
  df.f <- df[df$Gender == 'F',]
  df.m <- df[df$Gender == 'M',]

  totalF <- sum(df.f$Population)
  totalM <- sum(df.m$Population)

  df.f$Percent <- 100* (df.f$Population / totalF)
  df.m$Percent <- 100* (df.m$Population / totalM)

  max <- max(max(df.f$Percent), max(df.m$Percent))

  limits <- ceiling(abs(max)) * c(-1, 1)
  breaks <- seq(limits[1], limits[2], 1)
  labels <- scales::label_comma(accuracy = .1)(abs(breaks))

  grobF <-
    grobTree(textGrob(
      paste("FEMALE (N=", scales::label_comma()(totalF), ")", sep = ""),
      x = 0.9,
      y = 0.75,
      hjust = 1,
      gp = gpar(
        col = .colorF,
        fontsize = 13,
        fontface = "italic"
      )
    ))

  grobM <-
    grobTree(textGrob(
      paste("MALE (N=", scales::label_comma()(totalM), ")", sep = ""),
      x = 0.1,
      y = 0.75,
      hjust = 0,
      gp = gpar(
        col = .colorM,
        fontsize = 13,
        fontface = "italic"
      )
    ))

  g <- ggplot(data = df, aes(x = Age))
  g <-
    g + geom_segment(
      data = df.f,
      aes(
        x = Age,
        y = 0,
        xend = Age,
        yend = Percent
      ),
      color = .colorF,
      size = 1.5
    )
  g <-
    g + geom_segment(
      data = df.m,
      aes(
        x = Age,
        y = 0,
        xend = Age,
        yend = (-1 * Percent)
      ),
      color = .colorM,
      size = 1.5
    )
  g <- g + scale_y_continuous(limits = limits, breaks = breaks, labels = labels)
  g <- g + xlab("Age") + ylab("Percent of Population")
  g <- g + coord_flip()
  g <- g + annotation_custom(grobF) + annotation_custom(grobM)

  return(g)
}

#' Plot A Family Of Population Pyramids
#'
#' @param df Long, skinny dataframe of population data
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_segment
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 coord_flip
#' @importFrom ggplot2 annotation_custom
#' @importFrom scales label_comma
#' @importFrom grid grobTree
#' @importFrom grid textGrob
#' @importFrom grid gpar
#'
#' @return Graphics grob
#' @export
#'
#' @examples
#' \dontrun{
#' df <- pacehrh:::gatherPopulation(results[[1]]$Population)
#' g <- pacehrh::PlotPyramids(df)
#' print(g)
#' }
PlotPyramids <- function(df) {
  dff <-
    df %>% group_by(Year, Gender) %>% summarize(Total = sum(Population))
  mdf <- merge(df, dff)

  mdf$Percent <- 100 * (mdf$Population / mdf$Total)

  mdf.f <- mdf[Gender == 'F', ]
  mdf.m <- mdf[Gender == 'M', ]

  max <- max(max(mdf.f$Percent), max(mdf.m$Percent))

  limits <- ceiling(abs(max)) * c(-1, 1)
  breaks <- seq(limits[1], limits[2], 1)
  labels <- scales::label_comma(accuracy = .1)(abs(breaks))

  g <-
    ggplot(data = mdf, aes(x = Age)) + facet_wrap(vars(Year), ncol = 5)
  g <-
    g + geom_segment(
      data = mdf.f,
      aes(
        x = Age,
        y = 0,
        xend = Age,
        yend = Percent
      ),
      color = .colorF,
      size = 1
    )
  g <-
    g + geom_segment(
      data = mdf.m,
      aes(
        x = Age,
        y = 0,
        xend = Age,
        yend = (-1 * Percent)
      ),
      color = .colorM,
      size = 1
    )
  g <- g + scale_y_continuous(limits = limits, breaks = breaks, labels = labels)
  g <- g + xlab("Age") + ylab("Percent of Population")
  g <- g + coord_flip()
  print(g)
}

#' Plot A Single Pair of Mortality Rates Curves From A Results List
#'
#' @param results Results list (as returned by \code{RunExperiments()})
#' @param trial Trail number (index into the results list)
#' @param year Year in trial timeseries to plot
#'
#' @return ggplot grob, or NULL on error
#' @export
#'
#' @examples
#' \dontrun{
#' library(pacehrh)
#'
#' pacehrh::InitializePopulation()
#' pacehrh::InitializeScenarios()
#' pacehrh::InitializeStochasticParameters()
#' pacehrh::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   pacehrh::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#'
#' g <- pacehrh::PlotResultsMortalityRates(results, 49, 2030)
#' print(g)
#' }
PlotResultsMortalityRates <- function(results, trial = 1, year = 2020){
  if (!.validResultsParams(results, trial, year)) {
    return(NULL)
  }

  return(PlotMortalityRates(results[[trial]]$PopulationRates, year))
}

#' Plot Mortality Rates
#'
#' @param populationRates Population rates list, as returned by \code{RunExperiments()}
#' @param year Year
#'
#' @return ggplot grob
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 vars
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 scale_color_manual
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \dontrun{
#' library(pacehrh)
#'
#' pacehrh::InitializePopulation()
#' pacehrh::InitializeScenarios()
#' pacehrh::InitializeStochasticParameters()
#' pacehrh::InitializeSeasonality()
#'
#' scenario <- "ScenarioName"
#'
#' results <-
#'   pacehrh::RunExperiments(scenarioName = scenario,
#'                        trials = 100)
#'
#' g <- pacehrh::PlotMortalityRates(results[[49]]$PopulationRates, 2030)
#' print(g)
#' }
PlotMortalityRates <- function(populationRates, year){
  rates <- .explodeRates(populationRates, year)

  df <- as.data.frame(rates)
  df$Age <- GPE$ages
  dff <-
    tidyr::pivot_longer(
      df,
      cols = c(
        "femaleFertility",
        "maleFertility",
        "femaleMortality",
        "maleMortality"
      ),
      names_to = "Sex",
      values_to = "Rate"
    )

  dff <- dff[dff$Sex %in% c("femaleMortality", "maleMortality"),]

  titleStr <- paste("Mortality Rates (", year, ")", sep = "")

  g <- ggplot(dff, aes(x = Age, y = Rate, color = Sex))
  g <- g + scale_color_manual(values = c(.colorF, .colorM))
  g <- g + theme(legend.position = "none")
  g <- g + geom_point(alpha = 0.5)
  g <- g + facet_grid(cols = vars(Sex))
  g <- g + ggtitle(titleStr) + xlab("Age") + ylab("Rate")
  return(g)
}
