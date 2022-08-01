.allowedFPlotTypes <- c("lines", "ribbon", "boxplot")
.defaultFPlotType <- .allowedFPlotTypes[1]

.validateFPlotType <- function(type) {
  allowedTypes <-

  if (is.null(type)) {
    return(.defaultFPlotType)
  }

  if (!is.character(type)) {
    return(.defaultFPlotType)
  }

  if (type %in% .allowedFPlotTypes) {
    return(type)
  }

  return(.defaultFPlotType)
}

#' Extract And Combine Fertility Matrices Across Multiple Trials
#'
#' @param results Results list (as returned by \code{RunExperiments()})
#'
#' @return Data frame of fertility rates across all trials
#'
#' @importFrom tidyr pivot_longer
#'
.combineFRatesMatrices <- function(results) {
  l <- lapply(results, function(r) {
    df <- as.data.frame(r$PopulationParams$FRatesMatrix)
    cols <- names(df)
    df$Label <- row.names(df)
    df <-
      tidyr::pivot_longer(
        df,
        cols = all_of(cols),
        names_to = "Year",
        values_to = "Rate"
      )
    return(df)
  })

  df <- data.table::rbindlist(l)
  df$LogRate <- log10(df$Rate)

  # Label                Year Rate        LogRate
  # AnnualBirthRate15_19 2020 0.075817219 -1.120232
  # AnnualBirthRate15_19 2021 0.076072137 -1.118774
  # AnnualBirthRate15_19 2022 0.073017269 -1.136574
  # AnnualBirthRate15_19 2023 0.080289581 -1.095341
  # AnnualBirthRate15_19 2024 0.070030775 -1.154711

  return(df)
}

#' Plot Fertility Rates Statistics
#'
#' @param results Results list (as returned by \code{RunExperiments()})
#' @param se Whether to show standard error intervals, or projection intervals
#'
#' @return ggplot grob
#' @export
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 vars
#' @importFrom ggplot2 scale_x_discrete
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 geom_errorbar
#' @importFrom ggplot2 geom_pointrange
#' @importFrom ggplot2 geom_ribbon
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#'
PlotFertilityRatesStats <- function(results, se = FALSE, type = "lines") {
  if (is.null(results)) {
    return(NULL)
  }

  n <- length(results)

  if (n < 1) {
    return(NULL)
  }

  type <- .validateFPlotType(type)

  df <- .combineFRatesMatrices(results)

  if (type == "boxplot") {
    return(.fRatesBoxPlot(df))
  } else if (type == "ribbon") {
    return(.fRatesRibbonPlot(df, se, n))
  } else if (type == "lines") {
    return(.fRatesLinesPlot(df, se, n))
  } else {
    return(.fRatesLinesPlot(df, se, n))
  }
}

#' Plot Fertility Rates Statistics As Box Plot
#'
#' @param results Results list (as returned by \code{RunExperiments()})
#'
#' @return ggplot grob
#'
.fRatesBoxPlot <- function(df) {
  g <- ggplot(df, aes(
    x = Year,
    y = LogRate,
    color = Label,
    group = Year))
  g <- g + geom_boxplot()
  g <- g + facet_wrap(vars(Label))
  g <- g + scale_x_discrete(breaks = c(2020, 2030, 2040))
  g <- g + theme(legend.position = "none")
  g <- g + ylab("log10(Rate)") + xlab("Year")

  return(g)
}

#' Plot Fertility Rates Statistics As Ribbon Plot
#'
#' @param results Results list (as returned by \code{RunExperiments()})
#' @param se Whether to show standard error intervals
#' @param trials Number of trials
#'
#' @return ggplot grob
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#'
.fRatesRibbonPlot <- function(df, se, trials) {
  dff <- df %>% group_by(Label,Year) %>% summarize(m = mean(LogRate), sd = sd(LogRate))

  # Compute the 95% confidence interval
  if (se == TRUE) {
    dff$CI <- dff$sd * qt(0.975, trials - 1) / sqrt(trials)
    ylabel <- "Mean log10(Rate) (CI = 95%)"
  } else {
    dff$CI <- dff$sd * qt(0.975, trials - 1)
    ylabel <- "Variance log10(Rate) (CI = 95%)"
  }

  g <- ggplot(dff, aes(
    x = Year,
    y = m,
    color = Label,
    group = Label
  ))
  g <- g + geom_ribbon(aes(ymin = m - CI, ymax = m + CI, fill = Label), alpha = 0.5)
  g <- g + geom_line(size = .5)
  g <- g + facet_wrap(vars(Label))
  g <- g + scale_x_discrete(breaks = c(2020, 2030, 2040))
  g <- g + theme(legend.position = "none")
  g <- g + ylab(ylabel) + xlab("Year")
  g <- g + xlab("Year")

  return(g)
}

#' Plot Fertility Rates Statistics As Error Bars Plot
#'
#' @param results Results list (as returned by \code{RunExperiments()})
#' @param se Whether to show standard error intervals
#' @param trials Number of trials
#'
#' @return ggplot grob
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#'
.fRatesLinesPlot <- function(df, se, trials) {
  dff <- df %>% group_by(Label,Year) %>% summarize(m = mean(LogRate), sd = sd(LogRate))

  # Compute the 95% confidence interval
  if (se == TRUE) {
    dff$CI <- dff$sd * qt(0.975, trials - 1) / sqrt(trials)
    ylabel <- "Mean log10(Rate) (CI = 95%)"
  } else {
    dff$CI <- dff$sd * qt(0.975, trials - 1)
    ylabel <- "Variance log10(Rate) (CI = 95%)"
  }

  g <- ggplot(dff, aes(
    x = Year,
    y = m,
    color = Label,
    group = Label
  ))
  g <- g + geom_pointrange(aes(ymin = m - CI, ymax = m + CI), size = 0.5)
  g <- g + facet_wrap(vars(Label))
  g <- g + scale_x_discrete(breaks = c(2020, 2030, 2040))
  g <- g + theme(legend.position = "none")
  g <- g + ylab(ylabel) + xlab("Year")
  g <- g + xlab("Year")

  return(g)
}
