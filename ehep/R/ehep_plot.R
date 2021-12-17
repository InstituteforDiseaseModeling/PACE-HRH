#' Plot A Single Population Curve
#'
#' @param pop Population values as a numeric vector
#' @param xaxis Vector of age values for the x axis
#' @param color Plot color
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
PlotPopulationCurve <- function(pop, xaxis = NULL, color = NULL){

  if (is.null(xaxis)){
    # seq_along produces 1 ... n, but population curves usually start at 0, so
    # reset the x values to start at 0.
    xaxis <- seq_along(pop) - 1
  }

  assertthat::assert_that(length(pop) == length(xaxis))

  if (is.null(color)){
    color = "blue"
  }

  df <- data.frame(Age = xaxis, Population = pop)

  g <- ggplot2::ggplot(data = df)
  g <- g + geom_line(aes(x = Age, y = Population), color = color)
  g <- g + geom_point(aes(x = Age, y = Population), color = color, shape = 0)
  g <- g + scale_y_continuous(labels = scales::comma)

  print(g)

  return(g)
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

  g <- g + scale_y_continuous(labels = scales::comma)

  print(g)

  return(g)
}
