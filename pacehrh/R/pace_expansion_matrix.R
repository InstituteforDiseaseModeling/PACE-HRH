# Expansion matrix: an R x C matrix which when applied to a C-dimensional
# vector of rates produces an R-dimensional vector with each rate repeated
# a number of times. The number of repetitions is determined by the breaks
# parameter to the .generateExpansionMatrix() function.


#   | 1  0  0 |            | a |
#   | 0  1  0 |   | a |    | b |
#   | 0  1  0 | X | b | =  | b |
#   | 0  0  1 |   | c |    | c |
#   | 0  0  1 |            | c |
#   | 0  0  1 |            | c |

.generateExpansionMatrix <- function(ages = 0:100, breaks = c(0,4,9,14,19,24)){
  if (is.null(breaks) | length(breaks) == 0){
    return(matrix(data = 1, nrow = length(ages), ncol = 1))
  }

  assertthat::assert_that(identical(breaks, breaks[order(breaks)]))
  assertthat::assert_that(identical(ages, ages[order(ages)]))
  assertthat::assert_that(anyDuplicated(breaks) == 0)
  assertthat::assert_that(anyDuplicated(ages) == 0)
  assertthat::assert_that(ages[1] == 0)

  m <- matrix(data = 0, nrow = length(ages), ncol = (length(breaks) + 1))

  startAge <- ages[1]

  for (i in seq_along(breaks)){
    endAge <- breaks[i]

    startRow <- startAge + 1
    endRow <- endAge + 1

    m[startRow:endRow, i] <- 1
    startAge <- endAge + 1
  }

  # Fill in the final bucket
  endAge <- ages[length(ages)]
  if (startAge <= endAge){
    i <- i + 1

    startRow <- startAge + 1
    endRow <- endAge + 1

    m[startRow:endRow, i] <- 1
    startAge <- endAge + 1
  }

  return(m)
}
