#' Summary statistics of river lengths and discharges
#'
#' @param data A data frame with two rows. The first row indicates river length and
#'   second row indicates river discharge. The columns of data indicate river names.
#' @param pattern A pattern by which to include only particular rivers
#' @param FUN A function to summarize the rivers by
#' @param ... Additional arguments to \code{apply()}
#'
#' @return The summarized river length and mean river discharge for the desired rivers
#' @export
#'
#' @examples
#' data("river")
#' river_stat(river, "Mi|C", mean, trim = 0.5)
river_stat <- function(data, pattern, FUN, ...) {
  # find the column locations of the rivers matching the pattern
  desired_rivers <- grep(pattern, names(data))
  # error if a pattern is not matched
  if (length(desired_rivers) == 0) {
    stop(paste0(
      "The pattern, ",
      "\"",
      pattern,
      "\"",
      ", does not match any names in the data, ",
      substitute(data),
      "."
    )
    )
  }
  # subset the original data to include only the desired rivers
  new_data <- data[, desired_rivers, drop = FALSE]
  apply(new_data, 1, FUN, ...)
}
