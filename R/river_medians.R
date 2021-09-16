#' Medians of river lengths and discharges
#'
#' @param data A data frame with two rows. The first row indicates river length and
#'   second row indicates river discharge. The columns of data indicate river names.
#' @param pattern A pattern by which to include only particular rivers
#'
#' @return The median river length and mean river discharge for the desired rivers
#' @export
#'
#' @import stats
#'
#' @examples
#' data("river")
#' river_medians(river, "Mi")
river_medians <- function(data, pattern) {
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
  apply(new_data, 1, stats::median)
}
