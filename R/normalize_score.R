#' Normalize numeric values to a given range
#'
#' @param x Numeric vector of values to scale
#' @param old_min Minimum score (e.g 1 for a rating between 1 and 5)
#' @param old_max Maximum score (e.g 5 for a rating between 1 and 5)
#' @param new_min Desired minimum (default = 0)
#' @param new_max Desired maximum (default = 1)
#'
#' @return Vector scaled to [new_min, new_max]
#'
#' @examples
#' ratings <- c(3.5, 4.2, 4.8)
#' normalize_score(ratings, old_min = 1, old_max = 5)
#'
#' @export
normalize_score <- function(x,
                            old_min = 1,
                            old_max = 5,
                            new_min = 0,
                            new_max = 1) {

  # validation
  if (any(x < old_min | x > old_max, na.rm = TRUE)) {
    stop("Values in `x` fall outside the specified old_min and old_max.")
  }

  # special case: constant vector
  if (old_max == old_min) {
    return(rep((new_min + new_max) / 2, length(x)))
  }

  # normalization formula
  scaled <- (x - old_min) / (old_max - old_min) * (new_max - new_min) + new_min

  return(scaled)
}
