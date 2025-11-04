#' Wilson Score Lower Bound for Arbitrary Rating Scale
#'
#' Computes the lower bound of the Wilson confidence interval for a given
#' rating and number of reviews, adjusting for arbitrary minimum and maximum scores.
#'
#' @param rating Numeric vector of observed ratings
#' @param n Numeric vector of number of reviews (same length as rating)
#' @param min_score Minimum rating (default = 1)
#' @param max_score Maximum rating (default = 5)
#' @param conf Confidence level for the interval (default = 0.95)
#'
#' @return Numeric vector of Wilson lower bound scores on the original scale
#'
#' @importFrom stats qnorm
#'
#' @examples
#' ratings <- c(5, 1.5, 4)
#' n_reviews <- c(300, 10, 2)
#' wilson_score(ratings, n_reviews, min_score = 1, max_score = 5)
#'
#' # With a 0-10 rating scale
#' ratings2 <- c(8, 3, 5)
#' n_reviews2 <- c(100, 20, 5)
#' wilson_score(ratings2, n_reviews2, min_score = 0, max_score = 10)
#'
#' @export
wilson_score <- function(rating, n,
                         min_score = 1, max_score = 5,
                         conf = 0.95) {

  # validate inputs
  if (any(rating < min_score | rating > max_score))
    stop("Values in `rating` lie outside the allowed range [min_score, max_score].")
  if (any(n < 0))
    stop("Number of reviews `n` must be non-negative.")

  # convert to [0,1]
  p <- (rating - min_score) / (max_score - min_score)

  z <- qnorm(1 - (1 - conf) / 2)

  # Wilson lower bound
  lb <- ifelse(
    n > 0,
    (p + z^2 / (2*n) - z * sqrt((p*(1-p) + z^2 / (4*n)) / n)) /
      (1 + z^2 / n),
    0
  )

  # convert back to original scale
  new_score <- lb * (max_score - min_score) + min_score

  return(new_score)
}
