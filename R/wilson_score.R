#' Wilson Score Lower Bound for Arbitrary Rating Scales
#'
#' Computes the lower bound of the Wilson confidence interval for a mean rating,
#' generalized to any rating scale. Instead of using the raw average rating,
#' this method produces a conservative score that accounts for uncertainty:
#' items with few reviews get penalized, while high-volume items stay near their
#' observed average.
#'
#' @section Why Wilson scoring?:
#' Raw averages are unstable when review counts are small.
#' A feature with a single 5-star rating should not outrank another with
#' hundreds of reviews averaging 4.8.
#'
#' Wilson scoring asks a different question:
#' \emph{"What is the worst reasonable rating this item could have?"}.
#' The lower bound of the Wilson confidence interval gives a statistically
#' conservative estimate. Low-review items are pushed downward, while
#' well-reviewed items remain stable. This makes Wilson a robust way to sort
#' items by rating while accounting for uncertainty.
#'
#' @section Formula:
#' Ratings are first mapped to [0,1]:
#' \deqn{ p = \frac{\text{rating} - min\_score}{max\_score - min\_score} }
#'
#' Let \eqn{n} be the number of reviews and \eqn{z} the Z-value for the
#' confidence level. The Wilson lower bound is:
#' \deqn{
#' \hat{p}_{lower} =
#' \frac{
#'   p + \frac{z^2}{2n} - z \sqrt{\frac{p(1-p) + z^2/(4n)}{n}}
#' }{
#'   1 + \frac{z^2}{n}
#' }
#' }
#' If \eqn{n = 0}, the score defaults to the bottom of the scale.
#' Finally, \eqn{\hat{p}_{lower}} is mapped back to the original rating scale.
#'
#' @param rating Numeric vector of observed ratings
#' @param n Numeric vector of number of reviews (same length as \code{rating})
#' @param min_score Minimum rating (default = 1)
#' @param max_score Maximum rating (default = 5)
#' @param conf Confidence level for the interval (default = 0.95)
#'
#' @return Numeric vector of Wilson lower bound scores on the original scale
#'
#' @importFrom stats qnorm
#'
#' @examples
#' # Example 1: Standard 1–5 rating scale
#' ratings <- c(5, 2, 4.5, 5)
#' n_reviews <- c(1, 5, 30, 500)
#' wilson_score(ratings, n_reviews)
#'
#' # Example 2: Custom 0–10 scale with neutral prior
#' ratings2 <- c(10, 7, 2)
#' n_reviews2 <- c(2, 20, 500)
#' wilson_score(ratings2, n_reviews2, min_score = 0, max_score = 10)
#'
#' @seealso
#'   \code{\link{bayes_adjusted}} for Bayesian shrinkage toward a prior mean.
#'
#' @export
wilson_score <- function(rating, n,
                         min_score = 1, max_score = 5,
                         conf = 0.95) {

  # input checks
  if (any(rating < min_score | rating > max_score))
    stop("Values in `rating` lie outside [min_score, max_score].")
  if (any(n < 0))
    stop("Number of reviews `n` must be non-negative.")

  # convert to [0,1]
  p <- (rating - min_score) / (max_score - min_score)

  # z-value for confidence interval
  z <- qnorm(1 - (1 - conf) / 2)

  # Wilson lower bound
  lb <- ifelse(
    n > 0,
    (p + z^2 / (2*n) - z * sqrt((p*(1-p) + z^2/(4*n)) / n)) / (1 + z^2 / n),
    0
  )

  # map back to original scale
  score <- lb * (max_score - min_score) + min_score

  return(score)
}
