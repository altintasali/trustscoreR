#' Bayesian Adjusted Rating (Beta-Binomial Shrinkage) for Arbitrary Scale
#'
#' Computes a Bayesian shrinkage estimate of a rating, adjusting for
#' the number of reviews and a prior belief. This is useful because
#' small numbers of reviews can produce extreme or unreliable average ratings.
#'
#' The shrinkage estimate is based on a Beta-Binomial model:
#' \deqn{ \hat{\theta} = \frac{\alpha + r}{\alpha + \beta + n} }
#' where:
#' \itemize{
#'   \item \eqn{r = p \cdot n} is the number of "successes" (rating converted to [0,1])
#'   \item \eqn{\alpha = prior\_mean \cdot prior\_n}
#'   \item \eqn{\beta = (1 - prior\_mean) \cdot prior\_n}
#' }
#'
#' @param rating Numeric vector of observed ratings
#' @param n Numeric vector of number of reviews (same length as rating)
#' @param min_score Minimum rating (default = 1)
#' @param max_score Maximum rating (default = 5)
#' @param prior_mean Prior expected rating on the same scale as `rating`.
#'   Represents the rating you would trust if no reviews were available.
#'   Default is the midpoint of the scale.
#'   Example: on a 1–5 scale, prior_mean = 3.5 represents a neutral prior.
#' @param prior_n Prior strength (pseudo-count). Interpreted as the **minimum trusted number of reviews**.
#'   It controls how strongly the prior pulls small-sample ratings toward `prior_mean`.
#'   Ratings based on fewer reviews than `prior_n` are strongly shrunk; ratings with many reviews are minimally affected.
#'
#' @return Numeric vector of Bayesian-adjusted ratings on the original scale
#'
#' @examples
#' # 1-5 scale
#' ratings <- c(5, 1.5, 4)
#' n_reviews <- c(300, 10, 2)
#' bayes_adjusted(ratings, n_reviews, min_score = 1, max_score = 5)
#'
#' # 0-10 scale with custom prior
#' ratings2 <- c(8, 3, 5)
#' n_reviews2 <- c(100, 20, 5)
#' bayes_adjusted(ratings2, n_reviews2, min_score = 0, max_score = 10,
#'                prior_mean = 5, prior_n = 30)
#'
#' @export
bayes_adjusted <- function(rating, n,
                           min_score = 1, max_score = 5,
                           prior_mean = (min_score + max_score)/2,
                           prior_n = 20) {

  # validate inputs
  if (any(rating < min_score | rating > max_score))
    stop("Values in `rating` lie outside the allowed range [min_score, max_score].")
  if (any(n < 0))
    stop("Number of reviews `n` must be non-negative.")
  if (prior_mean < min_score || prior_mean > max_score)
    stop("`prior_mean` must lie within [min_score, max_score].")
  if (prior_n < 0)
    stop("`prior_n` must be non-negative.")

  # convert rating and prior to [0,1]
  p <- (rating - min_score) / (max_score - min_score)
  p0 <- (prior_mean - min_score) / (max_score - min_score)

  # prior alpha and beta
  a0 <- p0 * prior_n
  b0 <- (1 - p0) * prior_n

  # posterior mean in [0,1]
  post <- (a0 + p * n) / (a0 + b0 + n)

  # convert back to original scale
  adjusted_score <- post * (max_score - min_score) + min_score

  return(adjusted_score)
}
