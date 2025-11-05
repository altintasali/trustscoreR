#' Bayesian adjusted rating (beta-binomial shrinkage) for arbitrary scale
#'
#' Computes a Bayesian shrinkage estimate of a rating, adjusting for
#' the number of reviews and a prior belief. This is useful because
#' small numbers of reviews can produce extreme or unreliable average ratings.
#'
#' @section Why Bayesian adjusted scores?:
#' Raw averages are extremely sensitive when review counts are small.
#' A feature with a single 5-star rating should not outrank another with
#' hundreds of reviews averaging 4.8. \cr
#' \code{bayes_adjusted()} solves this by shrinking each observed rating
#' toward a prior value (\code{prior_mean}), with the strength of the pull
#' controlled by \code{prior_n}. \cr
#' High-volume items are barely affected; low-volume items are pulled
#' toward a neutral rating until more evidence is available.
#'
#' @section Formula:
#' \strong{Under the hood: Beta–Binomial shrinkage!}
#'
#' First, each rating is mapped to the 0-1 scale:
#' \deqn{ p = \frac{rating - min\_score}{max\_score - min\_score} }
#'
#' With \eqn{n} reviews, the data contribute \eqn{r = p \cdot n} "successes".
#'
#' The prior is expressed as a Beta distribution with parameters:
#' \deqn{ \alpha = prior\_mean \cdot prior\_n }
#' \deqn{ \beta  = (1 - prior\_mean) \cdot prior\_n }
#'
#' Combining the prior and observed data yields a posterior mean:
#' \deqn{
#'   \hat{\theta}
#'   = \frac{\alpha + r}{\alpha + \beta + n}
#' }
#'
#' Intuition:
#' \itemize{
#'   \item If \eqn{n} is small, the prior dominates and \eqn{\hat{\theta}} stays close to \code{prior_mean}
#'   \item As \eqn{n} grows large, \eqn{\hat{\theta}} approaches the observed average
#' }
#'
#' Finally, \eqn{\hat{\theta}} is mapped back to the original rating scale.
#'
#' @param rating Numeric vector of observed ratings
#' @param n Numeric vector of number of reviews (same length as \code{rating})
#' @param min_score Minimum rating (default = 1)
#' @param max_score Maximum rating (default = 5)
#' @param prior_mean Prior expected rating on the same scale as \code{rating}.
#'   Represents the rating you would trust if no reviews were available.
#'   Default is the midpoint of the scale.
#' @param prior_n Prior strength (pseudo-count). Interpreted as the
#'   \emph{minimum trusted number of reviews}. Ratings based on fewer
#'   reviews than \code{prior_n} are strongly shrunk; ratings with many reviews
#'   are minimally affected.
#'
#' @return Numeric vector of Bayesian-adjusted ratings on the original scale
#'
#' @examples
#' # Example 1: Standard 1–5 rating scale
#' ratings <- c(5, 2, 4.5, 5)
#' n_reviews <- c(1, 5, 30, 500)
#'
#' # Raw averages vs Bayesian shrinkage
#' data.frame(
#'   rating = ratings,
#'   n = n_reviews,
#'   bayes = bayes_adjusted(ratings, n_reviews)
#' )
#'
#' # The item with 1 review (5 stars) is pulled downward strongly,
#' # while the item with 500 reviews remains almost unchanged.
#'
#' # Example 2: Custom 0–10 scale with a neutral prior (mean = 5) and stronger shrinkage
#' ratings2 <- c(10, 7, 2)
#' n_reviews2 <- c(2, 20, 500)
#'
#' bayes_adjusted(ratings2, n_reviews2,
#'                min_score = 0, max_score = 10,
#'                prior_mean = 5, prior_n = 30)
#'
#' # Items with very few reviews are pulled strongly toward 5 (neutral),
#' # while the item with 500 reviews remains close to its observed average.
#'
#' @seealso
#' \code{\link{wilson_score}} for conservative, uncertainty-aware ranking of ratings
#' based on the Wilson confidence interval.
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
  score <- post * (max_score - min_score) + min_score

  return(score)
}
