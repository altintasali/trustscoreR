#' Compute Trust Scores (Wilson and Bayesian) for a Dataset
#'
#' Computes both Wilson score lower bounds and Bayesian-adjusted ratings
#' for a dataset of ratings and review counts, supporting arbitrary scales.
#'
#' The function adds two columns:
#' \itemize{
#'   \item `wilson`: Wilson score lower bound (based on `rating` and `n`)
#'   \item `bayes`: Bayesian-adjusted rating (uses `prior_mean` and `prior_n`)
#' }
#'
#' **Note:** `prior_mean` and `prior_n` are used only for the Bayesian adjustment.
#' `wilson` scores are computed independently of these parameters.
#'
#' @param df Data frame containing feature/entity, rating, and review count columns
#' @param feature_col Name of the column containing feature/entity names (optional, default = NULL)
#' @param rating_col Name of the column containing ratings (default = "rating")
#' @param n_col Name of the column containing number of reviews (default = "n")
#' @param min_score Minimum rating (default = 1)
#' @param max_score Maximum rating (default = 5)
#' @param conf Confidence level for Wilson score (default = 0.95)
#' @param prior_mean Prior expected rating for Bayesian adjustment (default = midpoint)
#'   Represents the rating you would trust if no reviews were available.
#'   Example: on a 1–5 scale, prior_mean = 3.5 represents a neutral prior.
#' @param prior_n Prior strength (pseudo-count / minimum trusted reviews, default = 20)
#'   Controls how strongly the prior pulls small-sample ratings toward `prior_mean`.
#'   Ratings based on fewer reviews than `prior_n` are strongly shrunk;
#'   ratings with many reviews are minimally affected.
#'
#' @return Data frame with added columns:
#'   \item{wilson}{Wilson lower bound score}
#'   \item{bayes}{Bayesian-adjusted rating}
#'
#' @examples
#' df <- data.frame(
#'   company = c("A", "B", "C"),
#'   stars = c(5, 1.5, 4),
#'   votes = c(300, 10, 2)
#' )
#' trustscore(df, feature_col = "company", rating_col = "stars", n_col = "votes")
#'
#' # Custom 0-10 scale and prior
#' df2 <- data.frame(
#'   company = c("X", "Y", "Z"),
#'   score = c(8, 3, 5),
#'   reviews = c(100, 20, 5)
#' )
#' trustscore(df2, feature_col = "company", rating_col = "score", n_col = "reviews",
#'            min_score = 0, max_score = 10, prior_mean = 5, prior_n = 30)
#'
#' @export
trustscore <- function(df,
                       feature_col = NULL,
                       rating_col = "rating",
                       n_col = "n",
                       min_score = 1,
                       max_score = 5,
                       conf = 0.95,
                       prior_mean = (min_score + max_score)/2,
                       prior_n = 20) {

  stopifnot(is.data.frame(df))
  stopifnot(rating_col %in% names(df))
  stopifnot(n_col %in% names(df))
  if (!is.null(feature_col)) stopifnot(feature_col %in% names(df))

  ratings <- df[[rating_col]]
  n_reviews <- df[[n_col]]

  df$wilson <- wilson_score(
    rating = ratings,
    n = n_reviews,
    min_score = min_score,
    max_score = max_score,
    conf = conf
  )

  df$bayes <- bayes_adjusted(
    rating = ratings,
    n = n_reviews,
    min_score = min_score,
    max_score = max_score,
    prior_mean = prior_mean,
    prior_n = prior_n
  )

  df
}
