#' Rank Companies by Trust Score
#'
#' Adds a ranking column based on either the Wilson score or Bayesian-adjusted rating.
#' Optionally, ties can be broken by the number of reviews.
#'
#' @param df Data frame containing trust scores (output from `trustscore()`)
#' @param score_col Column to use for ranking ("wilson" or "bayes", default = "wilson")
#' @param n_col Name of the column containing the number of reviews (optional, default = NULL)
#'   If provided, ties are broken by descending number of reviews.
#' @param rank_col Name of the output ranking column (default = "rank")
#'
#' @return Data frame with an additional column for rank
#'
#' @examples
#' df <- data.frame(
#'   company = c("A", "B", "C"),
#'   stars = c(5, 1.5, 4),
#'   votes = c(300, 10, 2)
#' )
#' df <- trustscore(df, rating_col = "stars", n_col = "votes")
#' rank_trustscore(df) # defaults to Wilson score
#'
#' # Rank by Bayesian-adjusted rating with tie-breaker
#' rank_trustscore(df, score_col = "bayes", n_col = "votes")
#'
#' @export
rank_trustscore <- function(df, score_col = "wilson", n_col = NULL, rank_col = "rank") {

  stopifnot(score_col %in% names(df))
  if (!is.null(n_col)) stopifnot(n_col %in% names(df))

  # Create a ranking key
  if (!is.null(n_col)) {
    # Rank by score, then by number of reviews (descending)
    ranking <- order(-df[[score_col]], -df[[n_col]])
  } else {
    # Rank by score only
    ranking <- order(-df[[score_col]])
  }

  df[[rank_col]] <- rank(ranking, ties.method = "first")
  df
}
