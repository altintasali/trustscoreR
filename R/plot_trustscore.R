#' Plot Wilson and Bayesian Scores (Base R reshaping)
#'
#' Plots Wilson and Bayesian scores for features/entities with different colors.
#'
#' @param df Data frame containing at least the feature/entity column, 'wilson', and 'bayes' columns
#' @param feature_col Name of the column with feature/entity names (default = "feature")
#' @param sort_by Column to sort features along x-axis ("wilson" or "bayes", default = "wilson")
#' @param point_size Size of points (default = 3)
#'
#' @return A ggplot object
#'
#' @examples
#' df <- data.frame(
#'   company = c("A", "B", "C"),
#'   rating = c(5, 1.5, 4),
#'   n = c(300, 10, 2)
#' )
#' df <- trustscore(df, feature_col = "company", rating_col = "rating", n_col = "n")
#' plot_trustscore(df, feature_col = "company")
#'
#' @import ggplot2
#' @export
plot_trustscore <- function(df,
                            feature_col = "feature",
                            sort_by = "wilson",
                            point_size = 3) {

  stopifnot(feature_col %in% names(df))
  stopifnot(sort_by %in% c("wilson", "bayes"))
  stopifnot(all(c("wilson", "bayes") %in% names(df)))

  library(ggplot2)

  # Order features/entities by the chosen metric
  df[[feature_col]] <- factor(df[[feature_col]],
                              levels = df[[feature_col]][order(df[[sort_by]], decreasing = TRUE)])

  # Convert to long format manually (base R)
  df_long <- data.frame(
    feature = rep(df[[feature_col]], 2),
    score_type = rep(c("wilson", "bayes"), each = nrow(df)),
    score = c(df$wilson, df$bayes)
  )

  # Plot
  ggplot(df_long, aes(x = feature, y = score, color = score_type)) +
    geom_point(size = point_size) +
    labs(x = "Feature/Entity",
         y = "Score",
         color = "Score Type",
         title = "Wilson vs Bayesian Scores") +
    theme_minimal()
}
