# trustscoreR

<!-- badges: start -->
<!-- badges: end -->

`trustscoreR` is an R package for statistically robust scoring and ranking of businesses, hospitals, or products based on review data from one or more platforms. Simple averages can be misleading—especially when sample sizes differ—so trustscoreR implements methods such as Wilson score intervals, Bayesian shrinkage, and hierarchical pooling to generate fair and uncertainty-aware ratings. The package accepts ratings and review counts from sources like Google, Trustpilot, and Yelp, and returns adjusted scores, confidence/credible intervals, and rankings. It is designed for analysts and decision-makers who need reliable insights from noisy or imbalanced review data.

## Installation

You can install the development version of trustscoreR from [GitHub](https://github.com/altintasali/trustscoreR) with:

``` r
# install.packages("pak")
pak::pak("altintasali/trustscoreR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(trustscoreR)

company <- LETTERS[1:3]
google_rating <- c(4.2, 4.3, 3.9 )
review_number <- c(200, 60,  1200)

df <- data.frame(company, google_rating, review_number)

adjusted_score <- trustscore(
  df,
  feature_col = "company",
  rating_col = "google_rating",
  n_col = "review_number",
  min_score = 1,
  max_score = 5,
  conf = 0.95
)

print(adjusted_score)
```

| # | company | google_rating  | review_number  | wilson   | bayes    |
|--:|--------:|---------------:|---------------:|---------:|---------:|
| 1 | A       | 4.2            | 200            | 3.956579 | 4.090909 |
| 2 | B       | 4.3            | 60             | 3.840836 | 3.975000 |
| 3 | C       | 3.9            | 1200           | 3.796195 | 3.885246 |



