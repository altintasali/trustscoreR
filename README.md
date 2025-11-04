
# trustscoreR

<!-- badges: start -->
<!-- badges: end -->

`trustscoreR` is an R package for statistically robust scoring and ranking of businesses, hospitals, or products based on review data from one or more platforms. Simple averages can be misleading—especially when sample sizes differ—so trustscoreR implements methods such as Wilson score intervals, Bayesian shrinkage, and hierarchical pooling to generate fair and uncertainty-aware ratings. The package accepts ratings and review counts from sources like Google, Trustpilot, and Yelp, and returns adjusted scores, confidence/credible intervals, and rankings. It is designed for analysts and decision-makers who need reliable insights from noisy or imbalanced review data.

## Installation

You can install the development version of trustscoreR from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("altintasali/trustscoreR")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(trustscoreR)
## basic example code
```

