
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gitdown

<!-- badges: start -->

<!-- badges: end -->

The goal of {gitdown} is to build a bookdown report of commit messages
arranged according to a pattern. Book can be organised according to git
tags, issues mentionned (*e.g.* `#123`) or any custom character chain
included in your git commit messages (*e.g.* `category_` for use like
`category_ui`, `category_doc`, …).

## Installation

You can install the last version of {gitdown} from Github:

``` r
remotes::install_github("gitdown")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dplyr)
library(gitdown)
## Create fake repository for the example
repo <- fake_repo()
```

Get commits with issues mentionned. The searched pattern is a `#`
followed by at least one number: `pattern =
"#[[:digit:]]+"`.

``` r
get_commits_pattern(repo, pattern = "#[[:digit:]]+", ref = "master") %>% 
  select(pattern, everything())
#> 4 commits found.
#> # A tibble: 6 x 10
#>   pattern sha   summary message author email when                order
#>   <chr>   <chr> <chr>   <chr>   <chr>  <chr> <dttm>              <int>
#> 1 #32     ce7e… Add NE… "Add N… Alice  alic… 2019-09-12 22:22:49     4
#> 2 #1      ce7e… Add NE… "Add N… Alice  alic… 2019-09-12 22:22:49     4
#> 3 #2      6ace… Third … "Third… Alice  alic… 2019-09-12 22:22:49     3
#> 4 #145    6ace… Third … "Third… Alice  alic… 2019-09-12 22:22:49     3
#> 5 #1      67a1… exampl… "examp… Alice  alic… 2019-09-12 22:22:49     2
#> 6 <NA>    ecdc… First … First … Alice  alic… 2019-09-12 22:22:49     1
#> # … with 2 more variables: tag.name <chr>, tag.message <chr>
```
