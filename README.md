
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
remotes::install_github("ThinkR-open/gitdown")
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
#> 1 #32     9934… Add NE… "Add N… Alice  alic… 2019-09-27 11:08:48     4
#> 2 #1      9934… Add NE… "Add N… Alice  alic… 2019-09-27 11:08:48     4
#> 3 #2      ec63… Third … "Third… Alice  alic… 2019-09-27 11:08:48     3
#> 4 #145    ec63… Third … "Third… Alice  alic… 2019-09-27 11:08:48     3
#> 5 #1      ad53… exampl… "examp… Alice  alic… 2019-09-27 11:08:48     2
#> 6 <NA>    0901… First … First … Alice  alic… 2019-09-27 11:08:48     1
#> # … with 2 more variables: tag.name <chr>, tag.message <chr>
```

## Create a gitbook of commits sorted by a pattern

``` r
git_down(repo, pattern = c("ticket[[:digit:]]+","#[[:digit:]]+"), 
         names_section = c("Ticket", "Issues"))
```
