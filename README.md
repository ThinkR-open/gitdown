
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gitdown

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/ThinkR-open/gitdown.svg?branch=master)](https://travis-ci.org/ThinkR-open/gitdown)
[![Coverage
status](https://codecov.io/gh/ThinkR-open/gitdown/branch/master/graph/badge.svg)](https://codecov.io/github/ThinkR-open/gitdown?branch=master)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ThinkR-open/gitdown?branch=master&svg=true)](https://ci.appveyor.com/project/ThinkR-open/gitdown)
<!-- badges: end -->

The goal of {gitdown} is to build a bookdown report of commit messages
arranged according to a pattern. Book can be organised according to git
tags, issues mentionned (*e.g.* `#123`) or any custom character chain
included in your git commit messages (*e.g.* `category_` for use like
`category_ui`, `category_doc`, …).

Full documentation on {pkgdown} site :
<https://thinkr-open.github.io/gitdown/index.html>

## Installation

You can install the last version of {gitdown} from Github:

``` r
remotes::install_github("ThinkR-open/gitdown")
```

## Example

``` r
library(dplyr)
library(gitdown)
## Create fake repository for the example
repo <- fake_repo()
```

Get commits with issues mentionned. The searched pattern is a `#`
followed by at least one number: `"#[[:digit:]]+"`. Variable
`pattern.content` lists patterns found in the commit messages.

``` r
get_commits_pattern(repo, pattern = "#[[:digit:]]+", ref = "master") %>% 
  select(pattern.content, everything())
#> 4 commits found.
#> # A tibble: 7 x 11
#>   pattern.content sha   summary message author email when                order
#>   <chr>           <chr> <chr>   <chr>   <chr>  <chr> <dttm>              <int>
#> 1 #32             68ad… Add NE… "Add N… Alice  alic… 2020-08-06 11:47:48     4
#> 2 #1              68ad… Add NE… "Add N… Alice  alic… 2020-08-06 11:47:48     4
#> 3 #12             68ad… Add NE… "Add N… Alice  alic… 2020-08-06 11:47:48     4
#> 4 #2              6130… Third … "Third… Alice  alic… 2020-08-06 11:47:48     3
#> 5 #145            6130… Third … "Third… Alice  alic… 2020-08-06 11:47:48     3
#> 6 #1              0348… exampl… "examp… Alice  alic… 2020-08-06 11:47:48     2
#> 7 <NA>            9fda… First … "First… Alice  alic… 2020-08-06 11:47:48     1
#> # … with 3 more variables: tag.name <chr>, tag.message <chr>,
#> #   pattern.type <chr>
```

Get commits with issues and specific home-made pattern. Use a named
vector to properly separate types of patterns.

``` r
get_commits_pattern(
  repo, 
  pattern =  c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"),
  ref = "master"
) %>% 
  select(pattern.type, pattern.content, everything())
#> 4 commits found.
#> # A tibble: 12 x 11
#>    pattern.type pattern.content sha   summary message author email
#>    <chr>        <chr>           <chr> <chr>   <chr>   <chr>  <chr>
#>  1 Tickets      ticket6789      68ad… Add NE… "Add N… Alice  alic…
#>  2 Tickets      ticket1234      68ad… Add NE… "Add N… Alice  alic…
#>  3 Issues       #32             68ad… Add NE… "Add N… Alice  alic…
#>  4 Issues       #1              68ad… Add NE… "Add N… Alice  alic…
#>  5 Issues       #12             68ad… Add NE… "Add N… Alice  alic…
#>  6 Tickets      <NA>            6130… Third … "Third… Alice  alic…
#>  7 Issues       #2              6130… Third … "Third… Alice  alic…
#>  8 Issues       #145            6130… Third … "Third… Alice  alic…
#>  9 Tickets      ticket1234      0348… exampl… "examp… Alice  alic…
#> 10 Issues       #1              0348… exampl… "examp… Alice  alic…
#> 11 Tickets      <NA>            9fda… First … "First… Alice  alic…
#> 12 Issues       <NA>            9fda… First … "First… Alice  alic…
#> # … with 4 more variables: when <dttm>, order <int>, tag.name <chr>,
#> #   tag.message <chr>
```

## Create a gitbook of commits sorted by a pattern

``` r
git_down(repo, pattern = c("Tickets" = "ticket[[:digit:]]+",
                           "Issues" = "#[[:digit:]]+"))
```

<img src="reference/figures/gitdown_links.png" width="90%" style="display: block; margin: auto;" />

## Create a vignette that lists all files with date of modification

``` r
repo_pkg <- fake_repo(as.package = TRUE)
# List only files in R/ directory
create_vignette_last_modif(repo_pkg)
# List all files of the git repository
create_vignette_last_modif(repo_pkg, path = "")
```

With this example, the vignette will show this content:

| File         | Tracked in git | Date of creation    | Last modification   |
| :----------- | :------------- | :------------------ | :------------------ |
| NEWS.md      | Yes            | 2020-08-06 13:47:49 | 2020-08-06 13:47:49 |
| example.txt  | Yes            | 2020-08-06 13:47:49 | 2020-08-06 13:47:49 |
| R/my\_mean.R | No             | NA                  | 2020-08-06 13:47:49 |

Please note that the {gitdown} project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
