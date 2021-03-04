
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gitdown <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Coverage
status](https://codecov.io/gh/ThinkR-open/gitdown/branch/master/graph/badge.svg)](https://codecov.io/github/ThinkR-open/gitdown?branch=master)
[![R-CMD-check](https://github.com/ThinkR-open/gitdown/workflows/R-CMD-check/badge.svg)](https://github.com/ThinkR-open/gitdown/actions)
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
#> # A tibble: 7 x 12
#>   pattern.content sha   summary message author email when                order
#>   <chr>           <chr> <chr>   <chr>   <chr>  <chr> <dttm>              <int>
#> 1 #32             03f7… Add NE… "Add N… Alice  alic… 2021-03-04 18:26:32     4
#> 2 #1              03f7… Add NE… "Add N… Alice  alic… 2021-03-04 18:26:32     4
#> 3 #12             03f7… Add NE… "Add N… Alice  alic… 2021-03-04 18:26:32     4
#> 4 #2              e7c9… Third … "Third… Alice  alic… 2021-03-04 18:26:32     3
#> 5 #145            e7c9… Third … "Third… Alice  alic… 2021-03-04 18:26:32     3
#> 6 #1              a02f… exampl… "examp… Alice  alic… 2021-03-04 18:26:32     2
#> 7 <NA>            8fcf… First … "First… Alice  alic… 2021-03-04 18:26:32     1
#> # … with 4 more variables: tag.name <chr>, tag.message <chr>,
#> #   pattern.type <chr>, pattern.title <chr>
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
#> # A tibble: 12 x 12
#>    pattern.type pattern.content sha   summary message author email
#>    <chr>        <chr>           <chr> <chr>   <chr>   <chr>  <chr>
#>  1 Tickets      ticket6789      03f7… Add NE… "Add N… Alice  alic…
#>  2 Tickets      ticket1234      03f7… Add NE… "Add N… Alice  alic…
#>  3 Issues       #32             03f7… Add NE… "Add N… Alice  alic…
#>  4 Issues       #1              03f7… Add NE… "Add N… Alice  alic…
#>  5 Issues       #12             03f7… Add NE… "Add N… Alice  alic…
#>  6 Tickets      <NA>            e7c9… Third … "Third… Alice  alic…
#>  7 Issues       #2              e7c9… Third … "Third… Alice  alic…
#>  8 Issues       #145            e7c9… Third … "Third… Alice  alic…
#>  9 Tickets      ticket1234      a02f… exampl… "examp… Alice  alic…
#> 10 Issues       #1              a02f… exampl… "examp… Alice  alic…
#> 11 Tickets      <NA>            8fcf… First … "First… Alice  alic…
#> 12 Issues       <NA>            8fcf… First … "First… Alice  alic…
#> # … with 5 more variables: when <dttm>, order <int>, tag.name <chr>,
#> #   tag.message <chr>, pattern.title <chr>
```

## Create a gitbook of commits sorted by a pattern

``` r
git_down(repo, pattern = c("Tickets" = "ticket[[:digit:]]+",
                           "Issues" = "#[[:digit:]]+"))
```

<img src="reference/figures/gitdown_links.png" width="90%" style="display: block; margin: auto;" />

If you add a table of correspondance, you can change titles of the
patterns.  
*Note that you can use [{gitlabr}](https://statnmap.github.io/gitlabr/)
or [{gh}](gh.r-lib.org) to retrieve list of issues from Gitlab or Github
respectively.*

``` r
# With table of correspondance
pattern.table <- data.frame(
  number = c("#2", "#1", "#1000"),
  title = c("#2 A second issue to illustrate a blog post",
            "#1 An example of issue",
            "#1000 issue with no commit"))
git_down(
  pattern = c("Issue" = "#[[:digit:]]+"),
  pattern.table = pattern.table
)
```

*Note that characters like `[`, `]`, `_` or `*` will be replaced by `-`
in the titles to avoid conflicts with markdown syntax.*

<img src="reference/figures/issues-with-title.png" width="90%" style="display: block; margin: auto;" />

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
|:-------------|:---------------|:--------------------|:--------------------|
| NEWS.md      | Yes            | 2021-03-04 19:26:33 | 2021-03-04 19:26:33 |
| example.txt  | Yes            | 2021-03-04 19:26:33 | 2021-03-04 19:26:33 |
| R/my\_mean.R | No             | NA                  | 2021-03-04 19:26:33 |

## Sponsor

The development of this package has been sponsored by:

<a href = "https://www.servier.fr/"><img src = "man/figures/servier.png"></img></a>

## Code of Conduct

Please note that the {gitdown} project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.
