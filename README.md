
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gitdown <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Coverage
status](https://codecov.io/gh/ThinkR-open/gitdown/branch/master/graph/badge.svg)](https://codecov.io/github/ThinkR-open/gitdown?branch=master)
[![R-CMD-check](https://github.com/ThinkR-open/gitdown/workflows/R-CMD-check/badge.svg)](https://github.com/ThinkR-open/gitdown/actions)
<!-- badges: end -->

The goal of {gitdown} is to build a bookdown report of commit messages
arranged according to a pattern. Book can be organized according to git
tags, issues mentioned (*e.g.* `#123`) or any custom character chain
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

Get commits with issues mentioned. The searched pattern is a `#`
followed by at least one number: `"#[[:digit:]]+"`. Variable
`pattern.content` lists patterns found in the commit messages.

``` r
get_commits_pattern(repo, pattern = "#[[:digit:]]+", ref = "master") %>% 
  select(pattern.content, everything())
#> 4 commits found.
#> # A tibble: 7 x 12
#>   pattern.content sha    summary message  author email when                order
#>   <chr>           <chr>  <chr>   <chr>    <chr>  <chr> <dttm>              <int>
#> 1 #32             9ba68… Add NE… "Add NE… Alice  alic… 2021-04-10 10:34:07     4
#> 2 #1              9ba68… Add NE… "Add NE… Alice  alic… 2021-04-10 10:34:07     4
#> 3 #12             9ba68… Add NE… "Add NE… Alice  alic… 2021-04-10 10:34:07     4
#> 4 #2              33fb4… Third … "Third … Alice  alic… 2021-04-10 10:34:07     3
#> 5 #145            33fb4… Third … "Third … Alice  alic… 2021-04-10 10:34:07     3
#> 6 #1              2939d… exampl… "exampl… Alice  alic… 2021-04-10 10:34:07     2
#> 7 <NA>            1f045… First … "First … Alice  alic… 2021-04-10 10:34:07     1
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
#>    pattern.type pattern.content sha       summary   message        author email 
#>    <chr>        <chr>           <chr>     <chr>     <chr>          <chr>  <chr> 
#>  1 Tickets      ticket6789      9ba682a8… Add NEWS  "Add NEWS\n\n… Alice  alice…
#>  2 Tickets      ticket1234      9ba682a8… Add NEWS  "Add NEWS\n\n… Alice  alice…
#>  3 Issues       #32             9ba682a8… Add NEWS  "Add NEWS\n\n… Alice  alice…
#>  4 Issues       #1              9ba682a8… Add NEWS  "Add NEWS\n\n… Alice  alice…
#>  5 Issues       #12             9ba682a8… Add NEWS  "Add NEWS\n\n… Alice  alice…
#>  6 Tickets      <NA>            33fb411b… Third co… "Third commit… Alice  alice…
#>  7 Issues       #2              33fb411b… Third co… "Third commit… Alice  alice…
#>  8 Issues       #145            33fb411b… Third co… "Third commit… Alice  alice…
#>  9 Tickets      ticket1234      2939d5b0… example:… "example: mod… Alice  alice…
#> 10 Issues       #1              2939d5b0… example:… "example: mod… Alice  alice…
#> 11 Tickets      <NA>            1f045324… First co… "First commit… Alice  alice…
#> 12 Issues       <NA>            1f045324… First co… "First commit… Alice  alice…
#> # … with 5 more variables: when <dttm>, order <int>, tag.name <chr>,
#> #   tag.message <chr>, pattern.title <chr>
```

## Create a gitbook of commits sorted by a pattern

``` r
git_down(repo, pattern = c("Tickets" = "ticket[[:digit:]]+",
                           "Issues" = "#[[:digit:]]+"))
```

<img src="reference/figures/gitdown_links.png" width="90%" style="display: block; margin: auto;" />

If you add a table of correspondence, you can change titles of the
patterns.  
*Note that you can use [{gitlabr}](https://statnmap.github.io/gitlabr/)
or [{gh}](https://gh.r-lib.org) to retrieve list of issues from Gitlab
or Github respectively.*

``` r
# With table of correspondence
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
| NEWS.md      | Yes            | 2021-04-10 12:34:07 | 2021-04-10 12:34:07 |
| example.txt  | Yes            | 2021-04-10 12:34:07 | 2021-04-10 12:34:07 |
| R/my\_mean.R | No             | NA                  | 2021-04-10 12:34:07 |

## Sponsor

The development of this package has been sponsored by:

<a href = "https://www.servier.fr/"><img src = "man/figures/servier.png"></img></a>

## Code of Conduct

Please note that the {gitdown} project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
