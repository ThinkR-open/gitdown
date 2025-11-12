# Nest all commits by each pattern found in the commit messages

Nest all commits by each pattern found in the commit messages

## Usage

``` r
nest_commits_by_pattern(
  repo,
  pattern = c(Issues = "#[[:digit:]]+"),
  pattern.table = NULL,
  ref = "main",
  silent = TRUE
)
```

## Arguments

- repo:

  a path to a repository or a `git_repository` object. Default is '.'

- pattern:

  Named vector with regex pattern to expose commits, like
  `c("Issues" = "#\[\[:digit:\]\]")` for issues

- pattern.table:

  data.frame with two columns: pattern and description of the pattern.
  This is used as correspondence table to add some names to existing
  patterns.

- ref:

  The name of a reference to list commits from e.g. a tag or a branch.
  The default is NULL for the current branch.

- silent:

  Logical. Whether to hide messages.

## Value

A tibble with a row for each different pattern found in commit messages
and following columns:

- pattern.type: name of the pattern found in the commit message

- pattern.content: pattern found in the commit message

- pattern.title: pattern.content or title used in the pattern.table a
  nested 'data' column with all related commits

- pattern_numeric: extraction of numeric value in pattern.content

- link_pattern: internal url of the pattern in the future HTML gitbook

- data: a nested list of tibbles with commits content as issued from
  [`get_commits_pattern()`](get_commits_pattern.md)

## Examples

``` r
repo <- fake_repo()
nest_commits_by_pattern(repo)
#> # A tibble: 6 × 6
#>   pattern.type pattern.content   pattern.title     pattern_numeric link_pattern
#>   <chr>        <chr>             <chr>                       <dbl> <chr>       
#> 1 Issues       #1                #1                              1 issues-1    
#> 2 Issues       #2                #2                              2 issues-2    
#> 3 Issues       #12               #12                            12 issues-12   
#> 4 Issues       #32               #32                            32 issues-32   
#> 5 Issues       #145              #145                          145 issues-145  
#> 6 Issues       No related issues No related issues              NA issues-na   
#> # ℹ 1 more variable: data <list>

# With table of correspondence
pattern.table <- data.frame(
  number = c("#2", "#1", "#1000"),
  title = c("#2 A second issue to illustrate a blog post",
            "#1 An example of issue",
            "#1000 issue with no commit"))
 nest_commits_by_pattern(
  repo,
  pattern.table = pattern.table,
  pattern = c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"))
#> # A tibble: 10 × 6
#>    pattern.type pattern.content    pattern.title    pattern_numeric link_pattern
#>    <chr>        <chr>              <chr>                      <dbl> <chr>       
#>  1 Issues       #1                 #1 An example o…               1 issues-1    
#>  2 Issues       #2                 #2 A second iss…               2 issues-2    
#>  3 Issues       #12                #12                           12 issues-12   
#>  4 Issues       #32                #32                           32 issues-32   
#>  5 Issues       #145               #145                         145 issues-145  
#>  6 Issues       #1000              #1000 issue wit…            1000 issues-1000 
#>  7 Issues       No related issues  No related issu…              NA issues-na   
#>  8 Tickets      ticket1234         ticket1234                  1234 tickets-tic…
#>  9 Tickets      ticket6789         ticket6789                  6789 tickets-tic…
#> 10 Tickets      No related tickets No related tick…              NA tickets-na  
#> # ℹ 1 more variable: data <list>
```
