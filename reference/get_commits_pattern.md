# Get commits associated with a text pattern

Get commits associated with a text pattern

## Usage

``` r
get_commits_pattern(
  repo = ".",
  pattern = c(Ticket = "#[[:digit:]]+"),
  pattern.table = NULL,
  ref = "main",
  path = NULL,
  silent = FALSE
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

- path:

  The path to a file. If not NULL, only commits modifying this file will
  be returned. Note that modifying commits that occurred before the file
  was given its present name are not returned; that is, the output of
  `git log` with `--no-follow` is reproduced.

- silent:

  Logical. Whether to hide messages.

## Value

A tibble with one line for each commit, duplicated if associated with
multiple patterns and the following columns:

- sha: sha of the commit

- summary: First line of the commit message

- message: Full content of the commit message

- author: author of the commit

- email: email of the author

- when: commit time

- order: order of commit messages. 1 is the oldest.

- tag.name: name of tag associated with all commits since the last tag

- tag.message: message of the tagged commit

- pattern.type: name of the pattern found in the commit message

- pattern.content: pattern found in the commit message

## Examples

``` r
repo <- fake_repo()
get_commits_pattern(repo = repo, pattern = c("Ticket" = "#[[:digit:]]+"))
#> 4 commits found.
#> # A tibble: 7 × 12
#>   sha            summary message author email when                order tag.name
#>   <chr>          <chr>   <chr>   <chr>  <chr> <dttm>              <int> <chr>   
#> 1 c74a3c2eb6757… Add NE… "Add N… Alice  alic… 2026-08-05 02:22:19     4 NA      
#> 2 c74a3c2eb6757… Add NE… "Add N… Alice  alic… 2026-08-05 02:22:19     4 NA      
#> 3 c74a3c2eb6757… Add NE… "Add N… Alice  alic… 2026-08-05 02:22:19     4 NA      
#> 4 38684ab7ca2d8… Third … "Third… Alice  alic… 2026-08-05 02:22:19     3 v0.1    
#> 5 38684ab7ca2d8… Third … "Third… Alice  alic… 2026-08-05 02:22:19     3 v0.1    
#> 6 55ecdd6b3f1b0… exampl… "examp… Alice  alic… 2026-08-05 02:22:19     2 v0.1    
#> 7 2581547ce0c75… First … "First… Alice  alic… 2026-08-05 02:22:19     1 v0.1    
#> # ℹ 4 more variables: tag.message <chr>, pattern.type <chr>,
#> #   pattern.content <chr>, pattern.title <chr>
get_commits_pattern(repo = repo,
  pattern = c("Ticket" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"))
#> 4 commits found.
#> # A tibble: 12 × 12
#>    sha           summary message author email when                order tag.name
#>    <chr>         <chr>   <chr>   <chr>  <chr> <dttm>              <int> <chr>   
#>  1 c74a3c2eb675… Add NE… "Add N… Alice  alic… 2026-08-05 02:22:19     4 NA      
#>  2 c74a3c2eb675… Add NE… "Add N… Alice  alic… 2026-08-05 02:22:19     4 NA      
#>  3 c74a3c2eb675… Add NE… "Add N… Alice  alic… 2026-08-05 02:22:19     4 NA      
#>  4 c74a3c2eb675… Add NE… "Add N… Alice  alic… 2026-08-05 02:22:19     4 NA      
#>  5 c74a3c2eb675… Add NE… "Add N… Alice  alic… 2026-08-05 02:22:19     4 NA      
#>  6 38684ab7ca2d… Third … "Third… Alice  alic… 2026-08-05 02:22:19     3 v0.1    
#>  7 38684ab7ca2d… Third … "Third… Alice  alic… 2026-08-05 02:22:19     3 v0.1    
#>  8 38684ab7ca2d… Third … "Third… Alice  alic… 2026-08-05 02:22:19     3 v0.1    
#>  9 55ecdd6b3f1b… exampl… "examp… Alice  alic… 2026-08-05 02:22:19     2 v0.1    
#> 10 55ecdd6b3f1b… exampl… "examp… Alice  alic… 2026-08-05 02:22:19     2 v0.1    
#> 11 2581547ce0c7… First … "First… Alice  alic… 2026-08-05 02:22:19     1 v0.1    
#> 12 2581547ce0c7… First … "First… Alice  alic… 2026-08-05 02:22:19     1 v0.1    
#> # ℹ 4 more variables: tag.message <chr>, pattern.type <chr>,
#> #   pattern.content <chr>, pattern.title <chr>
```
