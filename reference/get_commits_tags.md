# Get commits associated chronologically with tags

Get commits associated chronologically with tags

## Usage

``` r
get_commits_tags(repo = ".", ref = "main", path = NULL, silent = FALSE)
```

## Arguments

- repo:

  a path to a repository or a `git_repository` object. Default is '.'

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

A tibble with one line for each commit and the following columns:

- sha: sha of the commit

- summary: First line of the commit message

- message: Full content of the commit message

- author: author of the commit

- email: email of the author

- when: commit time

- order: order of commit messages. 1 is the oldest.

- tag.name: name of tag associated with all commits since the last tag

- tag.message: message of the tagged commit

## Examples

``` r
repo <- fake_repo()
get_commits_tags(repo = repo)
#> 4 commits found.
#> # A tibble: 4 × 9
#>   sha            summary message author email when                order tag.name
#>   <chr>          <chr>   <chr>   <chr>  <chr> <dttm>              <int> <chr>   
#> 1 17db76e13f3f5… Add NE… "Add N… Alice  alic… 2026-08-12 01:46:46     4 NA      
#> 2 0e24972ff77e3… Third … "Third… Alice  alic… 2026-08-12 01:46:46     3 v0.1    
#> 3 4a884ca71e9c4… exampl… "examp… Alice  alic… 2026-08-12 01:46:46     2 v0.1    
#> 4 0a6b4d476c00d… First … "First… Alice  alic… 2026-08-12 01:46:46     1 v0.1    
#> # ℹ 1 more variable: tag.message <chr>
```
