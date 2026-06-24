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
#> 1 307b806ee52d3… Add NE… "Add N… Alice  alic… 2026-06-24 03:27:46     4 NA      
#> 2 d08228c4d7aa5… Third … "Third… Alice  alic… 2026-06-24 03:27:46     3 v0.1    
#> 3 c7ec8d6d1cab5… exampl… "examp… Alice  alic… 2026-06-24 03:27:46     2 v0.1    
#> 4 d280ce85ccbb5… First … "First… Alice  alic… 2026-06-24 03:27:46     1 v0.1    
#> # ℹ 1 more variable: tag.message <chr>
```
