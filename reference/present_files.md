# Presenting results of files and last modification time in a printed table using 'kable()'

Presenting results of files and last modification time in a printed
table using 'kable()'

## Usage

``` r
present_files(repo = ".", path = "R", recursive = TRUE, untracked = TRUE)
```

## Arguments

- repo:

  git repository

- path:

  Default to R folder. Use "" for the complete directory

- recursive:

  Logical. Should the listing recurse into directories?

- untracked:

  Logical. Should the not tracked files be included?

## Value

A 'kable()' output to be included in a markdown file

## Examples

``` r
repo <- fake_repo(as.package = TRUE)
cat(present_files(repo))
#> |File        |Tracked in git |Date of creation |Last modification   |  
#> |:-----------|:--------------|:----------------|:-------------------|  
#> |R/my_mean.R |No             |NA               |2026-03-04 01:56:27 |
```
