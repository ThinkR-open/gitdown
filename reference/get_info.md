# Get the first and last modification time for a specific file, based on git2r::blame().

Get the first and last modification time for a specific file, based on
git2r::blame().

## Usage

``` r
get_info(path, repo = ".")
```

## Arguments

- path:

  path to the file

- repo:

  repo of the git project

## Value

A list with information of the selected file:

- file: file name

- in_repository: Logical. Whether the file has already been commit once
  in the git repository

- first_modif: time of first modification. Commit time if in the git
  repository, system date of creation otherwise.

- last_modif: time of last modification. Commit time if in the git
  repository, system date of last modification otherwise.

## Examples

``` r
repo <- fake_repo()
get_info(list.files(repo)[1], repo = repo)
#> $file
#> [1] "NEWS.md"
#> 
#> $in_repository
#> [1] TRUE
#> 
#> $first_modif
#>                     first 
#> "2026-05-13 03:01:47 UTC" 
#> 
#> $last_modif
#>                      last 
#> "2026-05-13 03:01:47 UTC" 
#> 
```
