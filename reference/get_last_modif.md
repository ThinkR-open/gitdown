# Get the first and last modification time of files of a directory

Get the first and last modification time of files of a directory

## Usage

``` r
get_last_modif(repo = ".", path = "R", recursive = TRUE, untracked = TRUE)
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

A list of files with information of each file:

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
# Complete repository
get_last_modif(repo = repo, path = "")
#> [[1]]
#> [[1]]$file
#> [1] "NEWS.md"
#> 
#> [[1]]$in_repository
#> [1] TRUE
#> 
#> [[1]]$first_modif
#>                     first 
#> "2026-06-10 03:34:31 UTC" 
#> 
#> [[1]]$last_modif
#>                      last 
#> "2026-06-10 03:34:31 UTC" 
#> 
#> 
#> [[2]]
#> [[2]]$file
#> [1] "example.txt"
#> 
#> [[2]]$in_repository
#> [1] TRUE
#> 
#> [[2]]$first_modif
#>                     first 
#> "2026-06-10 03:34:31 UTC" 
#> 
#> [[2]]$last_modif
#>                      last 
#> "2026-06-10 03:34:31 UTC" 
#> 
#> 
repo <- fake_repo(as.package = TRUE)
# Complete repository
get_last_modif(repo = repo, path = "")
#> [[1]]
#> [[1]]$file
#> [1] "NEWS.md"
#> 
#> [[1]]$in_repository
#> [1] TRUE
#> 
#> [[1]]$first_modif
#>                     first 
#> "2026-06-10 03:34:31 UTC" 
#> 
#> [[1]]$last_modif
#>                      last 
#> "2026-06-10 03:34:31 UTC" 
#> 
#> 
#> [[2]]
#> [[2]]$file
#> [1] "example.txt"
#> 
#> [[2]]$in_repository
#> [1] TRUE
#> 
#> [[2]]$first_modif
#>                     first 
#> "2026-06-10 03:34:31 UTC" 
#> 
#> [[2]]$last_modif
#>                      last 
#> "2026-06-10 03:34:31 UTC" 
#> 
#> 
#> $untracked.untracked
#> $untracked.untracked$file
#> [1] "R/my_mean.R"
#> 
#> $untracked.untracked$in_repository
#> [1] FALSE
#> 
#> $untracked.untracked$first_modif
#> first 
#>    NA 
#> 
#> $untracked.untracked$last_modif
#>                      last 
#> "2026-06-10 03:34:31 UTC" 
#> 
#> 
```
