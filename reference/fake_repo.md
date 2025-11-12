# Create a fake git repository in a specific folder

A fake repository allows to create reproducible examples for this
package functions

## Usage

``` r
fake_repo(path = tempfile(pattern = "git2r-"), as.package = FALSE)
```

## Arguments

- path:

  Path to fake repository

- as.package:

  Logical. Whether to add R/ and vignettes/ directories to fake a
  package

## Value

Character. Path of a fake repository used for reproducible examples.
Fake repository contains a few files with an initiated git repository.

## Examples

``` r
# Fake repository with git
fake_repo()
#> [1] "/tmp/RtmptOlYwb/git2r-1c9b1e2a3d61"
# Fake repository that looks like package with git
fake_repo(as.package = TRUE)
#> [1] "/tmp/RtmptOlYwb/git2r-1c9b63f4e1e8"
```
