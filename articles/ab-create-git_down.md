# Create a gitbook with git_down

## Create a reproducible example of a versioned directory

Create a versioned directory with some commits and a NEWS.md in a
temporary directory

- Some commits mention an issue with `#`
- Some commits mention a ticket with `ticket`
- A commit is associated with a tag

``` r
repo <- fake_repo()
```

## Build book of commits messages

The main function of {gitdown} is to build this gitbook with all commit
messages ordered according to a pattern. Each commit message associated
with an issue will be recorded in the section of this issue. A commit
message can thus appears multiple times if it is associated with
multiple issues.  
If you have your own referencing system for tickets in an external
software, you can also create the gitbook associated like using `ticket`
as in the example below.

``` r
git_down(repo, pattern = c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"))
```

## Read list of commits and extract information

As a side effect of {gitdown}, you can get some intermediate information
used to build the book with some exported functions.

### Get commits and find pattern

- Find all commits of a branch and associate with tags recursively

``` r
get_commits_tags(repo, ref = "main")
#> 4 commits found.
#> # A tibble: 4 × 9
#>   sha            summary message author email when                order tag.name
#>   <chr>          <chr>   <chr>   <chr>  <chr> <dttm>              <int> <chr>   
#> 1 4d73c163d7dbc… Add NE… "Add N… Alice  alic… 2026-02-04 01:57:09     4 NA      
#> 2 67f8a45da8883… Third … "Third… Alice  alic… 2026-02-04 01:57:09     3 v0.1    
#> 3 d3a728f4cc63d… exampl… "examp… Alice  alic… 2026-02-04 01:57:09     2 v0.1    
#> 4 3c402a5956c18… First … "First… Alice  alic… 2026-02-04 01:57:09     1 v0.1    
#> # ℹ 1 more variable: tag.message <chr>
```

- Find all commits of a branch and detect a specific pattern
  - Here we find commits mentioning an issue with `#123`

``` r
get_commits_pattern(repo, pattern = "#[[:digit:]]+", ref = "main")
#> 4 commits found.
#> # A tibble: 7 × 12
#>   sha            summary message author email when                order tag.name
#>   <chr>          <chr>   <chr>   <chr>  <chr> <dttm>              <int> <chr>   
#> 1 4d73c163d7dbc… Add NE… "Add N… Alice  alic… 2026-02-04 01:57:09     4 NA      
#> 2 4d73c163d7dbc… Add NE… "Add N… Alice  alic… 2026-02-04 01:57:09     4 NA      
#> 3 4d73c163d7dbc… Add NE… "Add N… Alice  alic… 2026-02-04 01:57:09     4 NA      
#> 4 67f8a45da8883… Third … "Third… Alice  alic… 2026-02-04 01:57:09     3 v0.1    
#> 5 67f8a45da8883… Third … "Third… Alice  alic… 2026-02-04 01:57:09     3 v0.1    
#> 6 d3a728f4cc63d… exampl… "examp… Alice  alic… 2026-02-04 01:57:09     2 v0.1    
#> 7 3c402a5956c18… First … "First… Alice  alic… 2026-02-04 01:57:09     1 v0.1    
#> # ℹ 4 more variables: tag.message <chr>, pattern.type <chr>,
#> #   pattern.content <chr>, pattern.title <chr>
```

## Create a vignette that lists all files with date of modification

``` r
repo_pkg <- fake_repo(as.package = TRUE)
# List only files in R/ directory
create_vignette_last_modif(repo_pkg)
# List all files of the git repository
create_vignette_last_modif(repo_pkg, path = "")
```

With this example, the vignette will show this content:

| File        | Tracked in git | Date of creation    | Last modification   |
|:------------|:---------------|:--------------------|:--------------------|
| NEWS.md     | Yes            | 2026-02-04 01:57:09 | 2026-02-04 01:57:09 |
| example.txt | Yes            | 2026-02-04 01:57:09 | 2026-02-04 01:57:09 |
| R/my_mean.R | No             | NA                  | 2026-02-04 01:57:09 |
