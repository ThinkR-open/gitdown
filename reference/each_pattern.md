# Create the text to add in a markdown file to present each pattern as a chapter of the book

Create the text to add in a markdown file to present each pattern as a
chapter of the book

## Usage

``` r
each_pattern(nest_commits, pattern.type)
```

## Arguments

- nest_commits:

  commits as nested with nest_commits_by_pattern

- pattern.type:

  Character name of the pattern to filter

## Value

A tibble with a row for each different pattern found and a 'text' column
to be included in a markdown file:

- pattern.content: pattern found in the commit message

- link_pattern: internal url of the pattern in the future HTML gitbook

- text: list of vectors of markdown text to present commits of each
  pattern in the HTML gitbook output

- pattern.type: name of the pattern found in the commit message

- pattern.title: pattern.content or title used in the pattern.table a
  nested 'data' column with all related commits

- pattern_numeric: extraction of numeric value in pattern.content

- data: a nested list of tibbles with commits content as issued from
  [`get_commits_pattern()`](get_commits_pattern.md)

## Examples

``` r
repo <- fake_repo()
res_commits <- nest_commits_by_pattern(
  repo,
  pattern = c("Tickets" = "ticket[[:digit:]]+"),
  ref = "main", silent = TRUE
)
each_pattern(res_commits, "Tickets")
#> # A tibble: 4 × 7
#>   pattern.content  link_pattern text  pattern.type pattern.title pattern_numeric
#>   <chr>            <chr>        <lis> <chr>        <chr>                   <dbl>
#> 1 Tickets          tickets      <chr> NA           NA                         NA
#> 2 ticket1234       tickets-tic… <chr> Tickets      ticket1234               1234
#> 3 ticket6789       tickets-tic… <chr> Tickets      ticket6789               6789
#> 4 No related tick… tickets-na   <chr> Tickets      No related t…              NA
#> # ℹ 1 more variable: data <list>
```
