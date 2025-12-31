# Turns the active branch history of git into a bookdown.

Read all commit messages of your local git repository and sort them
according to tags or specific text pattern into chapters of a HTML book
using 'bookdown'. Each chapter is a group of commits. The first page
gives a summary of all the groups.

## Usage

``` r
git_down(
  repo = ".",
  book_path = "gitdown",
  open = TRUE,
  author = "John Doe",
  pattern = c(Issues = "#[[:digit:]]+"),
  pattern.table = NULL,
  ref = "main",
  ...
)
```

## Arguments

- repo:

  The path to a repository. Default is `.`

- book_path:

  The path to the bookdown output. Default is `"gitdown"`.

- open:

  Should the bookdown be opened once compiled? Default is TRUE.

- author:

  Author of the bookdown

- pattern:

  Named vector with regex pattern to expose commits, like
  `c("Issues" = "#\[\[:digit:\]\]")` for issues

- pattern.table:

  data.frame with two columns: pattern and description of the pattern.
  This is used as correspondence table to add some names to existing
  patterns.

- ref:

  the name of the branch, by default main

- ...:

  Other parameters to pass to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

## Value

Path of the HTML gitbook saved in the repo/book_path directory.

## Examples

``` r
repo <- fake_repo()
if (rmarkdown::pandoc_available("2.0.0")) {
  res <- git_down(repo, pattern = c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"),
    open = FALSE)
}
#> 
#> 
#> processing file: index.Rmd
#> 1/1
#> output file: index.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS index.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output index.html --lua-filter /home/runner/work/_temp/Library/bookdown/rmarkdown/lua/custom-environment.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/anchor-sections.lua --metadata-file /tmp/Rtmpl5QGP9/file1b20215673ad --wrap preserve --standalone --section-divs --table-of-contents --toc-depth 3 --template /home/runner/work/_temp/Library/bookdown/templates/gitbook.html --highlight-style pygments --number-sections --css style.css --mathjax --include-in-header /tmp/Rtmpl5QGP9/rmarkdown-str1b2078ada9f.html 
#> 
#> Output created: gitbook-for-git2r-1b202df5d1a3.html
if (FALSE) { # \dontrun{
# Open the book
  browseURL(res)
} # }
# With table of correspondence
pattern.table <- data.frame(number = c("#2", "#1"),
  title = c("#2 A second issue to illustrate a blog post",
                       "#1 An example of issue"))
if (rmarkdown::pandoc_available("2.0.0")) {
  res <- git_down(repo, pattern = c("Issues" = "#[[:digit:]]+"),
    pattern.table = pattern.table, open = FALSE)
}
#> 
#> 
#> processing file: index.Rmd
#> 1/1
#> output file: index.knit.md
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS index.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output index.html --lua-filter /home/runner/work/_temp/Library/bookdown/rmarkdown/lua/custom-environment.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/anchor-sections.lua --metadata-file /tmp/Rtmpl5QGP9/file1b207ad16482 --wrap preserve --standalone --section-divs --table-of-contents --toc-depth 3 --template /home/runner/work/_temp/Library/bookdown/templates/gitbook.html --highlight-style pygments --number-sections --css style.css --mathjax --include-in-header /tmp/Rtmpl5QGP9/rmarkdown-str1b20349c2032.html 
#> 
#> Output created: gitbook-for-git2r-1b202df5d1a3.html
if (FALSE) { # \dontrun{
# Open the book
  browseURL(res)
} # }
```
