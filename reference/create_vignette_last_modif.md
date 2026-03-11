# Creates and updates a vignette in the 'vignette/' directory of a package with last modification time of tracked files

Creates and updates a vignette in the 'vignette/' directory of a package
with last modification time of tracked files

## Usage

``` r
create_vignette_last_modif(
  repo = ".",
  path = "R",
  recursive = TRUE,
  untracked = TRUE
)

update_vignette_last_modif(
  repo = ".",
  path = "R",
  recursive = TRUE,
  untracked = TRUE
)
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

No return value, called for side effect. Creates and updates the content
of the "modification_files.Rmd" vignette

## Examples

``` r
# Creates vignette
repo <- fake_repo(as.package = TRUE)
if (rmarkdown::pandoc_available("2.0.0")) {
  create_vignette_last_modif(repo, path = "R")
}
# update vignette
repo <- fake_repo(as.package = TRUE)
if (rmarkdown::pandoc_available("2.0.0")) {
  update_vignette_last_modif(repo, path = "R")
  rmarkdown::render(file.path(repo, "vignettes", "modification_files.Rmd"))
}
#> 
#> 
#> processing file: modification_files.Rmd
#> 1/3        
#> 2/3 [setup]
#> 3/3        
#> output file: modification_files.knit.md
#> Warning: The vignette title specified in \VignetteIndexEntry{} is different from the title in the YAML metadata. The former is "zz-modification-of-files", and the latter is "Modifications of files". If that is intentional, you may set options(rmarkdown.html_vignette.check_title = FALSE) to suppress this check.
#> /opt/hostedtoolcache/pandoc/3.1.11/x64/pandoc +RTS -K512m -RTS modification_files.knit.md --to html4 --from markdown+autolink_bare_uris+tex_math_single_backslash --output modification_files.html --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/pagebreak.lua --lua-filter /home/runner/work/_temp/Library/rmarkdown/rmarkdown/lua/latex-div.lua --embed-resources --standalone --section-divs --template /home/runner/work/_temp/Library/rmarkdown/rmd/h/default.html --highlight-style pygments --css /home/runner/work/_temp/Library/rmarkdown/rmarkdown/templates/html_vignette/resources/vignette.css --mathjax --variable 'mathjax-url=https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --include-in-header /tmp/Rtmpymz4T5/rmarkdown-str19b612749888.html 
#> 
#> Output created: modification_files.html
```
