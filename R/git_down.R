#' git history to bookdown
#'
#' This function turns the history of git into a bookdown.
#' Each chapter is a group of commits. The first page gives a summary of all the groups.
#'
#' @param repo The path to a repository. Default is `.`
#' @param book_path The path to the bookdown output. Default is `"gitdown"`.
#' @param open Should the bookdown be opened once compiled? Default is TRUE.
#' @param author Author of the Bookdown
#' @param pattern pattern to expose commits, like "#[[:digit:]]" for issues
#' @param names_section names for each section, one pattern = one section
#' @param ref the name of the branch, by default master
#'
#' @export
#'
#' @importFrom attempt if_not
#' @importFrom dplyr group_by pull
#' @importFrom knitr kable knit
#' @importFrom rmarkdown render
#' @importFrom stats setNames
#' @importFrom tidyr nest
#' @importFrom utils read.csv2 browseURL data
#' @importFrom magrittr %>%
#' @importFrom git2r workdir
#' @importFrom purrr walk2
#'
git_down <- function(repo = ".", book_path = "gitdown", open = TRUE, author = "John Doe", pattern, names_section, ref = "master"){
  unlink(file.path(repo, book_path), recursive = TRUE)
  if_not(
    file.path(repo, book_path),
    dir.exists,
    ~ dir.create(file.path(repo, book_path),recursive = TRUE)
  )
  lapply(
    list.files(system.file("booktemplate/", package = "gitdown"), full.names = TRUE),
    function(x){file.copy(from = x, to = normalizePath(file.path(repo, book_path)))}
  )
  replace_in_file(
    file.path(repo, book_path, "_bookdown.yml"),
    "Gitbook",
    basename(repo))
  replace_in_file(
    file.path(repo, book_path, "index.Rmd"),
    "XXXXXX",
    basename(repo)
  )
  replace_in_file(
    file.path(repo, book_path, "index.Rmd"),
    "Yihui Xie",
    gsub("([^<]+) <.*", "\\1", author)
  )

  pat <- pattern
  meta_name <- basename(git2r::workdir(repo = repo))
  write_file <- function(x){
    write_in(x = x, repo = repo)
    }
  write_file("\n")
  write_file(paste("# Gitbook for ", meta_name,"{-} \n"))
  write_file(paste("Done on:", Sys.time(),"\n"))
  write_file("\n")

  walk2(pat, names_section, ~ each_pattern(.x, .y, repo, ref, write_file))
  res <- render(
    file.path(repo, "gitdown", "index.Rmd"))
  #knit(file.path(repo, "tests/testdown", "index.Rmd"))
  if (open){
    browseURL(res)
  }
  res
}
