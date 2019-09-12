#' git history to bookdown
#'
#' This function turns the history of git into a bookdown.
#' Each chapter is a group of commits. The first page gives a summary of all the groups.
#'
#' @param repo The path to a repository. Default is `.`
#' @param book_path The path to the bookdown output. Default is `"gitdown"`.
#' @param open Should the bookdown be opened once compiled? Default is TRUE.
#' @param author Author of the Bookdown
#'
#' @export
#'
#' @importFrom attempt if_not
#' @importFrom devtools as.package test
#' @importFrom dplyr group_by pull
#' @importFrom knitr kable knit
#' @importFrom rmarkdown render
#' @importFrom stats setNames
#' @importFrom tidyr nest
#' @importFrom utils read.csv2 browseURL data
#' @importFrom magrittr %>%
#'
git_down <- function(repo = ".", book_path = "gitdown", open = TRUE, author = "John Doe"){
  browser()
  # meta <- as.package(repo)
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
    "teeest",
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
  # temp_csv <- file.path(repo, book_path, "testcsv.csv")
  # file.create(temp_csv)
  # on.exit(unlink(temp_csv))
  # write(file = temp_csv, "Context; Test;Location;Test time;Result;File Name")
  # a <- test(repo, reporter = rmd_reporter)
  commits <- get_commits_pattern(repo, pattern = "#[[:digit:]]+", ref = "master", silent = TRUE)


  write_in <- function(x, there = file.path(repo, "tests/testdown", "index.Rmd")){
    write(x, file = there, append = TRUE)
  }
  write_in("\n")
  write_in(paste("# Coverage results for package", meta$package,"{-} \n"))
  write_in(paste("Done on:", Sys.time(),"\n"))
  write_in("\n")
  write_in(kable(a))
  y <- read.csv2(temp_csv)
  x <- y %>%
    group_by(Context, File.Name) %>%
    nest()
  res <- pull(x, data)
  res <- setNames(res, x$Context)
  for (i in seq_along(res)){
    write_in("\n")
    write_in( paste( "#", names(res)[i] ) )
    write_in("\n")
    write_in(kable(res[i]))
    write_in("\n")
  }

  res <- render(
    file.path(repo, "gitdown", "index.Rmd"))
  #knit(file.path(repo, "tests/testdown", "index.Rmd"))
  if (open){
    browseURL(res)
  }
  res
}
