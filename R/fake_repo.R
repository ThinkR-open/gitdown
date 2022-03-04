#' Create a fake git repository in a specific folder
#'
#' A fake repository allows to create reproducible examples for this package functions
#'
#' @param path Path to fake repository
#' @param as.package Logical. Whether to add R/ and vignettes/
#'  directories to fake a package
#'
#' @importFrom git2r init config add commit tag
#' @export
#'
#' @return Character. Path of a fake repository used for reproducible examples.
#' Fake repository contains a few files with an initiated git repository.
#' @examples
#' # Fake repository with git
#' fake_repo()
#' # Fake repository that looks like package with git
#' fake_repo(as.package = TRUE)

fake_repo <- function(path = tempfile(pattern = "git2r-"), as.package = FALSE) {
  if (!dir.exists(path)) {dir.create(path)}
  repo <- init(path)

  ## Config user
  config(repo, user.name = "Alice", user.email = "alice@example.org")

  ## Write to a file and commit
  writeLines(
    "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
    file.path(path, "example.txt")
  )
  add(repo, "example.txt")
  commit(repo, "First commit message")

  ## Change file and commit
  writeLines(
    c(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
      "eiusmod tempor incididunt ut labore et dolore magna aliqua."
    ),
    file.path(path, "example.txt")
  )
  add(repo, "example.txt")
  commit(repo, "example: modification\n\nissue #1\nticket1234")

  ## Change file again and commit
  writeLines(
    c(
      "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do",
      "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad",
      "minim veniam, quis nostrud exercitation ullamco laboris nisi ut"
    ),
    file.path(path, "example.txt")
  )
  add(repo, "example.txt")
  commit(repo, "Third commit message\n\n-issue #2\n-issue #145.\n More information important for the project as breaking changes")

  ## Create a tag
  tag(repo, name = "v0.1", message = "Tag v0.1 message")

  ## Add NEWS.md
  writeLines(
    c(
      "# my.package 0.0.1",
      "* example: modification 1. Issue #1.",
      "* example: add info. Issue #2. ticket1234"
    ),
    file.path(path, "NEWS.md")
  )
  add(repo, "NEWS.md")
  commit(repo, "Add NEWS\n\nissue #32.\nissue #1.\nissue#12\nticket6789.\nticket1234\n Creation of the NEWS file for version 0.1.")

  if (isTRUE(as.package)) {
    # Add a .gitignore
    writeLines(
      paste(".Rproj.user",
            sep = "\n"),
      file.path(path, ".gitignore")
    )
    ## Write a function in R/ and commit
    dir.create(file.path(path, "R"))
    writeLines(
      paste("#' Fake function",
            "#' @param x Vector of numeric",
            "my_mean <- function(x) {mean(x, na.rm = TRUE)}",
            sep = "\n"),
      file.path(path, "R/my_mean.R")
    )
    # Do not commit R file
    # add(repo, "R/my_mean.R")
    # commit(repo, "Add function my_mean")
    # Create a vignette directory
    dir.create(file.path(path, "vignettes"))
  }

  git2r::checkout(repo, branch = "main", create = TRUE)

  return(path)
}
