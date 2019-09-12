#' Create a fake git repository in tempfile for examples
#'
#' @param path Path to fake repository
#'
#' @importFrom git2r init config add commit tag
#' @export
#'
#' @return path to fake repository
#' @examples
#' fake_repo()
fake_repo <- function(path = tempfile(pattern = "git2r-")) {
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
  commit(repo, "Third commit message\n\nissue #2\nissue #145.\nticket1234\n More information important for the project as breaking changes")

  ## Create a tag
  tag(repo, "v0.1", "Tag v0.1 message")

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
  commit(repo, "Add NEWS\n\nissue #32.\nissue #1.\nticket6789.\nticket1234\n Creation of the NEWS file for version 0.1.")

  return(path)
}
