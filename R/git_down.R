#' Turns the active branch history of git into a bookdown.
#'
#' Read all commit messages of your local git repository and
#' sort them according to tags or specific text pattern into chapters of
#' a HTML book using 'bookdown'.
#' Each chapter is a group of commits. The first page gives a summary of all the groups.
#'
#' @param repo The path to a repository. Default is `.`
#' @param book_path The path to the bookdown output. Default is `"gitdown"`.
#' @param open Should the bookdown be opened once compiled? Default is TRUE.
#' @param author Author of the bookdown
#' @param ref the name of the branch, by default main
#' @param ... Other parameters to pass to [rmarkdown::render()]
#'
#' @inheritParams get_commits_pattern
#' @export
#'
#' @return Path of the HTML gitbook saved in the repo/book_path directory.
#'
#' @importFrom attempt if_not
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @importFrom git2r workdir
#' @importFrom purrr map_dfr
#' @importFrom bookdown gitbook
#'
#' @examples
#' repo <- fake_repo()
#' if (rmarkdown::pandoc_available("2.0.0")) {
#'   res <- git_down(repo, pattern = c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"),
#'     open = FALSE)
#' }
#' \dontrun{
#' # Open the book
#'   browseURL(res)
#' }
#' # With table of correspondence
#' pattern.table <- data.frame(number = c("#2", "#1"),
#'   title = c("#2 A second issue to illustrate a blog post",
#'                        "#1 An example of issue"))
#' if (rmarkdown::pandoc_available("2.0.0")) {
#'   res <- git_down(repo, pattern = c("Issues" = "#[[:digit:]]+"),
#'     pattern.table = pattern.table, open = FALSE)
#' }
#' \dontrun{
#' # Open the book
#'   browseURL(res)
#' }

git_down <- function(repo = ".", book_path = "gitdown",
                     open = TRUE, author = "John Doe",
                     pattern = c("Issues" = "#[[:digit:]]+"),
                     pattern.table = NULL,
                     ref = "main", ...) {

  # Clean previous book
  unlink(file.path(repo, book_path), recursive = TRUE)
  if_not(
    file.path(repo, book_path),
    dir.exists,
    ~ dir.create(file.path(repo, book_path),recursive = TRUE)
  )
  file.copy(from = list.files(system.file("booktemplate", package = "gitdown"), full.names = TRUE),
            to = normalizePath(file.path(repo, book_path)))

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

  meta_name <- basename(workdir(repo = repo))

  # Link commits and patterns
  if (is.null(names(pattern))) {names(pattern) <- paste0("`", pattern, "`")}
  res_commits <- nest_commits_by_pattern(
    repo,
    pattern = pattern,
    pattern.table = pattern.table,
    ref = ref, silent = TRUE
  )

  # Create content
  res <- map_dfr(
    names(pattern),
    ~each_pattern(res_commits, pattern.type = .x))

  content <- paste(
    c(
      # presentation
      paste0("# Gitbook for ", meta_name,"{-} \n"),
      paste0("Done on: ", Sys.time(),"\n"),
      "\n",
      # All commits by Pattern
      unlist(res$text)
    ),
    collapse = "\n\n"
  )

  write_in(x = content,
           repo = repo,
           dir = book_path,
           rmd = "index.Rmd")

  res <- render(file.path(repo, book_path, "index.Rmd"), ...)

  if (isTRUE(open)) {
    browseURL(res)
  }
  res
}
