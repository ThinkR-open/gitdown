#' Replace in file
#'
#' @param file Path of file
#' @param pattern Pattern to replace
#' @param replacement A character vector of replacements
#' @noRd
replace_in_file <- function(file, pattern, replacement) {
  a <- readLines(file)
  a <- gsub(pattern, replacement, a)
  write(a, file)
}

#' Write inside file
#'
#' @param x what text to write
#' @param dir Name of the dir where to write file
#' @param rmd name and type of the file to write in
#' @param repo Path to directory where to store "dir"
#' @noRd
write_in <- function(x, repo, dir =  "gitdown", rmd = "index.Rmd") {
  dir_out <- normalizePath(file.path(repo, dir))
  if (!dir.exists(dir_out)) {dir.create(dir_out)}
  there <- file.path(dir_out, rmd)
  write(enc2utf8(x), file = there, append = TRUE)
}

#' Presentation of commit
#'
#' @param commit one line data from get_commits_pattern
#' @noRd

presentation_commit <- function(commit) {
  res <- paste0(
    paste0("### commit: ", clean_text(commit$summary), "{#", commit$link_commit, "}"),
    "\n\n",
    paste("- Commit number:", commit$sha),
    "\n",
    paste("- Author:", commit$author, "; E-mail:", commit$email),
    "\n",
    paste("- Date:", commit$when),
    "\n",
    paste("- Message content:"),
    "\n\n",
    "```",
    "\n",
    paste(commit$message),
    "\n\n",
    "```",
    "\n",
    paste("- Related patterns:\n"),
    # "```{block, type='commit-content'}",
    # "\n",
    paste(commit$message_link)
    # "\n",
    # "```"
  )
}

#' Write presentation for each commit
#'
#' @param commits table of commits of one pattern as issued from nest_commits_by_pattern
#' @param pattern.type character. Name of the section
#' @param pattern.content Name of the pattern selected
#' @param link_pattern Name of the pattern selected transformed as slug
#' @param pattern.title Title of the pattern selected
#'
#' @importFrom purrr map_chr
#'
#' @noRd

each_commit <- function(commits, pattern.content, link_pattern, pattern.type, pattern.title) {
  # if no commits
  if (length(commits$sha) == 1 && is.na(commits$sha)) {
    c(
      # One Pattern Section
      paste0("## ", to_singular(pattern.type), ": ",
             pattern.title, "{#", link_pattern, "}"),
      # Commits
      "*No commits*"
    )
  } else {
    c(
      # One Pattern Section
      paste0("## ", to_singular(pattern.type), ": ",
             pattern.title, "{#", link_pattern, "}"),
      # Commits
      map_chr(1:nrow(commits), ~presentation_commit(commits[.x, ]))
    )
  }
}

#' To singular
#' @param x Character
#' @noRd
to_singular <- function(x) {
  gsub("s$", "", x)
}

#' Nest all commits by each pattern found in the commit messages
#'
#' @inheritParams get_commits_pattern
#' @importFrom dplyr filter mutate arrange group_by if_else
#' @importFrom dplyr ungroup distinct select summarise left_join
#' @importFrom stringi stri_extract_all
#' @importFrom tidyr nest
#'
#' @return A tibble with a row for each different pattern found in commit messages
#' and following columns:
#'
#' - pattern.type: name of the pattern found in the commit message
#' - pattern.content: pattern found in the commit message
#' - pattern.title: pattern.content or title used in the pattern.table
#' a nested 'data' column with all related commits
#' - pattern_numeric: extraction of numeric value in pattern.content
#' - link_pattern: internal url of the pattern in the future HTML gitbook
#' - data: a nested list of tibbles with commits content as issued from [get_commits_pattern()]
#'
#' @export
#' @examples
#' repo <- fake_repo()
#' nest_commits_by_pattern(repo)
#'
#' # With table of correspondence
#' pattern.table <- data.frame(
#'   number = c("#2", "#1", "#1000"),
#'   title = c("#2 A second issue to illustrate a blog post",
#'             "#1 An example of issue",
#'             "#1000 issue with no commit"))
#'  nest_commits_by_pattern(
#'   repo,
#'   pattern.table = pattern.table,
#'   pattern = c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"))

nest_commits_by_pattern <- function(repo,
                                    pattern = c("Issues" = "#[[:digit:]]+"),
                                    pattern.table = NULL,
                                    ref = "main", silent = TRUE) {

  res <- get_commits_pattern(repo, pattern = pattern,
                             pattern.table = pattern.table,
                             ref = ref, silent = silent) %>%
    mutate(
      pattern_numeric =
        stri_extract_all(pattern.content, regex = "([[:digit:]]+)",
                         simplify = FALSE) %>%
        unlist() %>%
        as.numeric(),
      pattern.type = clean_text(pattern.type),
      pattern.content = clean_text(pattern.content),
      pattern.title = clean_text(pattern.title),
      link_pattern = clean_link(paste(pattern.type, pattern.content)),
      link_commit = paste0(link_pattern, "-", sha)
    )

  correspondence <- res %>%
    select(sha, pattern.type, pattern.title, link_pattern) %>%
    distinct() %>%
    # filter(!is.na(pattern.title)) %>%
    mutate(pattern.title = if_else(
      is.na(pattern.title),
      paste("No related", tolower(pattern.type)), pattern.title)) %>%
    mutate(
      text_link = paste0("[", pattern.title, "](#", link_pattern, ")")
    ) %>%
    # Longest chain to smallest
    group_by(sha, pattern.type) %>%
    summarise(
      message_link.type = paste(text_link, collapse = ", ")
    ) %>%
    group_by(sha) %>%
    summarise(
      message_link = paste(paste0("  + ", pattern.type, ": ", message_link.type),
                           collapse = "  \n")
    ) %>%
    ungroup()

  res %>%
    left_join(correspondence, by = "sha") %>%
    arrange(pattern.type, pattern_numeric, pattern.content, order) %>%
    mutate(pattern.title = if_else(is.na(pattern.title), paste("No related", tolower(pattern.type)), pattern.title)) %>%
    mutate(pattern.content = if_else(is.na(pattern.content), paste("No related", tolower(pattern.type)), pattern.content)) %>%
    group_by(pattern.type, pattern_numeric, pattern.content, pattern.title, link_pattern) %>%
    nest() %>%
    ungroup()
}


#' Create the text to add in a markdown file to present each pattern as a chapter of the book
#'
#' @param nest_commits commits as nested with nest_commits_by_pattern
#' @param pattern.type Character name of the pattern to filter
#'
#' @importFrom dplyr tibble mutate bind_rows filter
#' @importFrom purrr map_chr pmap
#' @export
#' @return A tibble with a row for each different pattern found and
#' a 'text' column to be included in a markdown file:
#'
#' - pattern.content: pattern found in the commit message
#' - link_pattern: internal url of the pattern in the future HTML gitbook
#' - text: list of vectors of markdown text to present commits of each pattern
#' in the HTML gitbook output
#' - pattern.type: name of the pattern found in the commit message
#' - pattern.title: pattern.content or title used in the pattern.table
#' a nested 'data' column with all related commits
#' - pattern_numeric: extraction of numeric value in pattern.content
#' - data: a nested list of tibbles with commits content as issued from [get_commits_pattern()]
#'
#' @examples
#' repo <- fake_repo()
#' res_commits <- nest_commits_by_pattern(
#'   repo,
#'   pattern = c("Tickets" = "ticket[[:digit:]]+"),
#'   ref = "main", silent = TRUE
#' )
#' each_pattern(res_commits, "Tickets")

each_pattern <- function(nest_commits, pattern.type) {

  filter_commits <- nest_commits %>%
    filter(pattern.type == clean_text(!!pattern.type))

  # Create text of each commit by pattern
  res_commits <- filter_commits %>%
    mutate(
      text = pmap(list(data, pattern.content, link_pattern, pattern.type, pattern.title),
                  function(commits, pattern.content, link_pattern, pattern.type, pattern.title)
                    each_commit(commits, pattern.content, link_pattern, pattern.type, pattern.title))
    )

  # Create summary table
  summary_table <- tibble(
    pattern.content = res_commits$pattern.title,
    `Commits links` = map_chr(
      res_commits$data,
      ~ifelse(length(.x$sha) == 1 && is.na(.x$sha),
              # no commits
              "No commits",
              # commits
              paste0("[", clean_text(.x$summary), "](#", .x$link_commit, ")") %>%
                paste(collapse = ", ")
      )
    )
  )
  names(summary_table)[1] <- pattern.type

  # Create text for section
  text_section <- paste0(
    paste0("# Section: ", pattern.type, "\n"),
    "## Summary table\n\n",
    paste(knitr::kable(summary_table, format = "markdown"),
          collapse = "\n")
  )

  res <- bind_rows(
    tibble(
      pattern.content = pattern.type,
      link_pattern = clean_link(pattern.type),
      text = list(text_section)
    ),
    res_commits
  )

  res
}

#' Clean link
#' @param x Character to clean to transform as slug
#' @noRd

clean_link <- function(x) {
  x %>%
    clean_text() %>%
    # Clean all special characters
    gsub("\\W", "-", .) %>%
    gsub("(-)+", "-", .) %>%
    gsub("^(-)|(-)$", "", .) %>%
    # Remove all spaces and digits at the beginning
    gsub("^(_*|-*|\\s*|\\d*)+", "", .) %>%
    tolower(.)
}

#' Clean text in titles
#'
#' Removes stars, underscores, \{\} and []
#' @param x Character to clean to transform as slug
#' @noRd

clean_text <- function(x) {
  x %>%
    gsub("\\*|\\{|\\}", "", .) %>%
    gsub("_", "-", .) %>%
    gsub("[", "-", ., fixed = TRUE) %>%
    gsub("]", "-", ., fixed = TRUE)
}
