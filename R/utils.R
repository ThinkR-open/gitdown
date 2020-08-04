#' Replace in file
#'
#' @param file Path of file
#' @param pattern Pattern to replace
#' @param replacement A character vector of replacements
#'
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
#'
write_in <- function(x, repo, dir =  "gitdown", rmd = "index.Rmd") {
  dir_out <- normalizePath(file.path(repo, dir))
  if (!dir.exists(dir_out)) {dir.create(dir_out)}
  there <- file.path(dir_out, rmd)
  write(x, file = there, append = TRUE)
}

#' Presentation of commit
#'
#' @param commit one line data from get_commits_pattern

presentation_commit <- function(commit) {
  res <- paste0(
    paste0("### commit: ", commit$summary, "{#", commit$link_commit, "}"),
    "\n\n",
    paste("- Commit number:", commit$sha),
    "\n",
    paste("- Author:", commit$author, "; E-mail:", commit$email),
    "\n",
    paste("- Date:", commit$when),
    "\n",
    paste("- Content:\n"),
    "```",
    "\n",
    commit$message,
    "\n",
    "```"
  )
}

#' Write presentation for each commit
#'
#' @param commits table of commits of one pattern as issued from nest_commits_by_pattern
#' @param name_section character. Name of the section
#' @param pattern_name Name of the pattern selected
#' @param link_pattern Name of the pattern selected transformed as slug
#'
#' @importFrom purrr map_chr
#'

each_commit <- function(commits, pattern_name, link_pattern, name_section) {
  c(
    # One Pattern Section
    paste0("## ", to_singular(name_section), ": ",
           pattern_name, "{#", link_pattern, "}"),
    # Commits
    map_chr(1:nrow(commits), ~presentation_commit(commits[.x, ]))
  )
  # repo <- fake_repo()
  # one_pattern_commits <- nest_commits_by_pattern(repo, pattern = "ticket[[:digit:]]+",
  # ref = "master", silent = TRUE)[1,]
  # each_commit(one_pattern_commits, name_section = "Ticket")
}

#' To singular
#' @param x Character
to_singular <- function(x) {
  gsub("s$", "", x)
}

#' Presentation for each pattern
#'
#' @param pattern vector of regex patterns
#' @param name_section names for pattern section
#' @param repo path to the repo
#' @param ref branch of the repo
#'
#' @importFrom dplyr tibble mutate bind_rows
#' @importFrom purrr map_chr
#' @export
#' @examples
#' repo <- fake_repo()
#' each_pattern("ticket[[:digit:]]+", "Ticket", repo)

each_pattern <- function(pattern, name_section, repo, ref = "master") {

  # Create text of each commit by pattern
  res_commits <- nest_commits_by_pattern(
    repo,
    pattern = pattern,
    ref = ref, silent = TRUE
  ) %>%
    mutate(
      text = map(data, ~each_commit(.x, pattern_name, link_pattern, name_section))
    )

  # Create summary table
  summary_table <- tibble(
    pattern_name = res_commits$pattern_name,
    Links = map_chr(
      res_commits$data,
      ~paste0("[", .x$summary, "](#", .x$link_commit, ")") %>%
        paste(collapse = ", "))
  )
  names(summary_table)[1] <- name_section

  # Create text for section
  text_section <- paste0(
    paste0("# Section: ", name_section, "\n"),
    "## Summary table\n\n",
    paste(knitr::kable(summary_table, format = "markdown"),
          collapse = "\n")
  )

  res <- bind_rows(
    tibble(
      pattern_name = name_section,
      link_pattern = clean_link(name_section),
      text = list(text_section)
    ),
    res_commits
  )

  res
}

#' Nest all commits by each pattern
#'
#' @inheritParams get_commits_pattern
#' @importFrom dplyr filter mutate arrange group_by
#' @importFrom stringr str_extract_all
#' @importFrom tidyr nest
#'
#' @export
#' @examples
#' repo <- fake_repo()
#' nest_commits_by_pattern(repo)
nest_commits_by_pattern <- function(repo, pattern = "#[[:digit:]]+",
                                    ref = "master", silent = TRUE) {

  get_commits_pattern(repo, pattern = pattern,
                      ref = ref, silent = silent) %>%
    mutate(
      pattern_numeric =
        str_extract_all(pattern, "([[:digit:]]+)",
                        simplify = FALSE) %>%
        unlist() %>%
        as.numeric(),
      pattern_name = pattern,
      link_pattern = clean_link(pattern_name),
      link_commit = paste0(link_pattern, "-", sha)) %>%
    arrange(pattern_numeric, pattern_name, order) %>%
    # arrange(pattern_name, order) %>%
    # If is.na => New part
    filter(!is.na(pattern_name)) %>%
    group_by(pattern_numeric, pattern_name, link_pattern) %>%
    # group_by(pattern_name, link_pattern) %>%
    nest()
}

#' Clean link
#' @param x Character to clean to transform as slug

clean_link <- function(x) {
  paste0("a", x) %>% # First must be a text
  gsub("^_|^ | $|_$|#|@|`|\\\\|/", "", .) %>%
  gsub("[[:space:]]+|_+", "-", .) %>%
  gsub("(-)+", "-", .) %>%
  gsub("^(-)|(-)$", "", .) %>%
  tolower(.)
  # clean_link(" -Text With # \\\\ \ / _characters_ ")
}

