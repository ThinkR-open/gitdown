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
    paste0("### commit: ", clean_text(commit$summary), "{#", commit$link_commit, "}"),
    "\n\n",
    paste("- Commit number:", commit$sha),
    "\n",
    paste("- Author:", commit$author, "; E-mail:", commit$email),
    "\n",
    paste("- Date:", commit$when),
    "\n",
    paste("- Message content:"),
    "\n",
    "```",
    "\n",
    paste(commit$message),
    "\n",
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

each_commit <- function(commits, pattern.content, link_pattern, pattern.type, pattern.title) {
  c(
    # One Pattern Section
    paste0("## ", to_singular(pattern.type), ": ",
           pattern.title, "{#", link_pattern, "}"),
    # Commits
    map_chr(1:nrow(commits), ~presentation_commit(commits[.x, ]))
  )
  # repo <- fake_repo()
  # one_pattern_commits <- nest_commits_by_pattern(repo, pattern = "ticket[[:digit:]]+",
  # ref = "master", silent = TRUE)[1,]
  # each_commit(one_pattern_commits, pattern.type = "Ticket")
}

#' To singular
#' @param x Character
to_singular <- function(x) {
  gsub("s$", "", x)
}

#' Presentation for each pattern
#'
#' @param nest_commits commits as nested with nest_commits_by_pattern
#' @param pattern.type Character name of the pattern to filter
#'
#' @importFrom dplyr tibble mutate bind_rows filter
#' @importFrom purrr map_chr pmap
#' @export
#' @examples
#' repo <- fake_repo()
#' res_commits <- nest_commits_by_pattern(
#'   repo,
#'   pattern = c("Ticket" = "ticket[[:digit:]]+"),
#'   ref = "master", silent = TRUE
#' )
#' each_pattern(res_commits, "Ticket")

each_pattern <- function(nest_commits, pattern.type) {

  filter_commits <- nest_commits %>%
    filter(pattern.type == !!pattern.type)

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
      ~paste0("[", clean_text(.x$summary), "](#", .x$link_commit, ")") %>%
        paste(collapse = ", "))
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

#' Nest all commits by each pattern
#'
#' @inheritParams get_commits_pattern
#' @importFrom dplyr filter mutate arrange group_by if_else
#' @importFrom dplyr ungroup distinct select summarise left_join
#' @importFrom stringi stri_extract_all
#' @importFrom tidyr nest
#'
#' @export
#' @examples
#' repo <- fake_repo()
#' nest_commits_by_pattern(repo, pattern = )

nest_commits_by_pattern <- function(repo,
                                    pattern = c("Issues" = "#[[:digit:]]+"),
                                    pattern.table = NULL,
                                    ref = "master", silent = TRUE) {

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

  correspondance <- res %>%
    select(sha, pattern.type, pattern.title, link_pattern) %>%
    distinct() %>%
    # filter(!is.na(pattern.title)) %>%
    mutate(pattern.title = if_else(
      is.na(pattern.title),
      paste("No related", tolower(pattern.type)), pattern.title)) %>%
    mutate(
      # pattern.title.regex = paste0("(?<!\\[)", pattern.title),
      # text_inter = paste0("-[", pattern.title, "]-"),
      text_link = paste0("[", pattern.title, "](#", link_pattern, ")")
    ) %>%
    # Longest chain to smallest
    # arrange(desc(nchar(pattern.title))) %>%
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

  # c("# text #1\n issue#1\n#2  #145  #") %>%
  # c("toto") %>%
  # stringi::stri_replace_all_regex(
  #   pattern = correspondance$pattern.content.regex,
  #   replacement = correspondance$text_inter,
  #   vectorize_all = FALSE) %>%
  # stringi::stri_extract_all_fixed(
  #   pattern = correspondance$text_inter,
  #   omit_no_match = TRUE
  # ) %>% unlist() %>% unique() %>%
  # # Replace with url
  # stringi::stri_replace_all_fixed(
  #   pattern = correspondance$text_inter,
  #   replacement = correspondance$text_link,
  #   vectorize_all = FALSE)# %>%
  # stringi::stri_replace_all_regex(replacement = "\\\\#", pattern = "^(#)") %>%
  # stringi::stri_replace_all_regex(replacement = "\\\n\\\n", pattern = "\\n")

  res %>%
    # rowwise() %>%
    left_join(correspondance, by = "sha") %>%
    # mutate(
    #   message_link =
    #     # Replace with url
    #     stringi::stri_replace_all_regex(
    #       message,
    #       pattern = correspondance$pattern.content.regex,
    #       replacement = correspondance$text_link,
    #       vectorize_all = FALSE
    #       ) %>%
    #     stringi::stri_replace_all_regex(replacement = "\\\\#", pattern = "^(#)") %>%
    #     # Go to newline
  #     stringi::stri_replace_all_regex(replacement = "  \\\n", pattern = "\\n")
  # ) %>%
  arrange(pattern.type, pattern_numeric, pattern.content, order) %>%
    # If is.na => New part
    # filter(!is.na(pattern.content)) %>%
    mutate(pattern.title = if_else(is.na(pattern.title), paste("No related", tolower(pattern.type)), pattern.title)) %>%
    mutate(pattern.content = if_else(is.na(pattern.content), paste("No related", tolower(pattern.type)), pattern.content)) %>%
    group_by(pattern.type, pattern_numeric, pattern.content, pattern.title, link_pattern) %>%
    nest() %>%
    ungroup()
}

#' Clean link
#' @param x Character to clean to transform as slug

clean_link <- function(x) {
  x %>%
    clean_text() %>%
    # Clean all special characters
    # gsub("^_|^ | $|_$|#|@|'|`|\\$|\\\\|/|\\[|\\]|\\(|\\)|\\{|\\}|\"|\\+|:", "", .) %>%
    gsub("\\W", "-", .) %>%
    # remove duplicate space (underscore kept)
    # gsub("[[:space:]]+|_+", "-", .) %>%
    # gsub("[[:space:]]+", "-", .) %>%
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

clean_text <- function(x) {
  x %>%
    gsub("_|\\*|\\{|\\}", "", .) %>%
    gsub("[", "-", ., fixed = TRUE) %>%
    gsub("]", "-", ., fixed = TRUE)
}
