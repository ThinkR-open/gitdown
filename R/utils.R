#' Replace in file
#'
#' @param file Path of file
#' @param pattern Pattern to replace
#' @param replacement A character vector of replacements
#'
replace_in_file <- function(file, pattern, replacement){
  a <- readLines(file)
  a <- gsub(pattern, replacement, a)
  write(a, file)
}

#' Write inside file
#'
#' @param x what
#' @param dir by deflaut gitbook
#' @param rmd wich file
#' @param repo where
#'
write_in <- function(x, repo, dir =  "gitdown", rmd = "index.Rmd"){
  there <- file.path(repo, dir, rmd)
  write(x, file = there, append = TRUE)
}

#' Presentation of commit
#'
#' @param data data from get_commits_pattern
#' @param i line number to present the commit
#' @param write_in function to write in good place

presentation_commit <- function(write_in, data, i) {
  data <- data[ i ,]
  write_in("\n")
  write_in("```")
  write_in(data$sha)
  write_in("\n")
  write_in(paste("Author: ", data$author, "   E-mail: ", data$email))
  write_in("\n")
  write_in(paste("Date: ", data$when))
  write_in("\n")
  write_in(data$message)
  write_in("\n")
  write_in("```")
}

#' Write presentation for each commit
#'
#' @param data list with pattern and data elements
#' @param write_in function to write in good place
#' @importFrom purrr walk

each_commit <- function(data, write_in){
  write_in("\n")
  write_in(paste("## `", data[["pattern"]],"`"))
  commits <- data[["data"]]
  walk(1:nrow(commits), ~ presentation_commit(write_in, commits, .x ))
}

#' Presentation for each pattern
#'
#' @param vec_pattern vector of pattern
#' @param names_section names for pattern section
#' @param repo path to the repo
#' @param ref branch of the repo
#' @param write_in function to write in good place
#'
#' @importFrom purrr transpose set_names walk
#' @importFrom dplyr filter arrange group_by mutate
#' @importFrom stringr str_extract_all
#' @importFrom tidyr nest
#'

each_pattern <- function(vec_pattern, names_section, repo, ref = "master", write_in){
  write_in("\n")
  write_in("\n")
  write_in(paste("# Section:", names_section))
  commits <- get_commits_pattern(repo, pattern = vec_pattern,
                                 ref = ref, silent = TRUE) %>%
    mutate(pattern_numeric =
             str_extract_all(pattern, "([[:digit:]]+)",
                             simplify = FALSE) %>%
             unlist() %>%
             as.numeric()) %>%
    arrange(pattern_numeric, pattern, order) %>%
    dplyr::filter(!is.na(pattern)) %>%
    group_by(pattern_numeric, pattern) %>%
    nest() %>%
    arrange(pattern_numeric, pattern) %>%
    transpose() %>%
    set_names(.$pattern)

  walk(commits, ~each_commit(.x, write_in))
}
