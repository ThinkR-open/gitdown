#' Get commits associated chronologically with tags
#'
#' @inheritParams git2r::commits
#' @param silent Logical. Whether to hide messages.
#'
#' @return A tibble with one line for each commit and the following columns:
#'
#' - sha: sha of the commit
#' - summary: First line of the commit message
#' - message: Full content of the commit message
#' - author: author of the commit
#' - email: email of the author
#' - when: commit time
#' - order: order of commit messages. 1 is the oldest.
#' - tag.name: name of tag associated with all commits since the last tag
#' - tag.message: message of the tagged commit
#'
#' @importFrom git2r commits tags
#' @importFrom purrr map_dfr flatten
#' @importFrom dplyr mutate n as_tibble select rename left_join arrange desc tibble
#' @importFrom tidyr fill
#'
#' @export
#'
#' @examples
#' repo <- fake_repo()
#' get_commits_tags(repo = repo)
get_commits_tags <- function(repo = ".", ref = "master",
                             path = NULL, silent = FALSE) {
  # checkout(repo, "master")
  # Get commits
  all_commits <- commits(
    repo = repo, ref = ref,
    topological = TRUE, time = TRUE, reverse = FALSE
  ) %>%
    map_dfr(as_tibble) %>%
    mutate(order = rev(1:n()))

  if (!isTRUE(silent)) {message(nrow(all_commits), " commits found.")}

  # Get all tags
  list_tags <- tags(repo)
  if (length(list_tags) == 0) {
    if (!isTRUE(silent)) {message("No tag found in this repository.")}
    all_tags <- tibble(sha = NA_character_, tag.name = NA_character_, tag.message = NA_character_)
  } else {
    #TODO change for modify from purrr
    for(i in names(list_tags)){
      list_tags[[i]][["name"]] <- i
    }
    all_tags <- do.call(rbind, lapply(list_tags, function(x) unlist(x))) %>%
      as_tibble()
    if("target" %in% names(all_tags)){
      all_tags <- all_tags %>%
        select(name, message, target) %>%
        rename(sha = target, tag.name = name, tag.message = message)
    }else{
      all_tags <- all_tags %>%
        select(name,message,sha) %>%
        rename( tag.name = name, tag.message = message)
    }
  }

  # Associate tags with commits
  all_commits %>%
    left_join(all_tags, by = "sha") %>%
    # Fill tag.name
    arrange(desc(order)) %>%
    fill(tag.name, tag.message)
}

#' Get commits associated with a text pattern
#'
#' @param pattern Named vector with regex pattern to expose commits, like `c("Issues" = "#\[\[:digit:\]\]")` for issues
#' @param pattern.table data.frame with two columns: pattern and description of the pattern.
#' This is used as correspondence table to add some names to existing patterns.
#' @inheritParams git2r::commits
#' @inheritParams get_commits_tags
#'
#' @return A tibble with one line for each commit, duplicated if
#'  associated with multiple patterns and the following columns:
#'
#' - sha: sha of the commit
#' - summary: First line of the commit message
#' - message: Full content of the commit message
#' - author: author of the commit
#' - email: email of the author
#' - when: commit time
#' - order: order of commit messages. 1 is the oldest.
#' - tag.name: name of tag associated with all commits since the last tag
#' - tag.message: message of the tagged commit
#' - pattern.type: name of the pattern found in the commit message
#' - pattern.content: pattern found in the commit message
#'
#' @importFrom dplyr mutate distinct rowwise tibble left_join mutate
#' @importFrom dplyr if_else mutate_all filter select
#' @importFrom tidyr unnest
#' @importFrom purrr imap flatten_chr
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' repo <- fake_repo()
#' get_commits_pattern(repo = repo, pattern = c("Ticket" = "#[[:digit:]]+"))
#' get_commits_pattern(repo = repo,
#'   pattern = c("Ticket" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"))

get_commits_pattern <- function(repo = ".", pattern = c("Ticket" = "#[[:digit:]]+"),
                                pattern.table = NULL,
                                ref = "master", path = NULL, silent = FALSE) {

  if (is.null(names(pattern))) {names(pattern) <- paste0("`", pattern, "`")}

  res <- get_commits_tags(repo = repo,  ref = ref, path = path, silent = silent) %>%
    rowwise() %>%
    mutate(
      pattern_extract = list(
        imap(pattern, ~my_extract(message, .x) %>%
               setNames(., rep(.y, length(.)))) %>%
          flatten_chr() %>%
          # {} are needed otherwise, there is a "." column
          {tibble(pattern.type = names(.), pattern.content = unname(.))}
      )
    ) %>%
    # unnest to separate multiple pattern values
    unnest(pattern_extract) %>%
    # remove duplicates
    distinct()

  if (!is.null(pattern.table)) {
    if (isTRUE(ncol(pattern.table) == 1)) {
      stop("pattern.table should be a data.frame with 2 columns, one with pattern, the second with title.")
    } else if (ncol(pattern.table) > 2) {
      pattern.table <- pattern.table[,1:2]
      warning("Only the first two columns will be used, first one as pattern, second on as title")
    }
    names(pattern.table) <- c("pattern.content.orig", "pattern.title")
    pattern.table.content <- pattern.table %>%
      rowwise() %>%
      mutate(
        pattern_extract = list(
          imap(pattern, ~my_extract(pattern.content.orig, .x) %>%
                 setNames(., rep(.y, length(.)))) %>%
            flatten_chr() %>%
            # {} are needed otherwise, there is a "." column
            {tibble(pattern.type = names(.), pattern.content = unname(.))}
        )
      ) %>%
      # unnest to separate multiple pattern values
      unnest(pattern_extract) %>%
      filter(!is.na(pattern.content)) %>%
      # remove duplicates
      distinct() %>%
      select(-pattern.content.orig)

    res %>%
      dplyr::full_join(pattern.table.content %>% mutate_all(as.character),
                       by = c("pattern.type", "pattern.content")) %>%
      mutate(pattern.title = if_else(is.na(pattern.title),
                                     pattern.content, pattern.title))
  } else {
    res %>%
      mutate(pattern.title = pattern.content)
  }


}

#' Extract pattern from message
#'
#' @param message Character
#' @inheritParams stringi::stri_extract_all
#'
#' @importFrom stringi stri_extract_all
#' @noRd

my_extract <- function(message, pattern) {
  # res <- unlist(str_extract_all(message, pattern, simplify = FALSE))
  res <- unlist(stri_extract_all(message, regex = pattern, simplify = FALSE))
  if(length(res) == 0) NA_character_ else res
}
