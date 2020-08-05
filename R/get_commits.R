#' Get commits associated chronologically with tags
#'
#' @inheritParams git2r::commits
#' @param silent Logical. Whether to hide messages.
#'
#' @return A tibble with commits and tags
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
#' @param pattern Named vector of regex patterns
#' @inheritParams git2r::commits
#' @inheritParams get_commits_tags
#'
#' @return A tibble with commits and tags
#'
#' @importFrom dplyr mutate distinct rowwise tibble
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
                                ref = "master", path = NULL, silent = FALSE) {

  if (is.null(names(pattern))) {names(pattern) <- paste0("`", pattern, "`")}

 get_commits_tags(repo = repo,  ref = ref, path = path, silent = silent) %>%
    rowwise() %>%
    mutate(
      pattern_extract = list(
        imap(pattern, ~my_extract(message, .x) %>%
               setNames(., rep(.y, length(.)))) %>%
          flatten_chr() %>%
          tibble(pattern.type = names(.), pattern.content = .)
        )
    ) %>%
    # unnest to separate multiple pattern values
    unnest(pattern_extract) %>%
    # remove duplicates
    distinct()
}

#' Extract pattern from message
#'
#' @param message Character
#' @inheritParams stringi::stri_extract_all
#'
#' @importFrom stringi stri_extract_all

my_extract <- function(message, pattern) {
  # res <- unlist(str_extract_all(message, pattern, simplify = FALSE))
  res <- unlist(stri_extract_all(message, regex = pattern, simplify = FALSE))
  if(length(res) == 0) NA_character_ else res
}
