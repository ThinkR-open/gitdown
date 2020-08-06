#' Get time
#'
#' @param object hunks list
#'
#' @return time
get_time <- function(object) {
  pluck(object, "final_signature", "when", "time")
}

#' Get info
#'
#' Get the first and last modification for one file. Based on git2r::blame.
#'
#' @param path path to the file
#' @param repo repo of the git project
#'
#' @return list
#' @export
#'
#' @importFrom git2r blame
#' @importFrom purrr pluck map flatten_dbl set_names
#' @importFrom dplyr mutate pull
#'
#' @examples
#' repo <- fake_repo()
#' get_info(list.files(repo)[1], repo = repo)
get_info <- function(path, repo = ".") {

  # test if in git repo
  in_repository <- path %in%
    (git2r::ls_tree(repo = repo, recursive = TRUE) %>%
       mutate(filepath = paste0(path, name)) %>%
       pull(filepath))

  if (in_repository) {
    # In git
    blame_object <- blame(repo = repo, path = path)

    file <- path #basename(blame_object$hunks[[1]]$orig_path) # %>%
    # gsub(pattern = "^R/", replacement = "") %>%
    # gsub(pattern = "\\.R$", replacement = "")

    first_last <- blame_object %>%
      pluck("hunks") %>%
      map(get_time) %>%
      flatten_dbl() %>%
      range() %>%
      as.POSIXct.numeric(origin = "1970-01-01") %>%
      set_names(nm = c("first", "last"))
  } else {
    # Not in git repo
    file <- path
    first_last <- c(as.POSIXct.numeric(NA_real_),
                    file.info(file.path(repo, path))$mtime) %>%
      set_names(nm = c("first", "last"))
  }

  list(
    file = file,
    in_repository = in_repository,
    first_modif = first_last[1],
    last_modif = first_last[2]
  )
}

#' Get last modification of files
#'
#' @param repo git repository
#' @param path Default to R folder. Use "" for the complete repository.
#' @param recursive Logical. Should the listing recurse into directories?
#' @param untracked Logical. Should the untracked files be included?
#'
#' @return list
#' @export
#'
#' @importFrom purrr map
#'
#' @examples
#' repo <- fake_repo()
#' # Complete repository
#' get_last_modif(repo = repo, path = "")
#' repo <- fake_repo(as.package = TRUE)
#' # Complete repository
#' get_last_modif(repo = repo, path = "")
get_last_modif <- function(repo = ".", path = "R",
                           recursive = TRUE, untracked = TRUE) {

  folder <- normalizePath(file.path(repo, path), mustWork = FALSE)
  if (dir.exists(folder)) {
    if (path != "") {
      # One directory
      files <- git2r::ls_tree(repo = repo, recursive = recursive) %>%
        filter(path == paste0(!!path, "/")) %>%
        mutate(filepath = paste0(path, name)) %>%
        pull(filepath)
      if (isTRUE(untracked)) {
        not_in_git <- git2r::status(repo, all_untracked = TRUE) %>% unlist()
        files <- c(files, not_in_git[grepl(paste0(path, "/"), not_in_git)])
      }
    } else {
      # All files
      files <- git2r::ls_tree(repo = repo, recursive = recursive) %>%
        mutate(filepath = paste0(path, name)) %>%
        pull(filepath)
      if (isTRUE(untracked)) {
        not_in_git <- git2r::status(repo, all_untracked = TRUE) %>% unlist()
        files <- c(files, not_in_git[grepl(paste0(path, "/"), not_in_git)])
      }
    }
  } else {
    stop(path, "/ folder was not found")
  }
  if (length(files) == 0) {
    stop("There are no files to show. ",
         "Check the path, recursive and untracked parameters.")
  }
  map(files, ~ get_info(.x, repo = repo))
}


#' Formatting results of get_last_modif
#'
#' @inheritParams get_last_modif
#'
#' @importFrom dplyr transmute
#'
#' @return kable
#' @export
#'
#' @examples
#' repo <- fake_repo(as.package = TRUE)
#' cat(present_files(repo))
present_files <- function(repo = ".", path = "R",
                          recursive = TRUE, untracked = TRUE) {

  get_last_modif(repo, path, recursive, untracked) %>%
  purrr::map_dfr(as_tibble) %>%
    transmute(
      File = file,
      `Tracked in git` = ifelse(in_repository, "Yes", "No"),
      `Date of creation` = first_modif,
      `Last modification` = last_modif
    ) %>%
    knitr::kable(., format = "markdown") %>%
    paste(., collapse = "  \n")

  # as.character(
  #   htmltools::tags$ul(
  #     lapply(
  #       , function(x) {
  #         htmltools::tags$li(
  #           htmltools::tags$h4(paste("File:", x$file)),
  #           htmltools::tags$p(paste("Date of creation:", x$first_modif)),
  #           htmltools::tags$p(paste("Last modification:", x$last_modif)),
  #           htmltools::tags$p("")
  #         )
  #       })
  #   )
  # )
}


#' Update vignette last modification of files
#'
#' @inheritParams get_last_modif
#'
#' @return update existing vignette
#' @export
#' @examples
#' repo <- fake_repo(as.package = TRUE)
#' update_vignette_last_modif(repo, path = "R")
#' rmarkdown::render(file.path(repo, "vignettes", "modification_files.Rmd"))
update_vignette_last_modif <- function(repo = ".", path = "R",
                                       recursive = TRUE, untracked = TRUE) {
  vig <- file.path(repo, "vignettes")
  file <- file.path(vig, "modification_files.Rmd")
  if (file.exists(file)) {
    unlink(file)
  }

  path_to_copy <- system.file("template/modification_files.Rmd", package = "gitdown")

  file.copy(path_to_copy, to = vig)

  if (file.exists(file)) {
    md <- c(present_files(repo, path, recursive, untracked), "\n\n")
    write(md, file = file, append = TRUE)
  } else {
    stop("Copying the file didn't work!")
  }
}

#' Create the vignette for last modification
#'
#' @inheritParams get_last_modif
#'
#'
#' @return copy a vignette
#' @export
#'
#' @examples
#' repo <- fake_repo(as.package = TRUE)
#' create_vignette_last_modif(repo, path = "R")

create_vignette_last_modif <- function(repo = ".", path = "R",
                                       recursive = TRUE, untracked = TRUE) {
  vig <- file.path(repo, "vignettes")

  if (!dir.exists(vig)) {
    stop("vignettes folder doesn't exist, please create vignettes folder")
  } else {
    update_vignette_last_modif(repo, path, recursive, untracked)
  }
}
