#' Get time
#'
#' @param object hunks list
#'
#' @return time
#' @noRd
get_time <- function(object) {
  pluck(object, "final_signature", "when", "time")
}

#' Get the first and last modification time for a specific file, based on git2r::blame().
#'
#' @param path path to the file
#' @param repo repo of the git project
#'
#' @return A list with information of the selected file:
#'
#' - file: file name
#' - in_repository: Logical. Whether the file has already been commit once in the
#' git repository
#' - first_modif: time of first modification. Commit time if in the git repository,
#' system date of creation otherwise.
#' - last_modif: time of last modification. Commit time if in the git repository,
#' system date of last modification otherwise.
#'
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
    first_last <- c(as.POSIXct.numeric(NA_real_, origin = "1970-01-01"),
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

#' Get the first and last modification time of files of a directory
#'
#' @param repo git repository
#' @param path Default to R folder. Use "" for the complete directory
#' @param recursive Logical. Should the listing recurse into directories?
#' @param untracked Logical. Should the not tracked files be included?
#'
#' @return A list of files with information of each file:
#'
#' - file: file name
#' - in_repository: Logical. Whether the file has already been commit once in the
#' git repository
#' - first_modif: time of first modification. Commit time if in the git repository,
#' system date of creation otherwise.
#' - last_modif: time of last modification. Commit time if in the git repository,
#' system date of last modification otherwise.
#'
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


#' Presenting results of files and last modification time in a printed table using 'kable()'
#'
#' @inheritParams get_last_modif
#'
#' @importFrom dplyr transmute
#'
#' @return A 'kable()' output to be included in a markdown file
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

}


#' Creates and updates a vignette in the 'vignette/' directory of a package with last modification time of tracked files
#'
#' @return No return value, called for side effect.
#' Creates and updates the content of the "modification_files.Rmd" vignette
#' @export
#'
#' @rdname create_vignette_last_modif
#' @examples
#'
#' # Creates vignette
#' repo <- fake_repo(as.package = TRUE)
#' if (rmarkdown::pandoc_available("1.12.3")) {
#'   create_vignette_last_modif(repo, path = "R")
#' }

create_vignette_last_modif <- function(repo = ".", path = "R",
                                       recursive = TRUE, untracked = TRUE) {
  vig <- file.path(repo, "vignettes")

  if (!dir.exists(vig)) {
    stop("vignettes folder doesn't exist, please create vignettes folder")
  } else {
    update_vignette_last_modif(repo, path, recursive, untracked)
  }
}

#' @inheritParams get_last_modif
#'
#' @export
#' @rdname create_vignette_last_modif
#'
#' @examples
#' # update vignette
#' repo <- fake_repo(as.package = TRUE)
#' if (rmarkdown::pandoc_available("1.12.3")) {
#'   update_vignette_last_modif(repo, path = "R")
#'   rmarkdown::render(file.path(repo, "vignettes", "modification_files.Rmd"))
#' }
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
    md <- c(
      paste0("Created on: ", Sys.time()),
      "\n\n",
      present_files(repo, path, recursive, untracked), "\n\n")
    write(md, file = file, append = TRUE)
  } else {
    stop("Copying the file didn't work!")
  }
}
