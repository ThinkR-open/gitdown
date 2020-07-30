#' Get time
#'
#' @param object hunks list
#'
#' @return time
get_time <- function(object){
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
#' @importFrom lubridate as_datetime
#' @importFrom git2r blame
@importFrom purrr pluck map flatten_dbl set_names
#'
#' @examples
#' repo <- fake_repo()
#' get_info(list.files(repo)[1], repo = repo )
get_info <- function(path, repo = "."){

  blame_object <- blame(repo = repo ,path = path)

    file <-  blame_object$hunks[[1]]$orig_path %>%
        gsub(pattern = "^R/", replacement = "") %>%
        gsub(pattern = "\\.R$", replacement = "")


    first_last <- blame_object %>%
      pluck("hunks") %>%
      map(get_time) %>%
      flatten_dbl() %>%
      range() %>%
      lubridate::as_datetime() %>%
      set_names(nm = c("first", "last"))

    list(file = file ,
         fisrt_modif = first_last[1],
         last_modif = first_last[2])
}

mise_en_forme <- function(repo = "."){

  R_path <- file.path(repo, "R")

  if(dir.exists(R_path)){
    files <- file.path('R',list.files(R_path))
    map(files, ~ get_info(.x, repo = repo))
  }

}

mise_en_forme()
