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
#' @importFrom git2r blame
#' @importFrom purrr pluck map flatten_dbl set_names
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
    as.POSIXct.numeric(origin = "1970-01-01 UTC") %>%
    set_names(nm = c("first", "last"))

  list(file = file ,
       fisrt_modif = first_last[1],
       last_modif = first_last[2])
}

#' Get last modification of files
#'
#' @param repo repo
#' @param R_folder If TRUE, it will list functions inside the R folder
#'
#' @return list
#' @export
#'
#' @importFrom purrr map
#'
#' @examples
#' repo <- fake_repo()
#' get_last_modif(repo = repo , R_folder = FALSE)
get_last_modif <- function(repo = ".", R_folder = TRUE){
  if(R_folder){
    folder <- file.path(repo, "R")
    if(dir.exists(folder)){
      files <- file.path('R',list.files(folder))
    }else{
      stop("Don't find R folder, you may use R_folder = FALSE")
    }

  }else{
    folder <- repo
    files <- list.files(repo)
  }
  map(files, ~ get_info(.x, repo = repo))
}


#' Formatting results of get_last_modif
#'
#' @inheritParams get_last_modif
#'
#' @return tagList
#' @export
#'
#' @examples
#' repo <- fake_repo()
#' mise_en_forme(repo, R_folder = FALSE)
mise_en_forme <- function(repo = ".", R_folder = TRUE){

  as.character(
    htmltools::tags$ul(
      lapply(get_last_modif(repo, R_folder), function(x){
        htmltools::tags$li(
          htmltools::tags$h4(paste("File:", x$file)),
          htmltools::tags$p(paste("Date of creation:", x$fisrt_modif)),
          htmltools::tags$p(paste("Last modification:", x$last_modif))
        )
      }
      )
    )
  )
}


#' Update vignette last modification of files
#'
#' @inheritParams get_last_modif
#'
#' @return update a vignette
#' @export
update_vign_last_modif <- function(repo = ".", R_folder = TRUE){
  vig <- file.path(repo, "vignettes")
  file <- file.path(vig, "modification_files.Rmd")
  if(file.exists(file)){
    unlink(file)
    }

    path_to_copy <- system.file("template/modification_files.Rmd", package = "gitdown")

    file.copy(path_to_copy, to = vig)

    if(file.exists(file)){
      html <- mise_en_forme(repo, R_folder)
      write(html, file = file, append = TRUE)
    }else{
      stop("Copying the file didn't work!")
    }
}

#' Get the vignette for last modification
#'
#' @inheritParams get_last_modif
#'
#'
#' @return copy a vignette
#' @export

vignette_last_modif <- function(repo = ".", R_folder = TRUE){
  vig <- file.path(repo, "vignettes")

  if(!dir.exists(vig)){
    stop("vignettes folder doesn't exist, please create vignettes folder")
  }else{
    update_vign_last_modif(repo = ".", R_folder = TRUE)
  }
}
