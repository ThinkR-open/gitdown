time_before <- Sys.time()
Sys.sleep(1)

repo_pkg <- fake_repo(as.package = TRUE)
repo_no_pkg <- fake_repo()

# get_info ----
files <- list.files(repo_pkg, recursive = TRUE)
infos <- get_info(files[grep("example", files)], repo = repo_pkg)
infos_not_in_repo <- get_info(files[grep("my_mean", files)], repo = repo_pkg)

# if check started one day and finishes the other day
Sys.sleep(1)
time_after <- Sys.time()

test_that("get_info works", {
  expect_true(infos$in_repository)
  expect_equal(infos$file, "example.txt")

  expect_gte(infos$last_modif, time_before)
  expect_lte(infos$last_modif, time_after)

  expect_false(infos_not_in_repo$in_repository)
  expect_equal(infos_not_in_repo$file, "R/my_mean.R")
  expect_true(is.na(infos_not_in_repo$first_modif))
  expect_gte(infos_not_in_repo$last_modif, time_before)
  expect_lte(infos_not_in_repo$last_modif, time_after)
})

# get_last_modif ----
R_dir_not_track <- get_last_modif(repo = repo_pkg)
all_dir_track <- get_last_modif(repo = repo_pkg, path = "")

test_that("get_last_modif works", {
  # 1 file in R/
  expect_equal(length(R_dir_not_track), 1)
  expect_equal(length(R_dir_not_track[[1]]), 4)
  # 3 files in repo
  expect_equal(length(all_dir_track), 3)
})


# present_files ----
present_out <- present_files(repo_pkg, path = "")
present_out_r <- present_files(repo_pkg, path = "R")

test_that("present_files works", {
  expect_true(grepl("example.txt", present_out))
  expect_true(grepl("NEWS.md", present_out))
  expect_true(grepl("R/my_mean.R", present_out))

  expect_false(grepl("example.txt", present_out_r))
  expect_false(grepl("NEWS.md", present_out_r))
  expect_true(grepl("R/my_mean.R", present_out_r))
})

test_that("present_files error", {
  expect_error(present_files(repo_no_pkg))
})

# create_vignette_last_modif ----
if (rmarkdown::pandoc_available("2.0.0")) {
  create_vignette_last_modif(repo_pkg)
  # browseURL(repo_pkg)

  test_that("create_vignette_last_modif works", {
    expect_true(file.exists(file.path(repo_pkg, "vignettes", "modification_files.Rmd")))
  })
  # Clean repo
  file.remove(file.path(repo_pkg, "vignettes", "modification_files.Rmd"))

  # All files
  create_vignette_last_modif(repo_pkg, path = "")

  test_that("create_vignette_last_modif works", {
    expect_true(file.exists(file.path(repo_pkg, "vignettes", "modification_files.Rmd")))
  })
  # Clean repo
  file.remove(file.path(repo_pkg, "vignettes", "modification_files.Rmd"))

  # No vignettes/
  test_that("create_vignette_last_modif error", {
    expect_error(create_vignette_last_modif(repo_no_pkg))
  })
}
