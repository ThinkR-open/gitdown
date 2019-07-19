# General
usethis::use_build_ignore("dev_history.R")
usethis::use_git(message = "init commitdown")
usethis::use_mit_license("ThinkR")

# Functions
usethis::use_r("fake_repo")
usethis::use_test("fake_repo")

# Documentation
usethis::use_vignette("aa-create-commit_down")

# Dependencies
attachment::att_to_description()
