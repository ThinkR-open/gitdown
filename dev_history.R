# General ----
usethis::use_build_ignore("dev_history.R")
usethis::use_git(message = "init commitdown")
usethis::use_mit_license("ThinkR")

# Functions ----
usethis::use_pipe()
usethis::use_r("fake_repo")
usethis::use_test("fake_repo")
usethis::use_r("get_commits")
usethis::use_test("get_commits")
usethis::use_r("utils")
usethis::use_test("utils")

# Documentation ----
usethis::use_vignette("aa-create-commit_down")
usethis::use_readme_rmd()
usethis::use_vignette("ab-create-git_down")

# Dependencies ----
attachment::att_to_description(extra.suggests = "bookdown")

# dev ----
devtools::load_all()
devtools::test()

# GLobalVariables ----
usethis::use_r("globals.R")
globals <- checkhelper::get_no_visible()
globals
# Print globals to copy-paste
checkhelper::print_globals(globals)
# Store in package using usethis::use_r("globals")

# _CI
tic::use_tic()
usethis::use_travis()
