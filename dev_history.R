# General ----
usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("reference/")
usethis::use_build_ignore("_pkgdown.yml")

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
usethis::use_test("git_down")
usethis::use_test("get_info_files")

# Documentation ----
usethis::use_vignette("aa-create-commit_down")
usethis::use_readme_rmd()
usethis::use_vignette("ab-create-git_down")
usethis::use_data_raw()
usethis::use_news_md()

# Dependencies ----
# Clean importFrom
# remotes::install_github("dreamRs/prefixer")
not_used <- prefixer::check_import_from()
not_used
# Build description deps
# attachment::att_to_description(extra.suggests = "bookdown")
attachment::att_to_description(extra.suggests = "rmarkdown")


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
# remotes::install_github("ropenscilabs/travis")
# remotes::install_github("ropensci/tic")
tic::use_tic()
Sys.setenv(R_TRAVIS = ".org")
travis::browse_travis_token(endpoint = '.org')
tic::use_tic(wizard = FALSE, linux = "travis", mac = "travis",
             windows = "appveyor", deploy = "travis", matrix = "travis",
             travis_endpoint = ".org")
# usethis::use_travis()
usethis::use_appveyor()