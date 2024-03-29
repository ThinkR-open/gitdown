# General ----
usethis::use_build_ignore("dev_history.R")
usethis::use_build_ignore("reference/")
usethis::use_build_ignore("_pkgdown.yml")
usethis::use_build_ignore("gitdown")
usethis::use_git_ignore("gitdown")

usethis::use_git(message = "init commitdown")
usethis::use_mit_license("ThinkR")
usethis::use_testthat(edition = 3)

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
usethis::use_roxygen_md()
usethis::use_package_doc()

# Dependencies ----
# Clean importFrom
# remotes::install_github("dreamRs/prefixer")
not_used <- prefixer::check_import_from()
not_used
# Build description deps
# attachment::att_to_description(extra.suggests = "bookdown")
attachment::att_amend_desc(extra.suggests = "rmarkdown")


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
# tic::use_tic()
# Sys.setenv(R_TRAVIS = ".org")
# travis::browse_travis_token(endpoint = '.org')
# tic::use_tic(wizard = FALSE, linux = "travis", mac = "travis",
#              windows = "appveyor", deploy = "travis", matrix = "travis",
#              travis_endpoint = ".org")
# # usethis::use_travis()
# usethis::use_appveyor()
usethis::use_github_action("check-standard")
usethis::use_github_action("pkgdown")
usethis::use_github_action("test-coverage")


# CRAN ----
# Check content
# remotes::install_github("ThinkR-open/checkhelper")
checkhelper::find_missing_tags()

# Check spelling
# usethis::use_spell_check()
spelling::spell_check_package()

# Check URL are correct
# remotes::install_github("r-lib/urlchecker")
urlchecker::url_check()
urlchecker::url_update()

# check on other distributions
Sys.time()
rcmdcheck::rcmdcheck(args = "--as-cran")
Sys.time()
devtools::test()
Sys.time()

# rhub::validate_email()
rhub::check_for_cran(show_status = FALSE)
rhub::check_on_windows(check_args = "--force-multiarch")
rhub::check_on_fedora()
rhub::check_on_solaris(show_status = FALSE)
rhub::check_on_windows()
rhub::check_on_debian(show_status = FALSE)

devtools::check_win_devel()
devtools::check_win_release()
devtools::check_mac_release()

# Update NEWS
# Bump version manually and add list of changes

# Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

usethis::use_cran_badge()
usethis::use_cran_comments()
usethis::use_git_ignore("cran-comments.md")
usethis::use_git_ignore("CRAN-RELEASE")
usethis::use_git_ignore("CRAN-SUBMISSION")

devtools::release()

# After release
usethis::use_dev_version()

