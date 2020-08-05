context("utils functions")
library(withr)

repo_input <- tempfile(pattern = "git2r-")
repo <- fake_repo(repo_input)

with_dir(repo, {
  dir.create("gitdown")
  file.copy(
    from = list.files(system.file("booktemplate/", package = "gitdown"),
                      full.names = TRUE),
    to = normalizePath(file.path(repo, "gitdown")))
})

# replace_file ----
test_that("replace_file", {
  with_dir(repo, {
    replace_in_file("example.txt", pattern = "Lorem", replacement = "test-replacment")
    test <- readLines(con = "example.txt", n = 1)
    expect_true(grepl("test-replacment", test))
  })

})

# write_in ----
test_that("write_in",{
  with_dir(repo, {
    write_in("test-write-in", repo )
    test <- readLines(con = "gitdown/index.Rmd")
    expect_true(any(grepl("test-write-in", test)))
  })
})

# nest_commits_by_pattern ----
nest_out <- nest_commits_by_pattern(repo)
nest_out_patterns <- nest_commits_by_pattern(
  repo,
  pattern = c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"))

# writeLines(aa, "tests/testthat/presentation_commit")

test_that("nest_commits_by_pattern", {
  # One pattern
  expect_equal(
    nest_out$data[[1]]$message_link[2],
    "  + Issues: [#32](#aissues-32), [#1](#aissues-1), [#12](#aissues-12)"
    # "Add NEWS  \n  \nissue [#32](#aissues-32).  \nissue [#1](#aissues-1).  \nissue[#12](#aissues-12)  \nticket6789.  \nticket1234  \n Creation of the NEWS file for version 0.1."
  )
  # No pattern
  expect_equal(
    nest_out$data[[6]]$message_link,
    "  + Issues: [No related issues](#aissues-na)"
    # nest_out$data[[6]]$message
    )
  # Multiple patterns
  expect_equal(
    nest_out_patterns$data[[1]]$message_link[2],
    "  + Issues: [#32](#aissues-32), [#1](#aissues-1), [#12](#aissues-12)  \n  + Tickets: [ticket6789](#atickets-ticket6789), [ticket1234](#atickets-ticket1234)"
    # "Add NEWS  \n  \nissue [#32](#aissues-32).  \nissue [#1](#aissues-1).  \nissue[#12](#aissues-12)  \n[ticket6789](#atickets-ticket6789).  \n[ticket1234](#atickets-ticket1234)  \n Creation of the NEWS file for version 0.1."
  )
  expect_equal(
    nest_out_patterns$data[[9]]$message_link[2],
    "  + Issues: [#2](#aissues-2), [#145](#aissues-145)  \n  + Tickets: [No related tickets](#atickets-na)"
    # "Third commit message  \n  \nissue [#2](#aissues-2)  \nissue [#145](#aissues-145).  \n[ticket1234](#atickets-ticket1234)  \n More information important for the project as breaking changes"
  )
})

# presentation_commit ----
one_commit <- structure(
  list(
    sha = "3f0bdaee89cecee654a1cfb24acd1a5efcfbe86e",
    summary = "Add NEWS",
    message = "Add NEWS\n\nissue #32.\nissue #1.\nticket6789.\nticket1234\n Creation of the NEWS file for version 0.1.",
    author = "Alice",
    email = "alice@example.org",
    when = structure(1596623129, tzone = "GMT", class = c("POSIXct", "POSIXt")),
    order = 4L, tag.name = NA_character_, tag.message = NA_character_,
    pattern.type = "issues", pattern.content = "#32", pattern_numeric = 32,
    link_pattern = "aissues-32",
    link_commit = "aissues-32-3f0bdaee89cecee654a1cfb24acd1a5efcfbe86e",
    message_link = "  + Issues: [#32](#aissues-32), [#1](#aissues-1), [#12](#aissues-12)  \n  + Tickets: [ticket6789](#atickets-ticket6789), [ticket1234](#atickets-ticket1234)"
    # message_link = "Add NEWS\n\nissue [#32](#aissues-32).\nissue [#1](#aissues-1).\nticket6789.\nticket1234\n Creation of the NEWS file for version 0.1."
    # message_link = "Add NEWS\n\n- issue [#32](#aissues-32).\n- issue [#1](#aissues-1).\nticket6789.\nticket1234\n Creation of the NEWS file for version 0.1."
  ),
  row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")
)

prez_commit <- gitdown:::presentation_commit(one_commit)
  # writeLines(prez_commit, "tests/testthat/presentation_commit")

test_that("presentation_commit", {
  expect_equal(prez_commit, paste(readLines("presentation_commit"), collapse = "\n"))
})

# clean_link ----
test_that("clean_link", {
  expect_equal(
    clean_link(" -Text With # \\\\ \ / (] -+ : {\"}) _characters_ "),
    "a-text-with-characters"
  )
})

