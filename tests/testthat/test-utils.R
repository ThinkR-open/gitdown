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
# With table of correspondence
pattern.table <- data.frame(
  number = c("#2", "#1", "#1000"),
  title = c("#2 A second issue to illustrate a blog post",
            "#1 An example of issue",
            "#1000 issue with no commit"))
nest_out_pattern_table <- nest_commits_by_pattern(
  repo,
  pattern.table = pattern.table,
  pattern = c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"))

test_that("nest_commits_by_pattern", {
  # One pattern
  expect_equal(
    nest_out$data[[1]]$message_link[2],
    "  + Issues: [#32](#issues-32), [#1](#issues-1), [#12](#issues-12)"
    # "Add NEWS  \n  \nissue [#32](#issues-32).  \nissue [#1](#issues-1).  \nissue[#12](#issues-12)  \nticket6789.  \nticket1234  \n Creation of the NEWS file for version 0.1."
  )
  # No pattern
  expect_equal(
    nest_out$data[[6]]$message_link,
    "  + Issues: [No related issues](#issues-na)"
    # nest_out$data[[6]]$message
  )
  # Multiple patterns
  expect_equal(
    nest_out_patterns$data[[1]]$message_link[2],
    "  + Issues: [#32](#issues-32), [#1](#issues-1), [#12](#issues-12)  \n  + Tickets: [ticket6789](#tickets-ticket6789), [ticket1234](#tickets-ticket1234)"
    # "Add NEWS  \n  \nissue [#32](#issues-32).  \nissue [#1](#issues-1).  \nissue[#12](#issues-12)  \n[ticket6789](#tickets-ticket6789).  \n[ticket1234](#tickets-ticket1234)  \n Creation of the NEWS file for version 0.1."
  )
  expect_equal(
    nest_out_patterns$data[[9]]$message_link[2],
    "  + Issues: [#2](#issues-2), [#145](#issues-145)  \n  + Tickets: [No related tickets](#tickets-na)"
    # "Third commit message  \n  \nissue [#2](#issues-2)  \nissue [#145](#issues-145).  \n[ticket1234](#tickets-ticket1234)  \n More information important for the project as breaking changes"
  )
  # With supp issues
  expect_true(is.na(nest_out_pattern_table$data[[6]]$sha))
})

# each_pattern ----
res_each_pattern <- each_pattern(nest_out_pattern_table, "Issues")

test_that("nest_commits_by_pattern", {
  expect_equal(nrow(res_each_pattern), 8)
  expect_equal(ncol(res_each_pattern), 7)
  expect_equal(res_each_pattern$text[[7]][1],
               "## Issue: #1000 issue with no commit{#issues-1000}")
})

# each_commit ----
# One issue with 2 commits
two_commits <- each_commit(
  commits = nest_out_pattern_table[1,]$data[[1]],
  pattern.content = nest_out_pattern_table[1,]$pattern.content,
  link_pattern = nest_out_pattern_table[1,]$link_pattern,
  pattern.type = nest_out_pattern_table[1,]$pattern.type,
  pattern.title = nest_out_pattern_table[1,]$pattern.title
)# One issue with no commits
no_commits <- each_commit(
  commits = nest_out_pattern_table[6,]$data[[1]],
  pattern.content = nest_out_pattern_table[6,]$pattern.content,
  link_pattern = nest_out_pattern_table[6,]$link_pattern,
  pattern.type = nest_out_pattern_table[6,]$pattern.type,
  pattern.title = nest_out_pattern_table[6,]$pattern.title
)

test_that("each_commit", {
  # two commits
  expect_equal(length(two_commits), 3)
  expect_equal(
    two_commits[1],
    "## Issue: #1 An example of issue{#issues-1}"
  )
  expect_true(
    grepl("Related patterns:\n  + Issues: [#1 An example of issue](#issues-1)  \n  + Tickets: [ticket1234](#tickets-ticket1234)",
          two_commits[2], fixed = TRUE)
  )
  # no commits
  expect_equal(length(no_commits), 2)
  expect_equal(
    no_commits[1],
    "## Issue: #1000 issue with no commit{#issues-1000}"
  )
  expect_equal(
    no_commits[2],
    "*No commits*"
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
    link_pattern = "issues-32",
    link_commit = "issues-32-3f0bdaee89cecee654a1cfb24acd1a5efcfbe86e",
    message_link = "  + Issues: [#32](#issues-32), [#1](#issues-1), [#12](#issues-12)  \n  + Tickets: [ticket6789](#tickets-ticket6789), [ticket1234](#tickets-ticket1234)"
  ),
  row.names = c(NA, -1L), class = c("tbl_df", "tbl", "data.frame")
)

prez_commit <- gitdown:::presentation_commit(one_commit)
# writeLines(prez_commit, "tests/testthat/presentation_commit.md")
# paste(readLines("tests/testthat/presentation_commit.md"), collapse = "\n")

test_that("presentation_commit", {
  expect_equal(prez_commit, paste(readLines("presentation_commit.md"), collapse = "\n"))
})

# clean_link ----
test_that("clean_link", {
  expect_equal(
    clean_link(" -Text With # \\\\ \ / (] -+ : {\"}) _characters_ "),
    "text-with-characters"
  )
  expect_equal(
    clean_link("\"\\_ 1  **1  B 2#\\$@ ['ici']__ et --   `là` (1) :/  _"),
    "b-2-ici-et-là-1")
})

# clean text ----

test_that("clean_text", {
  expect_equal(
    clean_text("__ toto *[toot] * _{#}"),
    "-- toto -toot-  -#")
})

