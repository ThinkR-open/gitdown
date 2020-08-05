repo <- fake_repo()
commits_tbl <- get_commits_tags(repo = repo)

test_that("get_commits_tags works", {
  expect_equal(nrow(commits_tbl), 4)
  expect_equal(ncol(commits_tbl), 9)
  expect_equal(commits_tbl$tag.name, c(NA, "v0.1", "v0.1", "v0.1"))
})

patterns_tbl <- get_commits_pattern(repo = repo, silent = TRUE)

test_that("get_commits_pattern works", {
  expect_equal(nrow(patterns_tbl), 7)
  expect_equal(ncol(patterns_tbl), 11)
  expect_equal(patterns_tbl$pattern.type, rep("Ticket", 7))
  expect_equal(patterns_tbl$pattern.content,
               setNames(c("#32", "#1", "#12", "#2", "#145", "#1", NA),
                        rep("Ticket", 7)))
})

test_that("my_extract works", {
  expect_equal(my_extract("message #1", "#[[:digit:]]+"), "#1")
  expect_equal(my_extract("message #1 #2", "#[[:digit:]]+"), c("#1", "#2"))
  expect_true(is.na(my_extract("message 0", "#[[:digit:]]+")))
})
