repo <- fake_repo()
commits_tbl <- get_commits_tags(repo = repo)

test_that("get_commits_tags works", {
  expect_equal(nrow(commits_tbl), 4)
  expect_equal(ncol(commits_tbl), 9)
  expect_equal(commits_tbl$tag.name, c(NA, "v0.1", "v0.1", "v0.1"))
})

patterns_tbl <- get_commits_pattern(repo = repo, silent = TRUE)

test_that("get_commits_pattern works", {
  expect_equal(nrow(patterns_tbl), 6)
  expect_equal(ncol(patterns_tbl), 10)
  expect_equal(patterns_tbl$pattern, c("#32", "#1", "#2", "#145", "#1", NA))
})
