repo <- fake_repo()

# get_commits_tags ----
commits_tbl <- get_commits_tags(repo = repo)

test_that("get_commits_tags works", {
  expect_equal(nrow(commits_tbl), 4)
  expect_equal(ncol(commits_tbl), 9)
  expect_equal(commits_tbl$tag.name, c(NA, "v0.1", "v0.1", "v0.1"))
})

# get_commits_pattern ----
patterns_tbl <- get_commits_pattern(repo = repo, silent = TRUE)

test_that("get_commits_pattern works", {
  expect_equal(nrow(patterns_tbl), 7)
  expect_equal(ncol(patterns_tbl), 12)
  expect_equal(patterns_tbl$pattern.type, rep("Ticket", 7))
  expect_equal(patterns_tbl$pattern.content,
               c("#32", "#1", "#12", "#2", "#145", "#1", NA))
  expect_equal(patterns_tbl$pattern.title,
               c("#32", "#1", "#12", "#2", "#145", "#1", NA))
})

# With table of correspondance
pattern.table <- data.frame(
  number = c("#2", "#1"),
  title = c("#2 A second issue to illustrate a blog post",
            "#1 An example of issue"))

patterns_table_tbl <- get_commits_pattern(repo = repo, silent = TRUE,
                                          pattern.table = pattern.table)

test_that("get_commits_pattern with table works", {
  expect_equal(nrow(patterns_table_tbl), 7)
  expect_equal(ncol(patterns_table_tbl), 12)
  expect_equal(patterns_table_tbl$pattern.type, rep("Ticket", 7))
  expect_equal(patterns_table_tbl$pattern.content,
               c("#32", "#1", "#12", "#2", "#145", "#1", NA))
  expect_equal(patterns_table_tbl$pattern.title,
               c("#32", "#1 An example of issue",
                          "#12", "#2 A second issue to illustrate a blog post",
                          "#145", "#1 An example of issue", NA))
})

# my_extract ----
test_that("my_extract works", {
  expect_equal(my_extract("message #1", "#[[:digit:]]+"), "#1")
  expect_equal(my_extract("message #1 #2", "#[[:digit:]]+"), c("#1", "#2"))
  expect_true(is.na(my_extract("message 0", "#[[:digit:]]+")))
})
