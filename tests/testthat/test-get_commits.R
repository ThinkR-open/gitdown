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

# With table of correspondence
pattern.table <- data.frame(
  number = c("#2", "#1", "#1000"),
  title = c("#2 A second issue to illustrate a blog post",
            "#1 An example of issue",
            "#1000 issue with no commit"))

patterns_table_tbl <- get_commits_pattern(repo = repo, silent = TRUE,
                                          pattern.table = pattern.table)

test_that("get_commits_pattern with table works", {
  expect_equal(nrow(patterns_table_tbl), 8)
  expect_equal(ncol(patterns_table_tbl), 12)
  expect_equal(patterns_table_tbl$pattern.type, rep("Ticket", 8))
  expect_equal(patterns_table_tbl$pattern.content,
               c("#32", "#1", "#12", "#2", "#145", "#1", NA, "#1000"))
  expect_equal(patterns_table_tbl$pattern.title,
               c("#32", "#1 An example of issue",
                          "#12", "#2 A second issue to illustrate a blog post",
                          "#145", "#1 An example of issue", NA,
                 "#1000 issue with no commit"))
})

# Test conditions
# With pattern names
pattern_no_name <- get_commits_pattern(repo = repo, silent = TRUE,
                    pattern = "#[[:digit:]]+")

test_that("get_commits_pattern fails", {
  expect_true(all(pattern_no_name$pattern.type == rep("`#[[:digit:]]+`", 7)))
})

# With bad table of correspondence
pattern.table.1 <- data.frame(
  number = c("#2", "#1", "#1000"))
pattern.table.3 <- data.frame(
  number = c("#2", "#1", "#1000"),
  title = c("#2 A second issue to illustrate a blog post",
            "#1 An example of issue",
            "#1000 issue with no commit"),
  third = c(1, 1, 1))

test_that("get_commits_pattern fails", {
  expect_error(
    get_commits_pattern(repo = repo, silent = TRUE,
                        pattern.table = pattern.table.1)
  )
  expect_warning(
    get_commits_pattern(repo = repo, silent = TRUE,
                        pattern.table = pattern.table.3)
  )
})


# my_extract ----
test_that("my_extract works", {
  expect_equal(my_extract("message #1", "#[[:digit:]]+"), "#1")
  expect_equal(my_extract("message #1 #2", "#[[:digit:]]+"), c("#1", "#2"))
  expect_true(is.na(my_extract("message 0", "#[[:digit:]]+")))
})
