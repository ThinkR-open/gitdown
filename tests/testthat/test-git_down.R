library(withr)

repo_input <- tempfile(pattern = "git2r-")
repo <- fake_repo(repo_input)

# Classic use
with_dir(repo, {
  res <- git_down(author = "Cervan",
                  pattern = c("Issue" = "#[[:digit:]]+"),
                  open = FALSE
  )
})
lines <- readLines(file.path(dirname(res), "section-issue.html"))
one_line_exists <- grep('<h2><span class="header-section-number">1.2</span> Issue: #1</h2>',
                        lines, fixed = TRUE)

test_that("git_down function",{
  expect_match(res, regexp = ".html")
  expect_true(file.exists(file.path(dirname(res), "section-issue.html")))
  expect_true(length(one_line_exists) == 1)
})

# Pattern without name and special characters cleaned
with_dir(repo, {
  res <- git_down(author = "StatnMap",
                  pattern = c("#[[:digit:]]+"),
                  open = FALSE
  )
})

lines <- readLines(file.path(dirname(res), "section-digit.html"))
one_line_exists <- grep('<h2><span class="header-section-number">1.2</span> <code>#--:digit:--+</code>: #1</h2>',
                        lines, fixed = TRUE)

test_that("git_down no name function",{
  expect_match(res, regexp = ".html")
  expect_true(file.exists(file.path(dirname(res), "section-digit.html")))
  expect_true(length(one_line_exists) == 1)
})

# With table of correspondance
pattern.table <- data.frame(
  number = c("#2", "#1", "#1000"),
  title = c("#2 A second issue to illustrate a blog post",
            "#1 An example of issue",
            "#1000 issue with no commit"))

test_that("git_down with pattern table",{
  with_dir(repo, {
    res <- git_down(author = "Cervan",
                    pattern = c("Issue" = "#[[:digit:]]+"),
                    pattern.table = pattern.table,
                    open = FALSE
    )
    expect_match(res, regexp = ".html")
  })
})
