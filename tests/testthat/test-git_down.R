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
sub_commit_1 <- grep('<h3><span class="header-section-number">1.2.1</span> commit: example: modification</h3>',
                     lines, fixed = TRUE)
sub_commit_2 <- grep('<h3><span class="header-section-number">1.2.2</span> commit: Add NEWS</h3>',
                     lines, fixed = TRUE)

test_that("git_down function",{
  expect_match(res, regexp = ".html")
  expect_true(file.exists(file.path(dirname(res), "section-issue.html")))
  expect_length(one_line_exists, 1)
  expect_length(sub_commit_1, 1)
  expect_length(sub_commit_2, 1)
})

# clean dir
unlink(dirname(res), recursive = TRUE)

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

# clean dir
unlink(dirname(res), recursive = TRUE)

# With multiple patterns ----
with_dir(repo, {
  res <- git_down(author = "Seb",
                  pattern = c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"),
                  open = FALSE
  )
})

test_that("git_down multiple pattern works", {
  expect_match(res, regexp = ".html")

  issues_file <- normalizePath(file.path(dirname(res), "section-issues.html"))
  tickets_file <- normalizePath(file.path(dirname(res), "section-tickets.html"))

  expect_true(file.exists(issues_file))
  expect_true(file.exists(tickets_file))

  lines <- readLines(tickets_file)
  one_line_ticket_exists <- grep('<h2><span class="header-section-number">1.2</span> Ticket: ticket1234</h2>',
                                 lines, fixed = TRUE)

  expect_true(length(one_line_ticket_exists) == 1)

  lines <- readLines(issues_file)
  one_line_exists <- grep('<h2><span class="header-section-number">2.2</span> Issue: #1</h2>',
                          lines, fixed = TRUE)
  expect_true(length(one_line_exists) == 1)
})

# clean dir
unlink(dirname(res), recursive = TRUE)

# With table of correspondence
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

# clean dir
unlink(dirname(res), recursive = TRUE)
