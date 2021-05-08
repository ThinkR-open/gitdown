library(withr)

# debug if problems with CRAN to retrieve outputs
debug <- FALSE

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
if (debug) {
  dir.create("res_one")
  file.copy(dirname(res), "res_one", recursive = TRUE, overwrite = TRUE)
}
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
if (debug) {
  dir.create("res_two")
  file.copy(dirname(res), "res_two", recursive = TRUE, overwrite = TRUE)
}
unlink(dirname(res), recursive = TRUE)

# With multiple patterns ----

with_dir(repo, {

  res <- git_down(author = "Seb",
                  pattern = c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+"),
                  open = FALSE
  )


  # For Debugging with CRAN ----
  if (debug) {
    res_files <- paste(list.files(dirname(res)), collapse = ", ")

    # repo <- "."

    res_commits <- nest_commits_by_pattern(
      repo,
      pattern = c("Issues" = "#[[:digit:]]+", "Tickets" = "ticket[[:digit:]]+"),
      silent = TRUE
    )

    res_pattern <- purrr::map_dfr(
      names(c("Tickets" = "ticket[[:digit:]]+", "Issues" = "#[[:digit:]]+")),
      ~each_pattern(res_commits, pattern.type = .x))

    res_content <- paste(c(paste0("# Gitbook for ", "toto", "{-} \n"),
                           paste0("Done on: ", Sys.time(), "\n"), "\n", unlist(res_pattern$text)),
                         collapse = "\n\n")

    dir.create(normalizePath(file.path(repo, "gitdown-cran"), mustWork = FALSE), recursive = TRUE)
    file.copy(from = list.files(system.file("booktemplate", package = "gitdown"), full.names = TRUE),
              to = normalizePath(file.path(repo, "gitdown-cran")), overwrite = TRUE)

    gitdown:::write_in(x = res_content, repo = repo, dir = "gitdown-cran", rmd = "index.Rmd")
    res_render <- rmarkdown::render(file.path(repo, "gitdown-cran", "index.Rmd"))
  }
})

# For Debugging with CRAN ----
if (debug) {
  dput(res_commits, file = "res_commits.txt")
  dput(res_files, file = "res_files.txt")
  dput(res_pattern, file = "res_pattern.txt")
  dput(res_content, file = "res_content.txt")
  file.copy(file.path(repo, "gitdown", 'index.Rmd'), "res_index.txt", overwrite = TRUE)
  file.copy(file.path(repo, "gitdown-cran", 'index.Rmd'), "res_index_cran.txt", overwrite = TRUE)
  file.copy(file.path(repo, "gitdown", '_bookdown.yml'), "res_bookdown.txt", overwrite = TRUE)
  file.copy(file.path(repo, "gitdown", '_output.yml'), "res_output.txt", overwrite = TRUE)


  dir.create("res_three")
  file.copy(dirname(res), "res_three", recursive = TRUE, overwrite = TRUE)
}

test_that("git_down multiple pattern works", {
  expect_match(res, regexp = ".html")

  issues_file <- normalizePath(file.path(dirname(res), "section-issues.html"))
  tickets_file <- normalizePath(file.path(dirname(res), "section-tickets.html"))

  # For Debugging with CRAN ----
  if (debug) {
    dput(dirname(res), file = "res_dirname.txt")
    dput(issues_file, file = "res_issues_file.txt")
    dput(tickets_file, file = "res_tickets_file.txt")
  }

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

if (debug) {
  all_res_files <- list.files(pattern = '^res_', full.names = TRUE)
  dir.create("debug")
  file.copy(all_res_files, "debug", recursive = TRUE)
  unlink(all_res_files, recursive = TRUE)
  utils::zip(zipfile = "debug.zip", files = "debug")
}

# clean dir
unlink(dirname(res), recursive = TRUE)
