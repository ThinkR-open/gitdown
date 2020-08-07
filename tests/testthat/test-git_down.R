library(withr)

repo_input <- tempfile(pattern = "git2r-")
repo <- fake_repo(repo_input)

test_that("git_down function",{
  with_dir(repo, {
    res <- git_down(author = "Cervan",
                    pattern = c("Issue" = "#[[:digit:]]+")
    )
    expect_match(res, regexp = ".html")
  })
})

# With table of correspondance
pattern.table <- data.frame(number = c("#2", "#1"),
  title = c("#2 A second issue to illustrate a blog post",
                       "#1 An example of issue"))

test_that("git_down with pattern table",{
  with_dir(repo, {
    res <- git_down(author = "Cervan",
                    pattern = c("Issue" = "#[[:digit:]]+"),
                    pattern.table = pattern.table
    )
    expect_match(res, regexp = ".html")
  })
})
