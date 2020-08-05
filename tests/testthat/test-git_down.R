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
