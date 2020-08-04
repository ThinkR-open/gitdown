context("utils functions")
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

test_that("replace_file", {
  with_dir(repo, {
    replace_in_file("example.txt", pattern = "Lorem", replacement = "test-replacment")
    test <- readLines(con = "example.txt", n = 1)
    expect_true(grepl("test-replacment", test))
  })

})

test_that("write_in",{
  with_dir(repo, {
    write_in("test-write-in", repo )
    test <- readLines(con = "gitdown/index.Rmd")
    expect_true(any(grepl("test-write-in", test)))
  })
})

test_that("git_down function",{
  with_dir(repo,{
    res <- git_down(author = "Cervan",
                    pattern = "#[[:digit:]]+",
                    names_section = "Issue"
                    )
    expect_match(res, regexp = ".html")
  })
})
