context("utils functions")

test_that("replace_file", {
  with_dir(repo,{
    replace_in_file("example.txt",pattern = "Lorem", replacement = "test-replacment")
    test <- readLines(con = "example.txt", n = 1)
    expect_true(str_detect(test,"test-replacment"))
  })

})

test_that("write_in",{
  with_dir(repo,{
    write_in("test-write-in", repo )
    test <- readLines(con = "gitdown/index.Rmd")
    expect_true(any(str_detect(test,"test-write-in")))
  })
})

test_that("git_down function",{
  with_dir(repo,{
    res <- git_down(author = "Cervan",
                    pattern = "#[[:digit:]]+",
                    names_section = "Issue"
                    )
    expect_match(res,regexp = ".html")
  })
})
