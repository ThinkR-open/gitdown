test_that("fake_repo works", {
  repo_input <- tempfile(pattern = "git2r-")
  repo_output <- fake_repo(repo_input)

  branches <- git2r::branches(repo_output)

  expect_false(is.null(branches[["main"]]))

  all_commits <- git2r::commits(
    repo = repo_output, ref = "main",
    topological = TRUE, time = TRUE, reverse = FALSE
  )

  # expect_equal(repo_input, repo_output)
  expect_length(all_commits, 4)
})
