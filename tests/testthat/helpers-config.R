library(withr)
library(stringr)
tmp <- tempdir()
repo <- fake_repo(file.path(tmp,"test-git"))

with_dir(repo,{
  dir.create("gitdown")
  lapply(
    list.files(system.file("booktemplate/", package = "gitdown"), full.names = TRUE),
    function(x){file.copy(from = x, to = normalizePath(file.path(repo, "gitdown")))}
    )
  })
