local({
  ## CRAN repository.
  repos.option <- getOption("repos")
  repos.option["CRAN"] <- "https://cran.cnr.berkeley.edu/"
  options(repos=repos.option)
})
