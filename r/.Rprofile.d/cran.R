local({
  ## CRAN repository.
  repos.option <- getOption("repos")
  repos.option["CRAN"] <- "http://lib.stat.cmu.edu/R/CRAN/"
  options(repos=repos.option)
})
