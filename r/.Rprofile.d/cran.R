local({
  ## CRAN repository.
  repos.option <- getOption("repos")
  repos.option["CRAN"] <- "https://ftp.osuosl.org/pub/cran/"
  options(repos=repos.option)
})
