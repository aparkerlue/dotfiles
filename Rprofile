local({
  ## CRAN repository.
  repos.option <- getOption("repos")
  repos.option["CRAN"] <- "https://cran.cnr.berkeley.edu/"
  options(repos=repos.option)
})

.First <- function() {
  ## Load personal library.
  path <- "~/lib/R"
  for (nm in list.files(path, pattern = "\\.R$")) {
    source(file.path(path, nm))
  }
}
