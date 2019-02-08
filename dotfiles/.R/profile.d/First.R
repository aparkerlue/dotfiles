.First <- function() {
  ## Load personal library.
  path <- "~/.local/lib/R"
  for (nm in list.files(path, pattern = "\\.R$")) {
    source(file.path(path, nm))
  }
}
