if (dir.exists("~/.R/profile.d")) {
  files <- list.files("~/.R/profile.d", pattern = "\\.R$", full.names = TRUE)
  for (f in files) {
    source(f)
  }
}
