export_versions <- function(filepath="versions.csv", pattern="crea") {
  # Export version of packages being used
  ns <- loadedNamespaces()
  ns <- ns[grepl(pattern, ns, ignore.case=T)]
  ns %>%
    lapply(function(x) {
      tibble(package=x, version=packageVersion(x))
    }) %>%
    bind_rows() %>%
    readr::write_csv(filepath)
}
