export_versions <- function(filepath="versions.csv", pattern="crea") {
  # Export version of packages being used
  ns <- loadedNamespaces()
  ns <- ns[grepl(pattern, ns, ignore.case=T)]
  ns %>%
    lapply(function(x) {
      tibble(package=x,
             version=packageVersion(x),
             last_commit=get_latest_commit_hash(glue("energyandcleanair/{x}"))
             )
    }) %>%
    bind_rows() %>%
    readr::write_csv(filepath)
}


get_latest_commit_hash <- function(repo) {
  tryCatch({
    url <- paste0("https://api.github.com/repos/", repo, "/commits")
    response <- httr::GET(url)
    if (httr::status_code(response) != 200) {
      return(NA)
    }
    commits <- httr::content(response, "parsed")
    latest_commit_hash <- commits[[1]]$sha
    latest_commit_hash},
    error = function(e) {
      return(NA)
    })
}


