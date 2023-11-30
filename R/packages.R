unload_packages <- function(packages){
  if(is.null(packages)) packages <- names(sessionInfo()$otherPkgs)
  for(package in packages){
    if(package %in% (.packages())){detach(sprintf('package:%s',package), character.only=T, unload=T)};
  }
}

reload_packages <- function(packages){
  if(is.null(packages)) packages <- names(sessionInfo()$otherPkgs)
  unload_packages(packages)
  for(package in packages){
    library(package, character.only=TRUE)
  }
}

