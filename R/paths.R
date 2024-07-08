#' Try to find CREA gis directory, from system environment, or R environment
#'
#' @return
#' @export
#'
#' @examples
get_gis_dir <- function(){

  if(exists('gis_dir', envir=.GlobalEnv))
    return(get('gis_dir', envir=.GlobalEnv))

  suppressWarnings(try(readRenviron(".Renviron"), silent = TRUE))
  suppressWarnings(try(dotenv::load_dot_env(), silent = TRUE))

  ds <- c(Sys.getenv("GIS_DIR"), Sys.getenv("DIR_GIS"))
  d <- ds[min(which(ds!=""))]

  if(d==""){
    stop("Couldn't find gis_dir. Please set gis_dir in your R environment,
         or GIS_DIR or DIR_GIS in your system environment (e.g. in .Renviron or .env file)")
  }


  if(!dir.exists(d)){
    stop(sprintf("GIS Directory %s doesn't exist. Please update gis_dir in your R environment,
                    or GIS_DIR or DIR_GIS in your system environment (e.g. in .Renviron or .env file)", d))
  }

  return(d)
}


get_boundaries_path <- function(file=""){
  file.path(get_gis_dir(), "boundaries", file)
}

get_concentration_path <- function(file=""){
  file.path(get_gis_dir(), "concentration", file)
}

get_landcover_path <- function(file=""){
  file.path(get_gis_dir(), "landcover", file)
}

get_population_path <- function(file=""){
  file.path(get_gis_dir(), "population", file)
}

get_elevation_path <- function(file=""){
  file.path(get_gis_dir(), "elevation", file)
}
