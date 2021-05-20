
#' Load administrative area boundaries
#'
#' Requires that the GADM RDS or shp files exist in a path specified in either the environment variable GISpath or in the function parameter path. Tries to load first the RDS file and secondarily the shp file. If RDS file does not exist, it will convert the shp into RDS and write to disk.
#'
#' The required files can be downloaded from https://gadm.org/
#' @param level The level of admin boundaries to load.
#' @param path Path to the file
#' @return SpatialPolygonsDataFrame
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
get_adm <- function(level=0, res='full', version='36') {

  resext=''
  if(res!='full') resext=paste0('_', res)

  f <- get_boundaries_path(paste0('gadm',version,'_',level, resext,'.RDS'))

  if(!file.exists(f) & res=='full') {
    message('RDS not found, trying shapefile')
    raster::shapefile(gsub('\\.RDS','.shp',f),
                      encoding='UTF-8', use_iconv=TRUE) -> adm
    saveRDS(adm, f)
  }

  if(!file.exists(f) & res!='full') {
    message('simplifying the admin features')
    simplify_adm(level, res, version)
  }

  readRDS(f)
}



#' Get WDPA polygons intersecting with bbox
#'
#' @param region
#'
#' @return
#' @export
#'
#' @examples
get_wdpa <- function(bbox, sp_or_sf="sp"){

  fs <- list.files(get_landcover_path("wdpa"),
                     "*polygons.shp",
                     recursive = T, full.names = T)

  if(all(dim(bbox)==c(2,2))){
    bbox <- c("xmin"=bbox[1,1],"xmax"=bbox[1,2],
              "ymin"=bbox[2,1],"ymax"=bbox[2,2])
  }

  regions <- pblapply(fs, function(f){
    sf::read_sf(f) %>%
      sf::st_crop(bbox)
  }) %>% do.call(rbind, .)

  if(sp_or_sf=="sp"){
    return(as(regions, "Spatial"))
  }else{
    return(regions)
  }

}
