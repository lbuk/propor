#' Aggregate data within tiles
#'
#' Function for aggregating data within map tiles.
#'
#' @param tile_data A spatial dataset (i.e. sf or SpatialPolygonsDataFrame) of polygon tiles
#' @param geo_points A spatial dataset (i.e. sf or SpatialPointsDataFrame) of points to aggregate
#' @param sum Variable from geo_points for calculating the sum per tile
#' @param count Count the number of points per tile. Input TRUE to count points. Defaults to FALSE
#' @param crs Coordinate Reference System
#' @importFrom dplyr "%>%"
#' @return An sf dataset containing the tiles with the aggregated data.
#' @examples
#' # Load the sf dataset of pumps and cholera deaths in Soho
#' data(soho_pumps, cholera_deaths)
#' 
#' # Generate tiles
#' tile_data <- propor::tiles(data = soho_pumps, shape = "hexagons")
#'
#' # Aggregate data within tiles
#' propor::aggregate(tile_data = tile_data, geo_points = cholera_deaths, sum = "cholera.deaths", crs = 27700)
#' @export

aggregate <- function(tile_data, geo_points, sum = NULL, count = F, crs) {
  
  if(grepl(x = class(tile_data)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(tile_data)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input tile_data as a spatial dataset', call. = F)
    
  } else {
    tile_data <- tile_data %>%
      sf::st_as_sf()
  }
  
  if(grepl(x = class(geo_points)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(geo_points)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input geo_points as a spatial dataset', call. = F)
    
  } else {
    geo_points <- geo_points %>%
      sf::st_as_sf()
  }
  
  new_data <- tile_data %>%
    dplyr::mutate(id = dplyr::row_number(.)) %>%
    sf::st_join(geo_points) %>%
    dplyr::rename_with( ~ stringr::str_remove(., ".x")) %>%
    dplyr::select(-c(dplyr::ends_with(".y")))
  
  if(is.null(sum) && count == F) {
    stop('no aggregation inputted', call. = F)
    
  } else if(is.null(sum) && count == T) {
    aggregated_data <- new_data %>%
      dplyr::mutate(count = lengths(sf::st_intersects(., geo_points))) %>%
      sf::st_transform(crs) %>%
      sf::st_set_geometry("geometry") %>%
      dplyr::select(id, count, geometry)
    
  } else if(is.null(sum) != T && count == F) {
    aggregated_data <- new_data %>%
      dplyr::group_by(id) %>%
      dplyr::summarise(sum_calc = sum(!! rlang::sym(sum), na.rm = T)) %>%
      sf::st_transform(crs) %>%
      dplyr::rename(sum = sum_calc) %>%
      dplyr::mutate(!!paste(sum) := sum) %>%
      sf::st_set_geometry("geometry") %>%
      dplyr::select(id, !!paste(sum), geometry) %>% 
      dplyr::distinct(id, .keep_all = T)
    
  } else {
    stop('error in aggregation parameter', call. = F)
  }
  
  aggregated_data
}