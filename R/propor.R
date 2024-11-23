#' Data for Wurman-Style Maps
#'
#' Function for creating data for Wurman-style maps, a mapping technique in which tiles (e.g. circles, hexagons, squares etc) are proportionally resized according to a specified variable and visualised with the original tiles for comparison
#'
#' @param data A spatial dataset (i.e. sf or SpatialPolygonsDataFrame) consisting of polygon tiles with data values
#' @param w The weighting variable
#' @param max_val Value used to establish the upper limit for the new polygons. Input either max_val or max_var
#' @param max_var Variable used to establish the upper limit for the new polygons. max_var should not be the same as the weighting variable. Input either max_var or max_val
#' @param crs Coordinate Reference System
#' @importFrom dplyr "%>%"
#' @return An sf dataset containing the new polygons and the original polygons.
#' @examples
#' # Load the sf dataset of cholera deaths in Soho and pumps
#' data(soho_pumps, cholera_deaths)

#' # First, create hexagonal-shaped tiles
#' tile_data <- propor::tiles(data = soho_pumps, shape = "hexagons")

#' # Next, aggregate the data within the tiles
#' aggregated_data <- propor::aggregate(tile_data = tile_data, geo_points = cholera_deaths, sum = "cholera.deaths", crs = 27700)

#' # Finally, Generate the proportionally-resized tiles
#' propor::propor(aggregated_data, w = "cholera.deaths", max_val = 37)
#' @export

propor <- function(data, w, max_val = NULL, max_var = NULL, crs = NULL) {
  
  if(grepl(x = class(data)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(data)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input data as a spatial dataset', call. = F)
    
  } else {
    data <- data %>%
      sf::st_as_sf() %>%
      dplyr::mutate(polygon = "original")
  }
  
  if(is.null(max_var) != T && is.null(max_val) != T || is.null(max_var) && is.null(max_val)) {
    stop('input max_val or max_var', call. = F)
    
  } else if(is.null(max_var) != T && is.null(max_val)) {
    data <- data %>%
      dplyr::mutate(w_val = data[[w]]/data[[max_var]])
    
  } else if(is.null(max_var) && is.null(max_val) != T) {
    data <- data %>%
      dplyr::mutate(w_val = data[[w]]/max_val)
    
  } else {
    stop('error in establishing the weighting upper limit via max_val or max_var', call. = F)
  }
  
  if(is.null(crs)) {
    crs <- sf::st_crs(data)
    
  } else {
    crs <- crs
  }
  
  data <- data %>%
    dplyr::mutate(w_val = ifelse(w_val > 1, 1, w_val),
                  w_sqrt = sqrt(w_val)) %>%
    sf::st_transform(crs)
  
  polygons <- data %>%
    sf::st_geometry()
  
  centroids <- lapply(1:length(polygons), function(i) {
    sf::st_centroid(sf::st_combine(polygons[[i]]))
  })
  
  new_polygons <- lapply(1:length(polygons), function(i) {
    ((sf::st_geometry(polygons[[i]]) - centroids[[i]]) * data$w_sqrt[[i]] + centroids[[i]]) %>%
      sf::st_sfc(crs = crs) %>%
      sf::st_as_sf()
  })
  
  new_polygons <- do.call(rbind, new_polygons) %>%
    dplyr::mutate(polygon = "new") %>%
    sf::st_join(data, join = sf::st_covered_by, left = T, suffix = c("", ".y_joineddata")) %>%
    dplyr::select(-dplyr::ends_with(".y_joineddata")) %>%
    dplyr::filter(!(w_val == 0 & polygon == "new")) %>%
    dplyr::select(-w_sqrt) %>%
    dplyr::relocate(polygon, everything()) %>%
    sf::st_set_geometry("geometry") %>%
    dplyr::relocate(w_val, .before = geometry)
  
  new_data <- data %>% 
    dplyr::select(-w_sqrt) %>%
    dplyr::mutate(w_val = NA) %>%
    dplyr::relocate(polygon, everything()) %>%
    sf::st_set_geometry("geometry") %>%
    dplyr::relocate(w_val, .before = geometry)
  
  new_data <- rbind(new_data, new_polygons)
  
  new_data
}