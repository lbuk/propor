#' Build Semi-Regular Tessellating Tile Data
#'
#' Function for creating semi-regular tessellating tiles including: a combination of circles and hexagons. Since the circles and hexagons tessellate when combined, they do not contain gaps unlike configurations that consist of only circles.
#'
#' @param data A spatial dataset (i.e. sf or sp) for defining the bounding box
#' @param shape Shapes of the tiles to make. The only option currently is "circles_hexagons"
#' @param crs Coordinate Reference System
#' @importFrom dplyr "%>%"
#' @return An sf dataset containing the semi-regular tessellating tiles.
#' @examples
#' # Load the sf dataset of Soho pumps
#' data(soho_pumps)
#'
#' # Generate the tiles
#' propor::semiregular(data = soho_pumps)
#' @export

semiregular <- function(data, shape = "circles_hexagons", crs = NULL) {
  
  if(grepl(x = class(data)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(data)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input data as a spatial dataset', call. = F)
    
  } else {
    data <- data %>%
      sf::st_as_sf()
  }
  
  if(is.null(crs)) {
    crs <- sf::st_crs(data)
    
  } else {
    crs <- crs
  }
  
  if(shape == "circles_hexagons") {
    tile_hexagons <- sf::st_make_grid(data, square = F) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(id = dplyr::row_number(.),
                    area_polygon = as.numeric(sf::st_area(.)),
                    side = sqrt(area_polygon / ((3/2) * sqrt(3))),
                    inner_radius = ((side * sqrt(3)) / 2))
    
    hexagon_centroids <- tile_hexagons %>%
      sf::st_set_agr("constant") %>%
      sf::st_centroid()
    
    circles_list <- lapply(1:nrow(hexagon_centroids), function(i) {
      hexagon_centroids[i,] %>%
        sf::st_buffer(hexagon_centroids[i,]$inner_radius)
    })
    
    tile_circles <- do.call(rbind, circles_list) %>%
      dplyr::mutate(id = dplyr::row_number(.)) %>%
      sf::st_transform(crs) %>%
      sf::st_set_geometry("geometry") %>%
      dplyr::mutate(shape = "circle") %>%
      dplyr::select(id, shape, geometry)
    
    tile_hexagons <- tile_hexagons %>%
      sf::st_transform(crs) %>%
      sf::st_set_geometry("geometry") %>%
      dplyr::mutate(shape = "hexagon") %>%
      dplyr::select(id, shape, geometry)
    
    sr_tessellation <- rbind(tile_hexagons, tile_circles)
    
  } else {
    stop('error in shape parameter', call. = F)
  }
  
  sr_tessellation
}
