#' Build Regular Tessellating Tile Data
#'
#' Function for creating simple meshes of regular tessellating map tiles - including triangles, hexagons, squares or diamonds - based on sf's st_make_grid function.
#'
#' @param data A spatial dataset (i.e. sf or sp) for defining the bounding box
#' @param shape Shape of the tiles to make. Select either "triangles", "hexagons", "squares" or "diamonds"
#' @param crs Coordinate Reference System
#' @importFrom dplyr "%>%"
#' @return An sf dataset containing the tiles.
#' @examples
#' # Load the sf dataset of Soho pumps
#' data(soho_pumps)
#'
#' # Generate hexagonal tiles
#' propor::tiles(data = soho_pumps, shape = "hexagons")
#' 
#' # Generate triangular tiles
#' propor::tiles(data = soho_pumps, shape = "triangles")
#' @export

tiles <- function(data, shape, crs = NULL) {
  
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
    
    data <- data %>%
      sf::st_transform(crs)
  }
  
  if(shape == "squares" || shape == "hexagons" && shape != "triangles" && shape != "diamonds") {
    if(shape == "squares" && shape != "hexagons") {
      tiles <- data %>% 
        sf::st_make_grid(., square = T) %>% 
        sf::st_as_sf()
      
    } else {
      tiles <- data %>% 
        sf::st_make_grid(., square = F) %>% 
        sf::st_as_sf()
    }
    
  } else if(shape == "triangles" && shape != "hexagons" && shape != "squares" && shape != "diamonds") {
    extend_bb <- data %>% 
      sf::st_make_grid(., square = F) %>% 
      sf::st_as_sf()
    
    tiles <- extend_bb %>% 
      sf::st_make_grid(., square = F) %>% 
      sf::st_as_sf()
    
    coords <- tiles %>% 
      sf::st_as_sf() %>%
      sf::st_centroid() %>% 
      sf::st_coordinates() %>% 
      as.data.frame()
    
    x <- coords$X
    y <- coords$Y
    
    delaunay <- deldir::deldir(x, y)
    
    coords_list <- deldir::triang.list(delaunay)
    
    triangles_list <- lapply(1:length(coords_list), function(i) {
      coords_list[[i]] %>%
        sf::st_as_sf(coords = c("x", "y")) %>%
        dplyr::summarise(geometry = sf::st_combine(geometry)) %>%
        sf::st_cast("POLYGON")
    })
    
    tiles <- do.call(rbind, triangles_list) %>%
      dplyr::mutate(perimeter = sf::st_perimeter(.)) %>%
      dplyr::filter(perimeter >= (median(perimeter) * 0.9) & perimeter <= (median(perimeter) * 1.1)) %>%
      sf::st_set_crs(crs) %>% 
      dplyr::select(geometry) %>% 
      dplyr::mutate(id = dplyr::row_number(.)) %>% 
      sf::st_set_agr("constant")
    
    square_bb <- propor::tiles(data = data, shape = "squares") %>% 
      sf::st_set_agr("constant")
    
    cropped_ids <- sf::st_crop(tiles, square_bb) %>% 
      sf::st_set_agr("constant") %>%
      dplyr::select(id) %>% 
      as.data.frame()
    
    tiles <- tiles %>%
      dplyr::filter(id %in% cropped_ids$id) %>%
      dplyr::select(-id)
    
  } else if(shape == "diamonds" && shape != "hexagons" && shape != "squares" && shape != "triangles") {
    tiles <- data %>% 
      sf::st_make_grid(., square = F) %>% 
      sf::st_as_sf() 
    
    points <- sf::st_cast(tiles[1,], "POINT")
    
    distance <- sf::st_distance(points[1,], points[2,])
    
    coords <- sf::st_coordinates(tiles) %>% 
      as.data.frame()
    
    hexagons_i <- coords %>%
      dplyr::mutate(Y = Y + units::drop_units(distance[[1]])) %>%
      dplyr::mutate(Y = Y + units::drop_units(distance[[1]])) %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = crs) %>%
      dplyr::group_by(L2) %>% 
      dplyr::summarise(do_union = F) %>% 
      sf::st_cast("LINESTRING") %>% 
      sf::st_cast("POLYGON") %>% 
      sf::st_set_agr("constant")
    
    diamonds_i <- sf::st_intersection(hexagons_i, tiles) %>% 
      dplyr::filter(sf::st_geometry_type(.) == "POLYGON") %>% 
      dplyr::select(-L2)
    
    hexagons_ii <- coords %>%
      dplyr::mutate(Y = Y - units::drop_units(distance[[1]])) %>%
      sf::st_as_sf(coords = c("X", "Y"), crs = crs) %>%
      dplyr::group_by(L2) %>% 
      dplyr::summarise(do_union = F) %>% 
      sf::st_cast("LINESTRING") %>% 
      sf::st_cast("POLYGON") %>%
      sf::st_set_agr("constant")
    
    diamonds_ii <- sf::st_intersection(hexagons_ii, tiles) %>% 
      dplyr::filter(sf::st_geometry_type(.) == "POLYGON") %>% 
      dplyr::select(-L2)
    
    tiles <- dplyr::bind_rows(diamonds_ii, diamonds_i) %>%
      sf::st_set_precision(1) %>%
      sf::st_intersection() %>% 
      dplyr::filter(sf::st_geometry_type(.) == "POLYGON") %>% 
      dplyr::mutate(area = as.numeric(sf::st_area(.))) %>% 
      dplyr::filter(area > ((median(area)) / 10000)) %>%
      dplyr::select(geometry)
    
  } else {
    stop('error in shape parameter', call. = F)
  }
  
  new_tiles <- tiles %>%
    dplyr::mutate(id = dplyr::row_number(.)) %>%
    sf::st_set_geometry("geometry") %>%
    dplyr::select(id, geometry)
  
  new_tiles
  
}