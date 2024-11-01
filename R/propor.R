#' Data for Wurman-Style Maps
#'
#' Function for creating data for Wurman-style maps, a form of cartogram in which polygons (e.g. circles, hexagons, squares) arranged in a grid are proportionally resized according to a specified variable and visualised along with the original polygons for reference.
#'
#' @param data A spatial dataset consisting of a grid of polygons
#' @param w Variable for weighting the data
#' @param max_val Value used to establish the weighting upper limit for sizing the cartogram polygons. Input either max_val or max_var
#' @param max_var Variable used to establish the weighting upper limit for sizing the cartogram polygons. max_var should not be the same as the weighting (w) parameter. Input either max_var or max_val
#' @param crs Coordinate Reference System
#' @importFrom sf st_as_sf
#' @importFrom sf st_crs
#' @importFrom sf st_sfc
#' @importFrom sf st_geometry
#' @importFrom sf st_centroid
#' @importFrom sf st_join
#' @importFrom sf st_covered_by
#' @importFrom sf st_combine
#' @importFrom dplyr "%>%"
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr rename
#' @importFrom dplyr case_when
#' @importFrom dplyr ends_with
#' @return An sf dataset containing the cartogram polygons and the original polygons.
#' @examples
#' # Load the sf dataset of housing stats
#' data(housing_london)
#'
#' # Generate the data for the Wurman-style map
#' propor(housing_london, w = "new_dwellings", max_var = "lp2021_target")
#' @export

propor <- function(data, w, max_val = NULL, max_var = NULL, crs = NULL) {

  if(grepl(x = class(data)[1], pattern = "sf", ignore.case = T) != T && grepl(x = class(data)[1], pattern = "sp", ignore.case = T) != T) {
    stop('input data as a spatial dataset', call. = F)

  } else {
    df <- data %>%
      st_as_sf() %>%
      dplyr::mutate(polygon = "original")
  }

  if(is.null(max_var) != T && is.null(max_val) != T || is.null(max_var) && is.null(max_val)) {
    stop('input max_val or max_var', call. = F)

  } else if(is.null(max_var) != T && is.null(max_val)) {
    df <- df %>%
      dplyr::mutate(pro_val = df[[w]]/df[[max_var]])

  } else if(is.null(max_var) && is.null(max_val) != T) {
    df <- df %>%
      dplyr::mutate(pro_val = df[[w]]/max_val)

  } else {
    stop('error in establishing the weighting upper limit via max_val or max_var', call. = F)
  }

  df <- df %>%
    dplyr::mutate(pro_val = case_when(pro_val > 1 ~ 1, .default = pro_val),
                  pro_sqrt = sqrt(pro_val))


  polygons <- df %>%
    st_geometry()

  centroids <- lapply(1:length(polygons), function(i) {
    st_centroid(st_combine(polygons[[i]]))
  })

  if(is.null(crs)) {
    crs <- st_crs(df)

  } else {
    crs <- crs
  }

  cells <- lapply(1:length(polygons), function(i) {
    ((st_geometry(polygons[[i]]) - centroids[[i]]) * df$pro_sqrt[[i]] + centroids[[i]]) %>%
      st_sfc(crs = crs) %>%
      st_as_sf()
  })

  cartogram <- do.call(rbind, cells) %>%
    dplyr::mutate(polygon = "cartogram") %>%
    dplyr::rename(geometry = x) %>%
    st_join(df, join = st_covered_by, left = T, suffix = c("", ".y_joineddata")) %>%
    dplyr::select(-c(ends_with(".y_joineddata"))) %>%
    dplyr::filter(!(pro_val == 0 & polygon == "cartogram"))

  new_data <- rbind(df, cartogram) %>%
    dplyr::select(-c(pro_sqrt, pro_val))

  new_data
}
