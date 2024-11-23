# propor

### Overview

propor is an R package for creating data for Wurman-style maps, a mapping technique in which tiles (e.g. triangles, hexagons, circles, squares, diamonds) are proportionally resized and visualised with the original tiles for comparison.

### Installation

propor can be installed from Github using devtools:

    library(devtools)
    install_github("lbuk/propor")

### Use

    library(propor)
    library(dplyr)
    library(ggplot2)

    # Load the sf datasets of cholera deaths in Soho and water pumps
    data(soho_pumps, cholera_deaths)

    # Create the tiles
    tile_data <- propor::tiles(data = soho_pumps, shape = "hexagons")

    # Aggregate points-based data within the tiled data
    aggregated_data <- propor::aggregate(tile_data = tile_data, geo_points = cholera_deaths, sum = "cholera.deaths", crs = 27700)

    # Generate the resized tiled data
    proportional <- propor::propor(aggregated_data, w = "cholera.deaths", max_val = 37)

    # Visualise the Wurman-style map
    ggplot(proportional) + 
        geom_sf(aes(fill = polygon), lwd = 0.2, col = "#000000") + 
        scale_fill_manual(values = c('new' = "#fb4968", 'original' = "#eeefef"), breaks = c("new"), labels = c("Cholera Deaths")) +
        geom_sf(data = soho_pumps, aes(color = "#474747"), size = 2) +
        scale_color_manual(values = "#474747", labels = c("Soho Pumps")) +
        labs(title = "Cholera Deaths in Soho", caption = "Data Source: Dr John Snow; Robin Wilson's Blog", fill = "") +
        theme(legend.position = 'top', 
              legend.justification = 0.07,
              legend.direction = "horizontal",
              legend.key.width = unit(0.4, "cm"),
              legend.key.height = unit(0.2, "cm"),
              legend.title = element_blank(),
              panel.background = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              plot.title = element_text(size = 10.75, face = "bold", hjust = 0.085),
              plot.caption = element_text(size = 9, face = "plain", hjust = 1)) +
        guides(color = guide_legend(title = "", nrow = 1, byrow = T, label.position = "bottom", title.position = "top", reverse = F, title.hjust = 0.5, label.vjust = 2.5), 
               fill = guide_legend(nrow = 1, byrow = T, label.position = "bottom", title.position = "top", reverse = F, title.hjust = 0.5, label.vjust = 2.5))


![](https://github.com/lbuk/propor/blob/main/img/example_map.png)

A range of tessellating tile shapes such as triangles, squares, diamonds or hexagons can be made using propor's tiles function, which is partly based on sf's st_make_grid function:

    # Create triangular tiles
    propor::tiles(data = soho_pumps, shape = "triangles") %>% 
        sf::st_geometry() %>% 
        plot()

![](https://github.com/lbuk/propor/blob/main/img/triangle_tiles.png)

    # Create square-shaped tiles
    propor::tiles(data = soho_pumps, shape = "squares") %>% 
        sf::st_geometry() %>% 
        plot()

![](https://github.com/lbuk/propor/blob/main/img/square_tiles.png)

    # Create diamond-shaped tiles
    propor::tiles(data = soho_pumps, shape = "diamonds") %>% 
        sf::st_geometry() %>% 
        plot()

![](https://github.com/lbuk/propor/blob/main/img/diamond_tiles.png)