library(tidyverse)
library(sf)
library(showtext)
library(MetBrewer)
library(ggtext)

`%+%` <- function(x, y) paste(x, y)

counties <- tigris::counties(state = "WI")

l <- rnaturalearth::ne_download(type = "lakes", category = "physical", scale = "large")  %>%
  st_as_sf(., crs = set_crs)

gl <- l %>% 
  filter(name %in% c("Lake Michigan", "Lake Superior")) %>%
  st_union()

co <- st_transform(counties, crs = set_crs)

counties_trim <- st_difference(co, gl)

font_add_google("EB Garamond", "ebg")
font_add_google("Grechen Fuemen", "gf")
showtext_auto()

admin_p <- counties %>%
  ggplot() +
  geom_sf() +
  theme_void() +
  theme(plot.title = element_text(family = "ebg", hjust = .5)) +
  labs(title = "Administrative County Boundaries")

no_lakes_p <- counties_trim %>%
  ggplot() +
  geom_sf() +
  theme_void() +
  theme(plot.title = element_text(family = "ebg", hjust = .5)) +
  labs(title = "County Boundaries with Great Lakes Erased")

m <- plot_grid(admin_p, no_lakes_p, align = "h", axis = "t")

ggsave(m, filename = "maps/county_bounds_plot.png", device = "png",
              bg = "white")

