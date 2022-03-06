library(tidyverse)
library(sf)
library(MetBrewer)
library(showtext)
library(ggtext)
library(scales)
library(geomtextpath)

aw_summed <- read_rds("data/aw_summed.rda")
counties_trim <- read_rds("data/counties_trim.rda")

with_sf <- left_join(counties_trim, aw_summed)

greens <- rev(met.brewer("VanGogh3", n = 5)[c(1, 3, 5)])

font_add_google("EB Garamond", "ebg")
font_add_google("Grechen Fuemen", "gf")
showtext_auto()

`%+%` <- function(x, y) paste(x, y)

# I tried overlaying label for Driftless Area,
# but I couldn't get the font family to update.

driftless <- tibble(
  x = c(44.67115648930256, 43.857066870958626, 42.81412214935392),
  y = c(-91.74290467158458, -90.69722864059193, -90.42643672102743),
  label = "Driftless Area"
) %>%
  st_as_sf(coords = c("y", "x"), crs = 4326) %>%
  group_by(label) %>%
  summarise(do_union = FALSE) %>%
  st_cast(., "LINESTRING")

with_sf %>%
  ggplot() +
  geom_sf(aes(fill = group), color = "white", size = .5) +
  scale_fill_manual(values = greens, labels = c("Curviest", "", "Straightest")) +
  theme_void() +
  theme(plot.title = element_text(family = "gf", size = 30, color = greens[1],
                                  margin = margin(t = 5, b = 5), hjust = .5),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(family = "gf", color = greens[1],
                                               size = 20, lineheight = .75, 
                                               fill = alpha(greens[3], .25), 
                                               r = unit(.05, "cm"), box.colour = greens[1],
                                               linetype = 1, 
                                               padding = margin(l = 5,
                                                                t = 3,
                                                                b = 3,
                                                                r = 3)),
        plot.caption = element_textbox_simple(family = "gf", lineheight = .9, 
                                              width = unit(7.5, "in"), size = 12,
                                              margin = margin(b = 5),
                                              color = alpha(greens[1], .75),
                                              fill = alpha(greens[3], .1), 
                                              r = unit(.05, "cm"), 
                                              box.colour = alpha(greens[1], .75),
                                              linetype = 1, linewidth = .1,
                                              padding = margin(rep(3, 4))),
        legend.position = c(.08, .25),
        legend.direction = "vertical",
        legend.title.align = -.5,
        legend.spacing = unit(10, "cm"),
        legend.text = element_text(family = "gf", size = 14)) +
  labs(title = "A Bend in the Road",
       subtitle = "Wisconsin's curviest roads are located in the Driftless Area to the west" %+%
         "and the Northwoods.",
       fill = "",
       caption = "Road curvature calculated as the straight-line distance between" %+%
         "start and end points of a road divided by the actual length of the road." %+%
         "Data from OpenStreetMap, roads limited to Secondary and Tertiary types." %+%
         "Analysis and graphic by Spencer Schien (@MrPecners).") +
  guides(fill = guide_legend(label.position = "left"))


ggsave("maps/final_map.png", width = 8, height = 8, bg = "white")

aw_summed %>%
  arrange(total_eff) %>%
  head(5) %>%
  bind_rows(aw_summed %>%
              arrange(desc(total_eff)) %>%
              head(5)) %>%
  ggplot(aes(reorder(NAME, as.numeric(total_eff)), as.numeric(total_eff))) +
  geom_segment(aes(xend = reorder(NAME, as.numeric(total_eff)),
                   x = reorder(NAME, as.numeric(total_eff)),
                   yend = .80, y = as.numeric(total_eff)),
               linetype = 2, color = greens[2]) +
  geom_point(size = 12, color = greens[1]) +
  geom_text(aes(label = percent(as.numeric(total_eff), 0.5)),
            size = 3.5, color = "white", family = "gf") +
  scale_y_continuous(labels = function(x) percent(x, 1)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        axis.text.x = element_text(family = "gf", size = 14, angle = 20,
                                   color = greens[2]),
        axis.text.y = element_text(color = greens[2]),
        text = element_text(family = "gf"),
        plot.title = element_textbox(family = "gf", size = 24, color = greens[1]),
        plot.subtitle = element_text(color = greens[1], size = 16),
        axis.title.y = element_textbox_simple(size = 16, halign = .5, 
                                              orientation = "left",
                                              lineheight = .8,
                                              color = greens[1]),
        axis.title.x = element_text(size = 16, color = greens[1]),
        plot.caption = element_textbox_simple(color = alpha(greens[1], .75),
                                              size = 10,
                                              margin = margin(t = 5, b = 5)),
        plot.caption.position = "plot") +
  labs(y = "Road Efficiency<br><span style='font-size:10pt'>" %+%
         "(100% is a straight road)</span>",
       x = "Counties",
       title = "Wisconsin counties with the curviest and straightest roads",
       subtitle = "Counties shown represent the top 5 curviest and straightest",
       caption = "Data from OpenStreetMap, roads limited to Secondary and Tertiary types." %+%
         "Analysis and graphic by Spencer Schien (@MrPecners).")

ggsave("maps/top_counties_plot.png", device = "png", bg = "white")
