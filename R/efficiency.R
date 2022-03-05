eff_start_time1 <- Sys.time()
all_wi <- map_df(1:nrow(counties), function(c) {
  one_c <- co[c,] 
  county <- one_c$NAME
  
  p_county <- st_intersection(r, one_c)
  
  temp_ <- map_df(1:nrow(p_county), function(x) {
    cat(crayon::cyan("Starting County", c, "Road", x, "  "))
    t_ <- p_county[x,"geometry"]
    
    # Extract LINESTRINGS from GEOMETRYCOLLECTION
    if (st_geometry_type(t_) == "GEOMETRYCOLLECTION") {
      t_ <- st_collection_extract(t_,type = c("LINESTRING"))
    }
    
    if (st_geometry_type(t_) == "MULTILINESTRING") {
      t_ <- st_cast(t_, "LINESTRING")
    }
    
    # Handle LINESTRINGS
    if (st_geometry_type(t_[1,]) == "LINESTRING") {
      cat(crayon::white(paste0(st_geometry_type(t_), "\n")))
      
      map_df(1:nrow(t_), function(ml) {
        sub_l <- t_[ml,]
        t_points <- st_cast(sub_l, "POINT")
        
        dist <- st_distance(t_points[1,], t_points[nrow(t_points),])
        l_length <- st_length(sub_l)
        point_count <- nrow(t_points)
        
        df <- tibble(dist = dist,
                     l_length = l_length,
                     point_count = point_count,
                     county = county)
      })
      
    } else {
      cat(crayon::red(paste0("OTHER GEOMETRY: ", st_geometry_type(t_), "\n")))
    }
  })
  
  return(temp_)
})
eff_end_time1 <- Sys.time()

comment_time <- eff_end_time - eff_start_time

c <- 1
x <- 65


aw_summed <- all_wi %>%
  mutate(efficiency = dist / l_length) %>%
  group_by("NAME" = county) %>%
  summarise(total_eff = sum(dist, na.rm = TRUE) / sum(l_length, na.rm = TRUE)) %>%
  mutate(te = as.numeric(total_eff),
         group = case_when(te < .9 ~ "Curviest",
                           te < .95 ~ "Middle",
                           te < 1 ~ "Straightest"))

saveRDS(aw_summed, "data/aw_summed.rda")
with_sf <- left_join(counties_trim, aw_summed)

greens <- rev(met.brewer("VanGogh3", n = 5)[c(1, 3, 5)])

with_sf %>%
  ggplot(aes(fill = group)) +
  geom_sf(color = "white", size = .5) +
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
  labs(fill = "") +
  guides(fill = guide_legend(label.position = "left"))

  labs(title = "A Bend in the Road",
       subtitle = "Wisconsin's curviest roads are located in the Driftless Area to the west" %+%
         "and the Northwoods.",
       fill = "",
       caption = "Road curvature calculated as the straight-line distance between" %+%
         "start and end points of a road divided by the actual length of the road." %+%
         "Data from OpenStreetMap, roads limited to Secondary and Tertiary types." %+%
         "Analysis and graphic by Spencer Schien (@MrPecners).") +
  guides(fill = guide_legend(label.position = "left"))

ggsave("maps/not_text_no_bg_map.png", width = 8, height = 8, bg = "transparent")
