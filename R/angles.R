library(cowplot)

sam <- r[sample(1:nrow(r), size = 100, replace = FALSE),]

# Average road angle

angle <- map_df(1:nrow(sam), function(x) {
    cat(crayon::cyan("Starting Road", x, "  "))
    t_ <- sam[x,"geometry"]
    
    # Handle LINESTRINGS
    if (st_geometry_type(t_) == "LINESTRING") {
      cat(crayon::white("LINESTRING\n"))
      
      t_points <- st_cast(t_, "POINT")
      
      if (nrow(t_points) > 2) {
        angles <- map_df(3:nrow(t_points), function(p) {
          seg <- t_points[c((p - 2):p),]
          ab <- st_distance(seg[1,], seg[2,])
          bc <- st_distance(seg[2,], seg[3,])
          ac <- st_distance(seg[1,], seg [3,])
          
          angle <- acos(
            (ac^2 - ab^2 - bc^2) / (-2 * ab * bc)
          )
          
          tibble(
            angle = 180 - (as.numeric(angle) * 180 / pi),
            end_index = p,
          )
          
        })
        
        tot_angle <- sum(angles$angle, na.rm = TRUE)
        dist <- st_distance(t_points[1,], t_points[nrow(t_points),])
        l_length <- st_length(t_)
        point_count <- nrow(t_points)
        
        df <- tibble(dist = dist,
                     l_length = l_length,
                     point_count = point_count,
                     tot_angle = tot_angle,
                     ind = x)
      } else {
        cat(crayon::red("Two or fewer points.\n"))
      }
    }else {
      cat(crayon::red("Not a LINESTRING.\n"))
    } 
})
af <- angle %>%
  mutate(efficiency = as.numeric(dist / l_length),
         angle_rate = tot_angle / dist) 

crow_flies <- function(x) {
  t_ <- st_cast(x, "POINT")
  d_ <- t_[c(1, nrow(t_)),] %>%
    summarise(do_union = FALSE) %>%
    st_cast(., "LINESTRING")
  return(d_)
}

most_ineff <- af %>%
  arrange(efficiency) %>%
  head(5) 

ex <- sam[most_ineff[["ind"]], "geometry"]

most_ang <- af %>%
  arrange(desc(angle_rate)) %>%
  head(5)

ex_ang <- sam[most_ang[["ind"]], "geometry"]

most_eff <- map(1:5, function(x) {
  t <- ex[x,]
  l <- most_ineff[x,]
  in_both <- if (l$ind %in% most_ang$ind) {
    " (Both)"
  } else {
    ""
  }
  
  bb <- st_bbox(t) 
  dx <- bb[["xmax"]] - bb[["xmin"]] 
  dy <- bb[["ymax"]] - bb[["ymin"]]
  newbb <- bb
  diff <- abs(dx - dy) / 2
  
  if (dx > dy) {
    newbb[["ymax"]] <- bb[["ymax"]] + diff
    newbb[["ymin"]] <- bb[["ymin"]] - diff
  } else {
    newbb[["xmax"]] <- bb[["xmax"]] + diff
    newbb[["xmin"]] <- bb[["xmin"]] - diff
  }
  
  t %>%
    ggplot() +
    geom_sf() +
    geom_sf(data = crow_flies(ex[x,]),
                color = "red", linetype = 2) +
    coord_sf(clip = "off", xlim = c(newbb[["xmin"]], newbb[["xmax"]]),
             ylim = c(newbb[["ymin"]], newbb[["ymax"]])) +
    theme_void() +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = .5)) +
    labs(title = paste0("#", x, in_both),
         subtitle = paste0("Efficiency: ", percent(l$efficiency),
                           "\n", round(l$angle_rate*1609), "\u00B0 per mile"),
         caption = paste0("Road ", l$ind, " (", round(l$l_length / 1609, 4), " mi.)"))
})



most_ang_p <- map(1:5, function(x) {
  t <- ex_ang[x,]
  l <- most_ang[x,]
  in_both <- if (l$ind %in% most_ineff$ind) {
    " (Both)"
  } else {
    ""
  }
  
  bb <- st_bbox(t) 
  dx <- bb[["xmax"]] - bb[["xmin"]] 
  dy <- bb[["ymax"]] - bb[["ymin"]]
  newbb <- bb
  diff <- abs(dx - dy) / 2
  
  if (dx > dy) {
    newbb[["ymax"]] <- bb[["ymax"]] + diff
    newbb[["ymin"]] <- bb[["ymin"]] - diff
  } else {
    newbb[["xmax"]] <- bb[["xmax"]] + diff
    newbb[["xmin"]] <- bb[["xmin"]] - diff
  }
  t %>%
    ggplot() +
    geom_sf() +
    geom_sf(data = crow_flies(ex_ang[x,]), 
                color = "red", linetype = 2) +
    coord_sf(clip = "off", xlim = c(newbb[["xmin"]], newbb[["xmax"]]),
             ylim = c(newbb[["ymin"]], newbb[["ymax"]])) +
    theme_void() +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = .5)) +
    labs(title = paste0("#", x, in_both),
         subtitle = paste0("Efficiency: ", percent(l$efficiency),
                        "\n", round(l$angle_rate*1609), "\u00B0 per mile"),
         caption = paste0("Road ", l$ind, " (", round(l$l_length / 1609, 4), " mi.)"))
  
})
  
method_a <- plot_grid(most_eff[[1]],
                      most_eff[[2]],
                      most_eff[[3]],
                      most_eff[[4]],
                      most_eff[[5]],
                      ncol = 5, align = "h", axis = "tb") +
  theme(plot.margin = margin(0, 10, 0, 10))

method_b <- plot_grid(most_ang_p[[1]],
                      most_ang_p[[2]],
                      most_ang_p[[3]],
                      most_ang_p[[4]],
                      most_ang_p[[5]],
                      ncol = 5)+
  theme(plot.margin = margin(0, 10, 0, 10))

title_a <- ggdraw() +
  draw_label(
    "Top 5 Curviest Roads by Method A: Road Efficiency",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.margin = margin(0, 0, 0, 5))
title_b <- ggdraw() +
  draw_label(
    "Top 5 Curviest Roads by Method B: Road Angles",
    fontface = 'bold',
    x = 0,
    hjust = 0
  )+
  theme(plot.margin = margin(0, 0, 0, 5))

method_comp_plot <- plot_grid(title_a, method_a, title_b, method_b,
          ncol = 1, rel_heights = c(.1, 1, .1, 1))

ggsave(method_comp_plot, filename = "maps/method_comp_plot.png", device = "png",
       bg = "white")
