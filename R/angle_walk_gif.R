library(magick)

ex <- sam[96, "geometry"]

make_seg_gif <- function(x) {
  t_ <- st_cast(x, "POINT")
  basic_p <- x %>%
    ggplot() +
    geom_sf(alpha = .5) +
    theme_void()
  
  walk(3:nrow(t_), function(y) {
    
    d_points <- t_[c((y-2):y),]
    ab <- st_distance(d_points[1,], d_points[2,])
    bc <- st_distance(d_points[2,], d_points[3,])
    ac <- st_distance(d_points[1,], d_points [3,])
    
    angle <- acos(
      (ac^2 - ab^2 - bc^2) / (-2 * ab * bc)
    )
    a <- 180 - (as.numeric(angle) * 180 / pi)
    d_ <- d_points %>%
      summarise(do_union = FALSE) %>%
      st_cast(., "LINESTRING")
    
    p_i <- basic_p +
      geom_sf(data = d_points, color = "red") +
      geom_sf(data = d_, color = "red") + 
      geom_sf_label(data = d_points[2,], aes(label = paste0(round(a), "\u00B0")),
                  size = 4, alpha = .9) +
      coord_sf(clip = "off")
    
    ggsave(p_i, filename = paste0("maps/seg_gif/plot_", y, ".png"), device = "png",
           bg = "white")
    
  })
  
  imgs <- list.files("maps/seg_gif", full.names = TRUE)
  img_list <- lapply(imgs, image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- image_resize(img_joined, '800') %>%
    image_animate(fps = 2)
  
  #img_animated
  
  ## save to disk
  image_write(image = img_animated,
              path = "maps/seg_walk.gif")
  
  
  
}

make_seg_gif(ex)
