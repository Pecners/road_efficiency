start_no_angles <- Sys.time()
no_angles <- map_df(1:nrow(sam), function(x) {
  cat(crayon::cyan("Starting Road", x, "  "))
  t_ <- sam[x,"geometry"]
  
  # Handle LINESTRINGS
  if (st_geometry_type(t_) == "LINESTRING") {
    cat(crayon::white("LINESTRING\n"))
    
    t_points <- st_cast(t_, "POINT")
    
    if (nrow(t_points) > 2) {
      
      dist <- st_distance(t_points[1,], t_points[nrow(t_points),])
      l_length <- st_length(t_)
      point_count <- nrow(t_points)
      
      df <- tibble(dist = dist,
                   l_length = l_length,
                   point_count = point_count,
                   ind = x)
    } else {
      cat(crayon::red("Two or fewer points.\n"))
    }
  } 
})
end_no_angles <- Sys.time()
time_no_angles <- end_no_angles - start_no_angles

start_angles <- Sys.time()
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
  } 
})
end_angles <- Sys.time()
na_time <- end_no_angles - start_angles
