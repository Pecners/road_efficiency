#eff_start_time1 <- Sys.time()
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
#eff_end_time1 <- Sys.time()
#comment_time <- eff_end_time - eff_start_time


aw_summed <- all_wi %>%
  mutate(efficiency = dist / l_length) %>%
  group_by("NAME" = county) %>%
  summarise(total_eff = sum(dist, na.rm = TRUE) / sum(l_length, na.rm = TRUE)) %>%
  mutate(te = as.numeric(total_eff),
         group = case_when(te < .9 ~ "Curviest",
                           te < .95 ~ "Middle",
                           te < 1 ~ "Straightest"))

saveRDS(aw_summed, "data/aw_summed.rda")
