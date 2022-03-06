library(tidyverse) 
library(sf)

#motorways <- read_rds("../wi_roads/data/motorway.rda")
#trunk <- read_rds("../wi_roads/data/trunk.rda")
#primary <- read_rds("../wi_roads/data/primary.rda")
secondary <- read_rds("../wi_roads/data/secondary.rda")
tertiary <- read_rds("../wi_roads/data/tertiary.rda")

roads <- list(
  #motorways = motorways$osm_lines,
  #trunk = trunk$osm_lines,
  #primary = primary$osm_lines,
  secondary = secondary$osm_lines,
  tertiary = tertiary$osm_lines
)

set_crs <- st_crs(roads$secondary)

r <- bind_rows(roads$secondary, roads$tertiary)
