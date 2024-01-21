# packages ####
library(dplyr)
library(stringr)
library(sf)
library(mapview)

# paths ####
# getwd()
path_outside <- getwd() %>% dirname()
path_data    <- file.path(path_outside, "data_4_leaflet.CH.basemaps")
if(!dir.exists(path_data)){dir.create(path_data)}
path_data

# download & unzip ####
# manually: download into above created folder and then unzip manually

# swissboundaries3d_2023-01_2056_5728.gpkg.zip from
# https://www.swisstopo.admin.ch/en/geodata/landscape/boundaries3d.html

# swisstlm3d_2023-03_2056_5728.gpkg.zip from
# https://www.swisstopo.admin.ch/en/geodata/landscape/tlm3d.html

# waldreservate.zip from
# https://map.geo.gl.ch/data/waldreservate.zip

# dns ####
dsn_boundaries <- file.path(path_data, "swissBOUNDARIES3D_1_5_LV95_LN02.gpkg")
file.exists(dsn_boundaries)

dsn_tlm3d <- file.path(path_data, "SWISSTLM3D_2023_LV95_LN02.gpkg")
file.exists(dsn_tlm3d)

dsn_gl_waldreservate <- file.path(path_data, "waldreservate", "geopackage", "pub_gl_waldreservate.gpkg")
file.exists(dsn_gl_waldreservate)

# manipulate data ####
## cantons ####
st_layers(dsn_boundaries)
cantons <-
  st_read(dsn_boundaries, "tlm_kantonsgebiet", quiet = TRUE) %>%
  st_make_valid() %>%
  st_zm() %>%
  select(kantonsnummer, name, see_flaeche, kantonsflaeche, einwohnerzahl) %>%
  arrange(kantonsnummer)
mapview(cantons)
# usethis::use_data(cantons, overwrite = TRUE)
# usethis::use_r("cantons")

## lakes ####
st_layers(dsn_tlm3d)
query_lakes <- 'SELECT * FROM "tlm_bb_bodenbedeckung" WHERE "objektart" = "Stehende Gewaesser"'
lakes <-
  st_read(dsn_tlm3d, query = query_lakes, quiet = TRUE) %>%
  st_make_valid() %>%
  st_zm() %>%
  filter(., st_area(.) >= units::set_units(2.5, "km^2"))
nrow(lakes) # 36

lakes_2_drop <- c("Grimselsee",  "Lac de la Gruyère", "Lac d'Emosson", "Lac des Dix", "Lago di Lei", "Lago di Livigno", "Schiffenensee", "Wohlensee")

query_lake_sides <- 'SELECT * FROM "tlm_gewaesser_stehendes_gewaesser" WHERE "objektart" = "See"'
lake_sides <-
  st_read(dsn_tlm3d, query = query_lake_sides, quiet = TRUE) %>%
  st_make_valid() %>%
  st_zm() %>%
  filter(
    !is.na(name),
    str_detect(name, paste(lakes_2_drop, collapse = "|"), negate = TRUE)
  ) %>%
  st_filter(lakes) %>%
  group_by(name) %>%
  summarise()
nrow(lake_sides) # 27
# sort(lake_sides$name)

lakes <-
  st_join(lakes[, NULL], lake_sides) %>%
  filter(!is.na(name))
nrow(lakes) # 28
n_distinct(lakes$name) # 27
lakes$name %>% .[duplicated(.)]
# --> 2 polygons for Bodensee
mapview(lakes, legend = FALSE, col.region = "cyan")
# usethis::use_data(lakes, overwrite = TRUE)
# usethis::use_r("lakes")

## waldperlen ####
st_layers(dsn_gl_waldreservate)
waldperlen <- st_read(dsn_gl_waldreservate ,"waldperlen", quiet = TRUE)
mapview(waldperlen)
# usethis::use_data(waldperlen, overwrite = TRUE)
# usethis::use_r("waldperlen")
