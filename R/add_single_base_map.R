#' Add single base map or empty background
#'
#' @param map object of class \code{leaflet}, \code{mapview} or \code{tmap}
#' @param group a character string setting the group name of the base map layer
#' @param language a single character string determining the group name in a
#' language if \code{group} is left unspecified. Available choices are
#' \code{"EN"} (English / default), \code{"DE"} (German), \code{"FR"} (French),
#' \code{"IT"} (Italian) or \code{"RM"} (Rhaeto-Romance).
#'
#' @description
#' Add a single base map from swisstopo (some not accessible via
#' leaflet-providers) or empty background as tile (WMS) layer to the map (s. Details).
#'
#' @details
#' \itemize{
#'   \item \code{add_no_base_map()} adds empty background layer, which may be a
#'   useful alternative when base maps distract from overlay layer(s)
#'   \item \code{add_swisstopo_grey()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.pixelkarte-grau&layers_visibility=false&layers_timestamp=18641231&E=2660000.00&N=1190000.00&zoom=4}{SwissFederalGeoportal.NationalMapGrey}
#'   \item \code{add_swisstopo_color()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.pixelkarte-farbe&layers_visibility=false&layers_timestamp=18641231&E=2660000.00&N=1190000.00&zoom=4}{SwissFederalGeoportal.NationalMapColor}
#'   \item \code{add_swissimage()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.swissimage&layers_visibility=false&layers_timestamp=18641231&E=2660000.00&N=1190000.00&zoom=4}{SwissFederalGeoportal.SWISSIMAGE}
#'   \item \code{add_relief()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.swissimage&layers=ch.swisstopo.leichte-basiskarte_reliefschattierung&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458}{Light base map relief}
#'   \item \code{add_swissTLM_color()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.pixelkarte-farbe&layers=ch.swisstopo.swisstlm3d-karte-farbe&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458}{swissTLM-Map (color)}
#'   \item \code{add_swissTLM_grey()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.swissimage&layers=ch.swisstopo.swisstlm3d-karte-grau&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458}{swissTLM-Map (grey)}
#'   \item \code{add_swissimage_1946()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.swissimage&layers=ch.swisstopo.swissimage-product_1946&layers_timestamp=1946&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458,485}{SWISSIMAGE HIST 1946} orthophotomosaic of Switzerland from 1946 / "US flight mission"
#'   \item \code{add_siegfried_map()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.swissimage&layers=ch.swisstopo.hiks-siegfried&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458}{Siegfried Map 1st edtiton} (1870 - 1926)
#'   \item \code{add_dufour_map()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.swissimage&layers=ch.swisstopo.hiks-dufour&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,532,458,599}{Dufour Map 1st edtiton} (1844 - 1864)
#'   \item \code{add_hillshade()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.swissimage&layers=ch.swisstopo.swissalti3d-reliefschattierung_monodirektional&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458,508}{swissALTI3D monodirectional Hillshade}
#'   \item \code{add_hillshade_2()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.swissimage&layers=ch.swisstopo.swissalti3d-reliefschattierung&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458,508}{swissALTI3D multidirectional Hillshade}
#'   \item \code{add_relief_nfi()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.swissimage&layers=ch.bafu.landesforstinventar-vegetationshoehenmodell_relief&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,532,458,599}{Relief shading of vegetation height model NFI} (National Forest Inventory)
#'   \item \code{add_national_map_10_color()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.pixelkarte-farbe&E=2660000.00&N=1190000.00&zoom=4&layers=ch.swisstopo.landeskarte-farbe-10}{National Map 1:10'000 (color)}
#'   \item \code{add_national_map_10_grey()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.pixelkarte-farbe&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458&layers=ch.swisstopo.landeskarte-grau-10}{National Map 1:10'000 (grey)}
#'   \item \code{add_national_map_25_color()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.pixelkarte-farbe&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458&layers=ch.swisstopo.pixelkarte-farbe-pk25.noscale}{National Map 1:25'000 (color)}
#'   \item \code{add_national_map_50_color()} \href{https://map.geo.admin.ch/?lang=en&topic=ech&bgLayer=ch.swisstopo.pixelkarte-farbe&E=2660000.00&N=1190000.00&zoom=4&catalogNodes=457,458&layers=ch.swisstopo.pixelkarte-farbe-pk50.noscale}{National Map 1:50'000 (color)}
#' }
#'
#' @return the new map as object of the class \code{leaflet}
#'
#' @importFrom leaflet addTiles addWMSTiles WMSTileOptions
#' @importFrom tmap tmap_leaflet
#'
#' @examples
#' library(dplyr)
#' library(sf)
#' library(mapview)
#' library(tmap)
#' library(leaflet)
#'
#' # with leaflet.CH.basemaps available base maps:
#' base_maps_language(language = "EN")
#'
#' # check what they look like:
#' all_base_maps <- base_maps()$base_map
#' mapview(lakes[10,], layer.name = "some lake", legend = FALSE) %>%
#'   add_base_maps(baseGroups = all_base_maps) %>%
#'   setView(8, 46.75, 13)
#'
#' # link / popup as new attributes
#' waldperlen <-
#'   waldperlen %>%
#'   mutate(
#'     link = paste0("<b><a href='", flyer,"'>", bezeichnung, "</a></b>"),
#'     popup = paste(link, thema, sep = "<br/>")
#'   )
#'
#' # a leaflet obj.
#' waldperlen_markers <-
#'   leaflet() %>%
#'   addMarkers(data = st_transform(waldperlen, 4326), popup = ~popup, group = "waldperlen")
#'
#' # add a base map form leaflet.CH.basemaps (no layers control)
#' waldperlen_markers %>% add_relief()
#'
#' # add multiple base maps from leaflet.CH.basemaps & other sources with
#' # user defined group names & sequence of base layers in control panel
#' waldperlen_markers %>%
#'   add_relief(group = "light relief") %>% #'
#'   add_swissimage(group = "current swissimage") %>%
#'   addProviderTiles("Stadia.StamenWatercolor", group = "watercolor") %>%
#'   addLayersControl(
#'     overlayGroups = "waldperlen",
#'     baseGroups    = c("watercolor", "light relief", "current swissimage"),
#'     options       = leaflet::layersControlOptions(collapsed = FALSE)
#'   )
#'
#' # for usage of language-wise default group names in layers control knowing them
#' # is precondition
#' base_maps_language(language = "DE") # get german group names
#'
#' # add multiple base maps from leaflet.CH.basemaps having german defalut group
#' # names + provider tiles
#' waldperlen_markers %>%
#'   add_swissTLM_color(language = "DE") %>%
#'   add_swisstopo_grey(language = "DE") %>%
#'   addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
#'   # group requiered --> else addLayersControl() wont work correctly
#'   addLayersControl(
#'     overlayGroups = "waldperlen",
#'     baseGroups    = c("OpenTopoMap",  "swisstTLM farbig", "swisstopo grau"),
#'     options       = leaflet::layersControlOptions(collapsed = FALSE)
#'   )
#'
#' # a mapview obj
#' m <-
#'   mapview(
#'     cantons,
#'     legend        = FALSE,
#'     color         = "red",
#'     lwd           = 1.5,
#'     alpha.regions = 0,
#'     hide          = TRUE,
#'     map.types     = c("OpenStreetMap", "Stadia.StamenTonerLite")
#'   ) +
#'   mapview(
#'     lakes,
#'     legend        = FALSE,
#'     col.region    = "cyan",
#'     alpha.regions = 1
#'   )
#' # m # view mapview
#' # use mapview obj. & inherit its base maps + add base maps form leaflet.CH.basemaps
#' m %>%
#'   add_relief() %>%  # group names determined by default language (EN)
#'   add_no_base_map() %>%
#'   addLayersControl(
#'     overlayGroups = c("cantons", "lakes"),
#'     baseGroups    = rev(c( "Stadia.StamenTonerLite", "relief", "OpenStreetMap", "no base map")),
#'     options       = leaflet::layersControlOptions(collapsed = FALSE)
#'   ) %>%
#'   leaflet.extras::setMapWidgetStyle(list(background = "black"))
#'
#' # a tmap obj. with its base maps
#' gl <- subset(cantons, name == "Glarus")
#' tm <-
#'   tm_shape(gl) + tm_borders(group = "Kt. Glarus", lwd = 2, col = "red") +
#'   tm_shape(waldperlen[,c("bezeichnung", "thema")]) +
#'   tm_bubbles(col = "red", group = "Waldperlen", size = 0.5) +
#'   tm_basemap(c("OpenStreetMap", "CyclOSM"))
#' # tmap_leaflet(tm) # view interactive version of tmap
#' # use tmap obj. & inherit its base maps + add base maps form leaflet.CH.basemaps
#' tm %>%
#'   add_relief(language = "DE") %>%
#'   add_swissimage(group = "SWISSIMAGE") %>%
#'   leaflet::addLayersControl(
#'     baseGroups    = c("Relief", "CyclOSM", "SWISSIMAGE", "OpenStreetMap"),
#'     overlayGroups = c("Waldperlen", "Kt. Glarus"),
#'     options       = leaflet::layersControlOptions(collapsed = FALSE)
#'   )
#' @name add_single_base_map
#' @export
add_no_base_map <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("no_base_map", map, group, language)
}
#' @name add_single_base_map
#' @export
add_swisstopo_grey  <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("swisstopo_grey", map, group, language)
}
#' @name add_single_base_map
#' @export
add_swisstopo_color <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("swisstopo_color", map, group, language)
}
#' @name add_single_base_map
#' @export
add_swissimage <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("swissimage", map, group, language)
}
#' @name add_single_base_map
#' @export
add_relief <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("relief", map, group, language)
}
#' @name add_single_base_map
#' @export
add_swissTLM_grey <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("swissTLM_grey", map, group, language)
}
#' @name add_single_base_map
#' @export
add_swissTLM_color <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("swissTLM_color", map, group, language)
}
#' @name add_single_base_map
#' @export
add_swissimage_1946 <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("swissimage_1946", map, group, language)
}
#' @name add_single_base_map
#' @export
add_siegfried_map <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("siegfried_map", map, group, language)
}
#' @name add_single_base_map
#' @export
add_dufour_map <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("dufour_map", map, group, language)
}
#' @name add_single_base_map
#' @export
add_hillshade <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("hillshade", map, group, language)
}
#' @name add_single_base_map
#' @export
add_hillshade_2 <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("hillshade_2", map, group, language)
}
#' @name add_single_base_map
#' @export
add_relief_nfi <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("relief_nfi", map, group, language)
}
#' @name add_single_base_map
#' @export
add_national_map_10_color <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("national_map_10_color", map, group, language)
}
#' @name add_single_base_map
#' @export
add_national_map_10_grey <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("national_map_10_grey", map, group, language)
}
#' @name add_single_base_map
#' @export
add_national_map_25_color <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("national_map_25_color", map, group, language)
}
#' @name add_single_base_map
#' @export
add_national_map_50_color <- function(map, group, language = c("EN", "DE", "FR", "IT", "RM")){
  language <- match.arg(language)
  add_1_base_map("national_map_50_color", map, group, language)
}
# helper functions for pkg-internal use
check_map <- function(map_) {
  if (!inherits(map_, c("leaflet", "mapview", "tmap"))) {
    stop("map must be of an object of the class leaflet, mapview or tamp", call. = FALSE)
  }
}
as_leaflet <- function(map_) {
  if (inherits(map_, "mapview")) {
    map_@map
  } else if (inherits(map_, "tmap")) {
    tmap::tmap_leaflet(map_)
  } else {
    map_
  }
}

add_1_base_map <- function(base_map, map, group, language){
  check_map(map)
  map <- as_leaflet(map)
  info <- base_maps_df()[base_map, ]
  if(missing(group)){
    group <- info[[language]]
  } else {
    if(!(length(group) == 1 & is.character(group) & is.vector(group))){
      stop("if specified, the argument group must be a single character string.", call. = FALSE)
    }
  }
  if (base_map == "no_base_map") {
    attribution <- NULL
  } else {
    attribution <- '&copy; <a href="https://www.swisstopo.admin.ch/">swisstopo</a>'
  }
  if(!is.na(info$urlTemplate)){
    leaflet::addTiles(
      map         = map,
      urlTemplate = info$urlTemplate,
      group       = group,
      attribution = attribution
    )
  } else {
    leaflet::addWMSTiles(
      map         = map,
      baseUrl     = info$baseUrl,
      layers      = info$layers,
      options     = leaflet::WMSTileOptions(format = "image/png", transparent = TRUE),
      group       = group,
      attribution = attribution
    )
  }
}
