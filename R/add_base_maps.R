#' Add multiple tile layers
#'
#' @param map object of class \code{leaflet}, \code{mapview} or \code{tmap}
#' @param overlayGroups character vector where each element is the name of a
#' group. The user can turn each overlay group on or off independently. If
#' unspecified and the input \code{map} includes a layers control, all
#' \code{overlayGroups} are inherited. If the input comes without layers control
#' then \code{overlayGroups} must be specified (s. Details).
#' @param hideGroup optional character vector of one or more group names
#' (\code{overlayGroups}) to hide
#' @param showGroup optional character vector of one or more group names
#' (of by default hidden \code{overlayGroups}) to show
#' @param baseGroups character vector inducing names of swisstopo base maps
#' and/or empty background the user chooses to add as (WMS) tile layer. All base
#' map layers available with functions of the type \code{\link{add_single_base_map}}
#' are selectable. Default selection: \code{"no_base_map"}, \code{"swisstopo_grey"},
#' \code{"swisstopo_color"}, \code{"swissimage"}, \code{"relief"}.
#' @param baseGroupNames if specified a character vector having the same length
#' as \code{baseGroups} giving each base map a user defined group name.
#' @param language a single character string determining the group names of the
#' selected base map in a language. Available choices are \code{"EN"} (English
#' / default), \code{"DE"} (German), \code{"FR"} (French), \code{"IT"} (Italian)
#' or \code{"RM"} (Rhaeto-Romance). \code{language} is only effective if
#' \code{group} is left unspecified.
#' @param bg_col background color. default \code{white}
#' @param collapsed_layer_control if \code{TRUE}, the layers control will be
#' rendered as an icon that expands when hovered over. Set to \code{FALSE}
#' (default) to have the layers control always appear in its expanded state.
#' @param position_layer_control position of layers control:
#' \code{"topright"} (default), \code{"bottomright"}, \code{"bottomleft"} or
#' \code{"topleft"}
#' @param  keep_baseGroups if \code{TRUE} keep base layers included in \code{map},
#' if \code{FALSE} (default) drop them (s. Details).
#'
#' @description
#' Add a selection of base maps from swisstopo (some not accessible via
#' leaflet-providers) and/or empty background as tile (WMS) layer to the map
#' (s. \code{\link{add_single_base_map}}).
#'
#' @details If the input \code{map} is of the the class \code{mapview} or
#' \code{tmap}, \code{add_base_maps()} converts the input into a \code{leaflet}
#' object.
#'
#' If the input \code{map} contains a layers control, which is always the case
#' for \code{mapview} and \code{tmap} objects, but does not apply for all
#' \code{leaflet} objects, all overlay layers are inherited by default. Furthermore
#' an existing layers control allows to inherit all base layers by setting the
#' argument \code{keep_baseGroups} to \code{TRUE}. In the returned layer control,
#' kept base layers are listed below those that \code{add_base_maps()} appends.
#' To achieve an alternative sequence of base layers in the layers control display
#' use \code{\link{add_single_base_map}} in combination with
#' \code{\link[leaflet]{addLayersControl}}.
#'
#' @return the new map as object of the class \code{leaflet}
#'
#' @seealso \code{\link{add_single_base_map}}, \code{\link{base_maps}}
#'
#' @examples
#' library(dplyr)
#' library(sf)
#' library(mapview)
#' library(tmap)
#' library(leaflet)
#'
#' # link / popup as new attributes
#' waldperlen <-
#'   waldperlen %>%
#'   mutate(
#'     link = paste0("<b><a href='", flyer, "'>", bezeichnung, "</a></b>"),
#'     popup = paste(link, thema, sep = "<br/>")
#'   )
#'
#' # a leaflet obj.
#' waldperlen_markers <-
#'   leaflet() %>%
#'   addMarkers(data = st_transform(waldperlen, 4326), popup = ~popup, group = "waldperlen")
#'
#' # applying add_base_maps() to a map obj. having no layers control ...
#' try(waldperlen_markers %>% add_base_maps())
#' # ... requires adjusting the default setting
#' waldperlen_markers %>% add_base_maps(overlayGroups = "waldperlen")
#'
#' # alter more of the default settings
#' waldperlen_markers %>%
#'   add_base_maps(
#'     overlayGroups = "waldperlen",
#'     baseGroups    = c("swissTLM_color", "relief", "swissimage", "swisstopo_color"),
#'     language      = "IT",
#'     collapsed_layer_control = TRUE,
#'     position_layer_control  = "bottomleft"
#'     )
#'
#' # user defined names for base maps
#' waldperlen_markers %>%
#'   add_base_maps(
#'     overlayGroups  = "waldperlen",
#'     baseGroups     = c("siegfried_map", "swisstopo_color", "swissimage_1946"),
#'     baseGroupNames = c("base map A", "base map B", "base map C")
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
#'     map.types     = c("Stadia.StamenTonerLite", "OpenStreetMap")
#'   ) +
#'   mapview(
#'     lakes,
#'     legend        = FALSE,
#'     col.region    = "cyan",
#'     alpha.regions = 1
#'   )
#' # m # view mapview
#'
#' # use add_base_maps() on a mapview obj. & keeping its base maps
#' m %>%
#'   add_base_maps(
#'     baseGroups      = c("relief", "no_base_map", "swissTLM_color"),
#'     # showGroup       = "cantons", # undo inherited layers hiding
#'     keep_baseGroups = TRUE,
#'     bg_col          = "black"
#'   )
#'
#' # a tmap obj. with a base map
#' gl <- subset(cantons, name == "Glarus")
#' bb <- st_bbox(gl) + c(0, -7, 3, 0) * 1000
#' tm <-
#'   tm_shape(lakes[gl, ], bbox = bb) +
#'   tm_polygons(col = "cyan", group = "Glarner Seen") +
#'   tm_shape(gl) +
#'   tm_borders(group = "Kt. Glarus", lwd = 2, col = "red") +
#'   tm_shape(waldperlen[, c("bezeichnung", "thema")]) +
#'   tm_bubbles(col = "red", group = "Waldperlen", size = 0.5) +
#'   tm_basemap("Stadia.StamenWatercolor", group = "wasserfarben")
#' # tmap_leaflet(tm) # view interactive version of tmap
#'
#' # use add_base_maps() on a tmap obj. & drop by default its base maps
#' tm %>%
#'   add_base_maps(
#'     baseGroups = c("relief", "swisstopo_grey", "swissimage", "hillshade", "swissTLM_color"),
#'     language   = "DE",
#'     hideGroup  = "Glarner Seen", # hide overlay layer shown by default
#'     # keep_baseGroups        = TRUE,
#'     position_layer_control = "bottomright"
#'   )
#'
#' @importFrom leaflet addLayersControl clearGroup hideGroup layersControlOptions
#' showGroup
#' @importFrom leaflet.extras setMapWidgetStyle
#'
#' @export
add_base_maps <- function(
    map,
    overlayGroups,
    hideGroup,
    showGroup,
    baseGroups = c("no_base_map", "swisstopo_grey", "swisstopo_color", "swissimage", "relief"),
    baseGroupNames,
    language   = c("EN", "DE", "FR", "IT", "RM"),
    bg_col     = "white",
    collapsed_layer_control = FALSE,
    position_layer_control  = c("topright", "bottomright", "bottomleft", "topleft"),
    keep_baseGroups = FALSE
){
  check_map(map)
  base_maps <- base_maps_df()[["base_map"]]
  no_base_map <- setdiff(baseGroups, base_maps)
  if(length(no_base_map) > 0){
    stop(
      paste0(
        paste(no_base_map, collapse = ", "),
        " included in argument baseGroups, but possible are only ",
        paste(base_maps[-length(base_maps)], collapse = ", "),
        " and/or ",
        base_maps[length(base_maps)]
      ),
      call. = FALSE
    )
  }
  language <- match.arg(language)
  if(missing(baseGroupNames)){
    baseGroups_4_control <- base_maps_df()[baseGroups, language]
    names(baseGroups_4_control) <- baseGroups
  } else {
    if(!(length(baseGroupNames) == length(baseGroups) & is.character(baseGroupNames) & is.vector(baseGroupNames))){
      stop("if specified, the argument baseGroupNames must be a character vector having the same length as argument baseGroups.", call. = FALSE)
    }
    baseGroups_4_control <- baseGroupNames
    names(baseGroups_4_control) <- baseGroups
  }
  if (!(isTRUE(keep_baseGroups) | isFALSE(keep_baseGroups))) {
    stop("keep_baseGroups must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  map <- as_leaflet(map)
  calls <- map$x$calls
  method <- vapply(calls, function(i) i$method, character(1))
  if("addLayersControl" %in% method) {
    ind <- which(method == "addLayersControl")[1]
    baseGroups_inherited <- calls[[ind]]$args[[1]]
    overlayGroups_inherited <- calls[[ind]]$args[[2]]
    if(!keep_baseGroups) {
      map <- leaflet::clearGroup(map, baseGroups_inherited)
    }
  }
  if(missing(overlayGroups)){
    if("addLayersControl" %in% method) {
      overlayGroups <- overlayGroups_inherited
    } else {
    stop(
      "list of layer groups can't be extracted from argument map --> specify argument overlayGroups",
      call. = FALSE
      )
    }
  } else {
    if("addLayersControl" %in% method) {
      no_overlayGroups <- setdiff(overlayGroups, overlayGroups_inherited)
      if(length(no_overlayGroups) > 0){
        stop(
          paste0(
            paste(no_overlayGroups, collapse = ", "),
            " included in argument overlayGroups, but missing among existing overlay groups: ",
            paste(overlayGroups_inherited, collapse = ", "),
            ". --> specify argument overlayGroups correctly"
            ),
          call. = FALSE
        )
      }
    }
  }
  if(!(keep_baseGroups & "addLayersControl" %in% method)){
    baseGroups_inherited <- NULL
  }
  if(missing(hideGroup)){
    hideGroup <- NULL
  } else {
    no_group <- setdiff(hideGroup, overlayGroups)
    if(length(no_group) > 0){
      stop(
        paste0(paste(no_group, collapse = ", "), " included in argument hideGroup, but missing among existing groups --> specify argument hideGroup correctly"),
        call. = FALSE
      )
    }
  }
  if(missing(showGroup)){
    showGroup <- NULL
  } else {
    no_group <- setdiff(showGroup, overlayGroups)
    if(length(no_group) > 0){
      stop(
        paste0(paste(no_group, collapse = ", "), " included in argument showGroup, but missing among existing groups --> specify argument showGroup correctly"),
        call. = FALSE
      )
    }
  }
  if(!is.null(showGroup) & !is.null(hideGroup)){
    int <- intersect(hideGroup, showGroup)
    if(length(int) > 0){
      stop(
        paste0(paste(int, collapse = ", "), " included in both arguments hideGroup and showGroup --> specify hideGrop and showGroup without any intersection"),
        call. = FALSE
      )
    }
  }
  if (!(isTRUE(collapsed_layer_control) | isFALSE(collapsed_layer_control))) {
    stop("collapsed_layer_control must be a single logical value: TRUE or FALSE", call. = FALSE)
  }
  position_layer_control <- match.arg(position_layer_control)
  for (i in baseGroups) {
    map <- add_1_base_map(i, map, unname(baseGroups_4_control[i]))
  }
  map <-
    leaflet::addLayersControl(
      map,
      baseGroups    = unname(c(baseGroups_4_control, baseGroups_inherited)),
      overlayGroups = overlayGroups,
      options       = leaflet::layersControlOptions(collapsed = collapsed_layer_control, position = position_layer_control)
    )
    map <- leaflet.extras::setMapWidgetStyle(map, list(background = bg_col))
    map <- leaflet::hideGroup(map, hideGroup)
    leaflet::showGroup(map, showGroup)
}
