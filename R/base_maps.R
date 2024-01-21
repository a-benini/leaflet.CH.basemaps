#' Available base maps
#'
#' @param regex character; regular expression to filter the \code{base_map} and
#' language (\code{EN}, \code{DE}, \code{FR}, \code{IT}, \code{RM}) fields on
#' @param source_type if specified character; \code{WMTS} (Web Map Tile Service)
#' or \code{WMS} (Web Map Service) for filtering base maps by source type
#' @param language a character vector selecting the language column(s)
#' \code{base_maps_language()} returns: \code{"EN"}, \code{"DE"}, \code{"FR"},
#' \code{"IT"} and/or \code{"RM"} are possible. By default all of them.
#'
#' @description
#' Find information about base map layers available with package \code{leaflet.CH.basemaps}
#'
#' @details
#' \code{base_maps()} returns a \code{data.frame} / \code{\link[tibble]{tibble}}
#' containing columns:
#' \itemize{
#'   \item \code{base_map}: base maps which can be selected by \code{\link{add_base_maps}}'s
#'   argument \code{baseGroups} or which can be added to a map as single layer
#'   (s. \code{\link{add_single_base_map}}).
#'   \item \code{EN} (English), \code{DE} (German), \code{FR} (French), \code{IT}
#'   (Italian) \code{RM} (Rhaeto-Romance): group name of base map layer determined
#'   by specification of argument \code{language} (s. \code{\link{add_base_maps}}
#'   and \code{\link{add_single_base_map}}).
#'   \item \code{source_type}: \code{WMTS} (Web Map Tile Service) or \code{WMS}
#'   (Web Map Service).
#'   \item \code{urlTemplate}: if \code{source_type} = \code{WMTS} URL template,
#'   else \code{NA}, can be used to specify \code{\link[leaflet]{addTiles}}'s
#'   argument \code{urlTemplate} or the argument \code{server} shared by
#'   \code{\link[tmap]{tm_basemap}} and \code{\link[tmap]{tm_tiles}}.
#'   \item \code{baseUrl}, \code{layers}: if \code{source_type} = \code{WMS} base
#'   URL of the WMS service resp. WMS layer, else \code{NA}, can be used to specify
#'   \code{\link[leaflet]{addWMSTiles}}'s arguments \code{baseUrl} / \code{layers}.
#' }
#'
#' \code{base_maps_language()} returns reduced version of \code{base_maps()} with
#' columns: \code{base_map}, \code{EN}, \code{DE}, \code{FR}, \code{IT}, \code{RM}.
#'
#' \code{base_maps_source()} returns reduced version of \code{base_maps()} with
#' columns: \code{base_map}, \code{source_type}, \code{urlTemplate}, \code{baseUrl}, \code{layers}.
#'
#' @importFrom tibble tibble tribble
#'
#' @examples
#' # get all the information on viable base maps:
#' base_maps() # overkill of information?
#'
#' # view group names in diff. languages + search with expression:
#' base_maps_language("image")
#'
#' # group names in specific language:
#' base_maps_language(language = "DE")
#'
#' # source information + search with expression:
#' base_maps_source("relief")
#'
#' # source information filtered by source type:
#' base_maps_source(source_type = "WMTS")
#'
#' # note that urlTemplate belonging to no_base_map is an empty character string:
#' base_maps("no_base_map")$urlTemplate == ""
#' @name base_maps
#' @export
base_maps <- function(regex, source_type) {
  tab <- base_maps_tib()
  tab <- tibble::tibble(tab[, 1:6], source_type = ifelse(is.na(tab$urlTemplate), "WMS", "WMTS"), tab[, c(9, 7, 8)])
  if (!missing(source_type)) {
    source_type <- match.arg(source_type, unique(tab$source_type))
    tab <- tab[tab$source_type == source_type, ]
  }
  if (missing(regex)) {
    tab
  } else {
    fn <- function(x, pattern) any(grepl(x, pattern = pattern))
    tab[apply(tab[c("base_map", "EN", "DE", "FR", "IT", "RM")], 1, fn, pattern = regex), ]
  }
}
#' @name base_maps
#' @export
base_maps_language <- function(regex, source_type, language = c("EN", "DE", "FR", "IT", "RM")) {
  language <- match.arg(language, several.ok = TRUE)
  base_maps(regex = regex, source_type = source_type)[, c("base_map", language)]
}
#' @name base_maps
#' @export
base_maps_source <- function(regex, source_type) {
  base_maps(regex = regex, source_type = source_type)[, c(1, 7:10)]
}
# helper functions for pkg-internal use
base_maps_tib <- function() {
  tibble::tribble(
    ~base_map,           ~EN,                 ~DE,                 ~FR,                  ~IT,                  ~RM,                    ~baseUrl,                   ~layers,                                             ~urlTemplate,
    # "no_base_map",       "no base map",       "kein Hintergrund",  "fond blanc",         "nessuno sfondo",     "nagin fuondi",         NA,                         NA,                                                  "",

    "no_base_map",       "no base map",       "kein Hintergrund",  "fond blanc",         "nessununa carta",    "nagina charta",        NA,                         NA,                                                  "",
    "swisstopo_grey",    "swisstopo grey",    "swisstopo grau",    "swisstopo gris",     "swisstopo grigio",   "swisstopo en greisch", NA,                         NA,                                                  'https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-grau/default/current/3857/{z}/{x}/{y}.jpeg',
    "swisstopo_color",   "swisstopo color",   "swisstopo farbig",  "swisstopo couleur",  "swisstopo a colori", "swisstopo en colur",   NA,                         NA,                                                  'https://wmts20.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-farbe/default/current/3857/{z}/{x}/{y}.jpeg',
    "swissimage",        "swissimage",        "swissimage",        "swissimage",         "swissimage",         "swissimage",           NA,                         NA,                                                  'https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.swissimage/default/current/3857/{z}/{x}/{y}.jpeg',
    "relief",            "relief",            "Relief",            "relief",             "rilievo",            "reliev",               "https://wms.geo.admin.ch", "ch.swisstopo.leichte-basiskarte_reliefschattierung", NA,

    "swissTLM_grey",     "swissTLM grey",     "swissTLM grau",     "swissTLM gris",      "swissTLM grigio",    "swissTLM en greisch",  "https://wms.geo.admin.ch", "ch.swisstopo.swisstlm3d-karte-grau",                 NA,
    "swissTLM_color",    "swissTLM color",    "swissTLM farbig",   "swissTLM couleur",   "swissTLM a colori",  "swissTLM en colur",    "https://wms.geo.admin.ch", "ch.swisstopo.swisstlm3d-karte-farbe",                NA,

    "swissimage_1946",   "swissimage 1946",   "swissimage 1946",   "swissimage 1946",    "swissimage 1946",    "swissimage 1946",      "https://wms.geo.admin.ch", "ch.swisstopo.swissimage-product_1946",               NA,
    "siegfried_map",     "Siegfried Map",     "Siegfried Karte",   "Carte Siegfried",    "Carta Siegfried",    "Charta Siegfried",     "https://wms.geo.admin.ch", "ch.swisstopo.hiks-siegfried",                        NA,
    "dufour_map",        "Dufour Map",        "Dufour Karte",      "Carte Dufour",       "Carta Dufour",       "Charta Dufour",        "https://wms.geo.admin.ch", "ch.swisstopo.hiks-dufour",                           NA,

    # "hillshade",         "hillshade",                  "Reliefschattierung",                 "estompage du relief",                   "ombreggiatura del rilievo",                 "sumbriva dal reliev",                  "https://wms.geo.admin.ch", "ch.swisstopo.swissalti3d-reliefschattierung",                 NA,
    # "hillshade_2",       "monodirectional hillshade",  "Reliefschattierung monodirektional", "estompage du relief monodirectionnel",  "ombreggiatura del rilievo monodirezionale", "sumbriva dal reliev monodirecziunal",  "https://wms.geo.admin.ch", "ch.swisstopo.swissalti3d-reliefschattierung_monodirektional", NA,

    "hillshade",         "hillshade",                   "Reliefschattierung",                  "estompage du relief",                    "ombreggiatura del rilievo",                 "sumbriva dal reliev",                    "https://wms.geo.admin.ch", "ch.swisstopo.swissalti3d-reliefschattierung_monodirektional",  NA,
    "hillshade_2",       "multidirectional hillshade",  "Reliefschattierung multidirektional", "estompage du relief multidirectionnel",  "ombreggiatura del rilievo multidirezionale", "sumbriva dal reliev multidirecziunal",  "https://wms.geo.admin.ch", "ch.swisstopo.swissalti3d-reliefschattierung",                  NA,

    "relief_nfi",        "relief NFI",                  "Reliefschattierung LFI",              "estompage du relief IFN",                "ombreggiatura del rilievo IFN",              "sumbriva dal reliev IFN",               "https://wms.geo.admin.ch", "ch.bafu.landesforstinventar-vegetationshoehenmodell_relief",   NA,

    "national_map_10_color", "National Map 1:10'000 (color)",  "Landeskarte 1:10'000 (farbig)",  "Carte nationale 1:10'000 (couleur)",  "Carta nazionale 1:10'000 (colori)",          "Charta naziunala 1:10'000 (colur)",     "https://wms.geo.admin.ch", "ch.swisstopo.landeskarte-farbe-10",                             NA,
    "national_map_10_grey",  "National Map 1:10'000 (grey)",   "Landeskarte 1:10'000 (grau)",    "Carte nationale 1:10'000 (grise)",    "Carta nazionale 1:10'000 (grigio)",          "Charta naziunala 1:10'000 (grisch)",    "https://wms.geo.admin.ch", "ch.swisstopo.landeskarte-grau-10",                              NA,
    "national_map_25_color", "National Map 1:25'000 (color)",  "Landeskarte 1:25'000 (farbig)",  "Carte nationale 1:25'000 (couleur)",  "Carta nazionale 1:25'000 (colori)",          "Charta naziunala 1:25'000 (colur)",     "https://wms.geo.admin.ch", "ch.swisstopo.pixelkarte-farbe-pk25.noscale",                    NA,
    "national_map_50_color", "National Map 1:50'000 (color)",  "Landeskarte 1:50'000 (farbig)",  "Carte nationale 1:50'000 (couleur)",  "Carta nazionale 1:50'000 (colori)",          "Charta naziunala 1:50'000 (colur)",     "https://wms.geo.admin.ch", "ch.swisstopo.pixelkarte-farbe-pk50.noscale",                    NA
    )
}

base_maps_df <- function(){data.frame(base_maps_tib(), row.names = base_maps_tib()$base_map)}
