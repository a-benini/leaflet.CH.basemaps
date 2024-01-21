#' Swiss Cantons
#'
#' @description
#' Administrative borders of Swiss Cantons
#'
#' @format \code{sf feature collection MULTIPOLYGON}:
#' \describe{
#'   \item{kantonsnummer}{number of canton}
#'   \item{name}{name of canton}
#'   \item{see_flaeche}{area of a canton covered by lakes (ha)}
#'   \item{kantonsflaeche}{whole area of a canton (ha)}
#'   \item{einwohnerzahl}{number of inhabitants}
#'   \item{geom}{geometry column}
#' }
#'
#' @details This geometry set origins form the \code{3D-MULTIPOLYGON} layer
#' \code{tlm_kantonsgebiet} included in \code{swissBOUNDARIES3D} (s. Source).
#' Z-dimension was dropped, and the number of attributes was reduced.
#'
#' @source swissboundaries3d_2023-01_2056_5728.gpkg.zip was downloaded on
#' 17/12/2023 from
#' \url{https://www.swisstopo.admin.ch/en/geodata/landscape/boundaries3d.html}
"cantons"
