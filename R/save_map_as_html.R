#' Save a map to an HTML file
#'
#' @param map object of class \code{leaflet}, \code{mapview} or \code{tmap} to save
#' @param file File to save HTML into
#' @param overwrite Overwrite existing \code{file}? (default \code{TRUE})
#'
#' @description Save an object of class \code{leaflet}, \code{mapview} or
#' \code{tmap} to a self-contained HTML file (e.g. for sharing with others).
#'
#' @importFrom htmlwidgets saveWidget
#' @importFrom withr with_dir
#'
#' @export
#'
#' @examples
#' library(mapview)
#' m <- mapview(lakes, legend = FALSE, col.region = "cyan")
#' if(FALSE){
#'   file_path <- file.path("path/to/some/directory", "my_map.html")
#'   save_map_as_html(m, file_path)
#' }
save_map_as_html <- function(map, file, overwrite = TRUE) {
  check_map(map)
  map <- as_leaflet(map)
  # save the map if it doesn't already exist or if overwrite == TRUE
  if (!file.exists(file) | overwrite) {
    withr::with_dir(
      new = dirname(file),
      code = htmlwidgets::saveWidget(
        widget = map,
        file = basename(file)
      )
    )
  } else {
    print("File already exists and 'overwrite' == FALSE. Nothing saved to file.")
  }
}
