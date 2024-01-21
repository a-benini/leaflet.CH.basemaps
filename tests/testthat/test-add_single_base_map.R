test_that("add_single_base_map", {
  library(sf)
  # library(mapview)
  # library(tmap)
  library(leaflet)

  expect_error(
    add_base_maps(NULL),
    "map must be of an object of the class leaflet, mapview or tamp"
  )

  waldperlen_circles <- leaflet() %>% addCircles(data = st_transform(waldperlen, 4326), group = "waldperlen")

  expect_error(
    add_swisstopo_grey(waldperlen_circles, NULL),
    "if specified, the argument group must be a single character string."
  )

  expect_error(add_swisstopo_grey(waldperlen_circles, language = "XXX"))
})
