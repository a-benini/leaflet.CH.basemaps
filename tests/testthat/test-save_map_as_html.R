test_that("save_map_as_html", {
  library(sf)
  library(mapview)
  library(tmap)
  library(leaflet)

  waldperlen_circles <- leaflet() %>% addCircles(data = st_transform(waldperlen, 4326))

  test_leaflet <- file.path(tempdir(), "leaflet_test.html")
  expect_false(file.exists(test_leaflet))
  save_map_as_html(waldperlen_circles, test_leaflet)
  expect_true(file.exists(test_leaflet))

  expect_equal(
    save_map_as_html(waldperlen_circles, test_leaflet, overwrite = FALSE),
    "File already exists and 'overwrite' == FALSE. Nothing saved to file."
  )

  expect_error(
    save_map_as_html(NULL, test_leaflet),
    "map must be of an object of the class leaflet, mapview or tamp"
  )

  tm <- tm_shape(waldperlen) + tm_dots()
  test_tmap <- file.path(tempdir(), "tm_test.html")
  expect_false(file.exists(test_tmap))
  save_map_as_html(tm, test_tmap)
  expect_true(file.exists(test_tmap))

  m <- mapview(waldperlen)
  test_mapview <- file.path(tempdir(), "mapview_test.html")
  expect_false(file.exists(test_mapview))
  save_map_as_html(m, test_mapview)
  expect_true(file.exists(test_mapview))
})
