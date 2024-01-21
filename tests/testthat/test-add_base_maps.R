test_that("add_base_maps", {
  library(sf)
  library(mapview)
  library(tmap)
  library(leaflet)

  waldperlen_circles <- leaflet() %>% addCircles(data = st_transform(waldperlen, 4326), group = "waldperlen")

  expect_error(
    add_base_maps(waldperlen_circles),
    "list of layer groups can't be extracted from argument map --> specify argument overlayGroups"
  )

  expect_error(
    add_base_maps(waldperlen_circles, overlayGroups = "waldperlen", language = "CH")
  )

  expect_error(
    add_base_maps(waldperlen_circles, overlayGroups = "waldperlen", hideGroup = "XXX"),
    "XXX included in argument hideGroup, but missing among existing groups --> specify argument hideGroup correctly"
  )

  expect_error(
    add_base_maps(waldperlen_circles, overlayGroups = "waldperlen", showGroup = "XXX"),
    "XXX included in argument showGroup, but missing among existing groups --> specify argument showGroup correctly"
  )

  expect_error(
    add_base_maps(waldperlen_circles, overlayGroups = "waldperlen", hideGroup = "waldperlen", showGroup = "waldperlen"),
    "waldperlen included in both arguments hideGroup and showGroup --> specify hideGrop and showGroup without any intersection"
  )

  base_maps <- base_maps()$base_map
  error_message <-
    paste0(
      "XXX",
      " included in argument baseGroups, but possible are only ",
      paste(base_maps[-length(base_maps)], collapse = ", "),
      " and/or ",
      base_maps[length(base_maps)]
    )

  expect_error(
    add_base_maps(waldperlen_circles, overlayGroups = "waldperlen", baseGroups = "XXX"),
    error_message
  )

  expect_error(
    add_base_maps(
      waldperlen_circles,
      overlayGroups = "waldperlen",
      baseGroups = c("swissimage", "relief"),
      baseGroupNames = c("A", "B", "C")
    ),
    "if specified, the argument baseGroupNames must be a character vector having the same length as argument baseGroups."
  )

  expect_error(
    add_base_maps(
      waldperlen_circles,
      overlayGroups = "waldperlen",
      collapsed_layer_control = c(TRUE, FALSE)
    ),
    "collapsed_layer_control must be a single logical value: TRUE or FALSE"
  )

  expect_error(
    add_base_maps(
      waldperlen_circles,
      overlayGroups = "waldperlen",
      position_layer_control = "top"
    )
  )

  expect_error(
    add_base_maps(
      waldperlen_circles,
      overlayGroups = "waldperlen",
      keep_baseGroups = list(TRUE)
    ),
    "keep_baseGroups must be a single logical value: TRUE or FALSE"
  )
})
