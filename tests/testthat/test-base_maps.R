test_that("base_maps", {
  expect_error(
    base_maps(NULL),
    "invalid 'pattern' argument"
  )

  expect_error(base_maps(source_type = "WWW"))

  expect_error(base_maps_language(language = "CH"))

  expect_equal(
    base_maps_language(language = "DE"),
    base_maps_language(language = c("DE", "CH"))
    )
})
