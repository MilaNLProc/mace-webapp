library(testpackage)

test_that("ingest data and train without errors", {
  expect_message(Model$new("../housing.csv", "median_house_value"))
})
