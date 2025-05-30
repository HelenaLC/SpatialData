x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("add metadata", {
  df <- data.frame(x = runif(100), y = runif(100))
  meta <- .createPointmeta(df)
})