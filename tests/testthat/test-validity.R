path <- system.file("extdata", "blobs", package="SpatialData", mustWork=TRUE)
x <- readSpatialData(path)

test_that("validity,images", {
    x@images <- list("")
    expect_error(validObject(x))
    x@images <- list(ImageArray())
    expect_silent(validObject(x))
})

test_that("validity,labels", {
    x@labels <- list("")
    expect_error(validObject(x))
    x@labels <- list(LabelArray())
    expect_silent(validObject(x))
})

test_that("validity,shapes", {
    x@shapes <- list("")
    expect_error(validObject(x))
    x@shapes <- list(ShapeFrame())
    expect_silent(validObject(x))
})

test_that("validity,points", {
    x@points <- list("")
    expect_error(validObject(x))
    path <- file.path(path, "points", "blobs_points")
    pq <- list.files(path, "*\\.parquet$", recursive=TRUE, full.names=TRUE)
    at <- arrow::read_parquet(pq, as_data_frame=FALSE)
    x@points <- list(PointFrame(at))
    expect_silent(validObject(x))
})
