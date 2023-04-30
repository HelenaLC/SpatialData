# constructor ----

test_that("ZarrArray,empty", {
    s4 <- c("Zarr", "Image", "Label")
    s4 <- paste0(s4, "Array")
    for (. in s4) {
        x <- get(.)()
        expect_s4_class(x, .)
        expect_true(is.na(as.array(x)))
        expect_length(dim(x), 1)
        expect_equal(dim(x), 1)
    }
})

# utils ----

path <- system.file("extdata", "raccoon", package="SpatialData", mustWork=TRUE)
path <- file.path(path, "images", "raccoon")
md <- Rarr::zarr_overview(
  list.dirs(path, recursive=FALSE),
  as_data_frame=TRUE)
ia <- readArray(path)

test_that("dim", {
  x <- dim(ia)
  expect_type(x, "integer")
  expect_identical(x, md$dim[[1]])
})

test_that("dimnames", {
  x <- dimnames(ia)
  expect_type(x, "list")
})

test_that("extract", {
  expect_error(ia[1,])
  expect_error(ia[1,,,])
  expect_equal(dim(ia[1,,])[1], 1)
  expect_equal(dim(ia[,1,])[2], 1)
  expect_equal(dim(ia[,,1])[3], 1)
})

test_that("as.array", {
  x <- as.array(ia)
  expect_true(is(x, "array"))
  expect_equal(dim(x), dim(ia))
})

test_that("aperm", {
  expect_equal(dim(aperm(ia)), rev(dim(ia)))
  expect_equal(dim(aperm(ia)), dim(aperm(as.array(ia))))
})