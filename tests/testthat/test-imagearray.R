rgb <- seq_len(255)

test_that("ImageArray()", {
    val <- sample(rgb, 3*20*20, replace=TRUE)
    mat <- array(val, dim=c(3, 20, 20))
    # invalid
    imgarray <- ImageArray(mat)
    expect_equal(dim(imgarray), dim(mat))
    expect_error(ImageArray(mat, 1))
    expect_error(ImageArray(mat, list()))
    # single scale
    expect_silent(ImageArray(list()))
    expect_silent(ImageArray(list(mat)))
    expect_silent(ImageArray(list(mat), Zattrs()))
    
    # multiscale
    # only for ImageArray with 2 dimensions, we can guess the dimensions
    dim <- lapply(c(20, 10, 5), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(sample(rgb, prod(.), replace=TRUE), dim=.))
    expect_silent(ImageArray(lys))
})

test_that("data(),ImageArray", {
    dim <- lapply(c(8, 4, 2), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(0, dim=.))
    img <- ImageArray(lys)
    for (. in seq_along(lys))
        expect_identical(data(img, .), lys[[.]])
    expect_identical(data(img, Inf), lys[[3]])
    expect_error(data(img, 0))
    expect_error(data(img, -1))
    expect_error(data(img, 99))
    expect_error(data(img, ""))
    expect_error(data(img, c(1,2)))
})

test_that("create, ImageArray", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- ImageArray(img)
  expect_identical(realize(data(imgarray)), img)
  expect_identical(dim(imgarray),dim(img))
  
  # coordinate systems
  expect_identical(CTname(imgarray), "global")
  expect_identical(CTtype(imgarray), "identity")
  imgarray_new <- addCT(imgarray, "test", "scale", c(1,2,2))
  expect_identical(CTname(imgarray_new), c("global", "test"))
  expect_identical(CTtype(imgarray_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(images = list(test_image = imgarray))
  expect_identical(data(image(sd)), data(imgarray))
  expect_identical(image(sd), imgarray)
  expect_identical(image(sd, 1), imgarray)
})

td <- tempdir()
zarr.store <- "test.zarr"
zarr.path <- file.path(td, zarr.store)
unlink(zarr.path, recursive = TRUE)

test_that("write, ImageArray", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- ImageArray(img)
  sd <- SpatialData(images = list(test_image = imgarray))
  
  # write to location
  zarr.path <- tempfile(fileext = ".zarr")
  writeSpatialData(sd, path = zarr.path)
  expect_true(dir.exists(zarr.path))
  
  # read back and compare
  sd2 <- readSpatialData(zarr.path)
  imgarray2 <- image(sd2)
  expect_identical(realize(data(imgarray)), 
                   realize(data(imgarray2)))
  expect_equal(meta(imgarray),
               meta(imgarray2))
})

test_that("create multiscale, ImageArray", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- ImageArray(img, scale_factors = c(2,2,2))
  expect_identical(realize(data(imgarray)), img)
  expect_identical(dim(imgarray),dim(img))
  
  # coordinate systems
  expect_identical(CTname(imgarray), "global")
  expect_identical(CTtype(imgarray), "identity")
  imgarray_new <- addCT(imgarray, "test", "scale", c(1,2,2))
  expect_identical(CTname(imgarray_new), c("global", "test"))
  expect_identical(CTtype(imgarray_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(images = list(test_image = imgarray))
  expect_identical(data(image(sd)), data(imgarray))
  expect_identical(data(image(sd),2), data(imgarray,2))
  expect_identical(data(image(sd),3), data(imgarray,3))
  expect_identical(image(sd), imgarray)
  expect_identical(image(sd, 1), imgarray)
})

td <- tempdir()
zarr.store <- "test.zarr"
zarr.path <- file.path(td, zarr.store)
unlink(zarr.path, recursive = TRUE)

test_that("write multiscale, ImageArray", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- ImageArray(img, scale_factors = c(2,2,2))
  sd <- SpatialData(images = list(test_image = imgarray))
  
  # write to location
  zarr.path <- tempfile(fileext = ".zarr")
  writeSpatialData(sd, path = zarr.path)
  expect_true(dir.exists(zarr.path))
  
  # read back and compare
  sd2 <- readSpatialData(zarr.path)
  imgarray2 <- image(sd2)
  expect_identical(realize(data(imgarray, 1)), 
                   realize(data(imgarray2, 1)))
  expect_identical(realize(data(imgarray, 2)), 
                   realize(data(imgarray2, 2)))
  expect_identical(realize(data(imgarray, 3)), 
                   realize(data(imgarray2, 3)))
  expect_equal(meta(imgarray),meta(imgarray2))
})

# test_that("write v3 uses Python-readable codec ordering", {
#   td <- tempdir()
#   zarr.path <- file.path(td, "test_v3.zarr")
#   unlink(zarr.path, recursive = TRUE)
# 
#   set.seed(1)
#   img <- array(sample(1:255, size = 20 * 20 * 3, replace = TRUE),
#                dim = c(3, 20, 20))
#   imgarray <- ImageArray(img, axes = c("c", "y", "x"))
#   sd <- SpatialData(images = list(test_image = imgarray))
# 
#   writeSpatialData(sd, "test_v3.zarr", path = td, version = "v3")
# 
#   metadata <- jsonlite::read_json(
#     file.path(zarr.path, "images", "test_image", "0", "zarr.json"),
#     simplifyVector = FALSE
#   )
#   codec_names <- vapply(metadata$codecs, `[[`, character(1), "name")
# 
#   expect_identical(codec_names, c("transpose", "bytes", "zstd"))
#   expect_equal(unname(unlist(metadata$dimension_names)), c("c", "y", "x"))
#   expect_equal(metadata$attributes, list())
#   expect_equal(metadata$storage_transformers, list())
# })
