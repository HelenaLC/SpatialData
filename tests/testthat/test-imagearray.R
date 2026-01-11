rgb <- seq_len(255)

test_that("ImageArray()", {
    val <- sample(rgb, 3*20*20, replace=TRUE)
    mat <- array(val, dim=c(3, 20, 20))
    expect_silent(ImageArray(mat, axes = c("c", "y", "x")))
    # invalid, need to define axes
    expect_error(ImageArray(mat))
    expect_error(ImageArray(mat, 1))
    expect_error(ImageArray(mat, list()))

    # single scale
    # empty ImageArray is not accepted anymore!
    expect_error(ImageArray(list()))
    expect_error(ImageArray(list(mat)))
    expect_error(ImageArray(list(), Zattrs()))
    expect_silent(ImageArray(list(mat), axes = c("c", "y", "x")))
    
    # multiscale
    # only for ImageArray with 2 dimensions, we can guess the dimensions
    dim <- lapply(c(20, 10, 5), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(sample(rgb, prod(.), replace=TRUE), dim=.))
    expect_error(ImageArray(lys))
    expect_silent(ImageArray(lys, axes = c("c", "y", "x")))
})

test_that("data(),ImageArray", {
    dim <- lapply(c(8, 4, 2), \(.) c(3, rep(., 2)))
    lys <- lapply(dim, \(.) array(0, dim=.))
    img <- ImageArray(lys, axes = c("c", "y", "x"))
    for (. in seq_along(lys))
        expect_identical(data(img, .), lys[[.]])
    expect_identical(data(img, Inf), lys[[3]])
    expect_error(data(img, 0))
    expect_error(data(img, -1))
    expect_error(data(img, 99))
    expect_error(data(img, ""))
    expect_error(data(img, c(1,2)))
})

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("[,ImageArray", {
  y <- image(x, i <- "blobs_image")
  y <- y[,seq_len(32)] # subset to make things harder
})

test_that("create", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- ImageArray(img, axes = c("c", "y", "x"))
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

test_that("write", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- ImageArray(img, axes = c("c", "y", "x"))
  sd <- SpatialData(images = list(test_image = imgarray))
  
  # write to location
  writeSpatialData(sd, "test.zarr", path = td)
  expect_true(dir.exists(zarr.path))
  
  # read back and compare
  sd2 <- readSpatialData(zarr.path)
  imgarray2 <- image(sd2)
  expect_identical(realize(data(imgarray)), 
                   realize(data(imgarray2)))
  expect_identical(meta(imgarray),meta(imgarray2))
})

test_that("create multiscale", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- ImageArray(img, multiscale = TRUE, axes = c("c", "y", "x"))
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

test_that("write multiscale", {
  
  # create image
  set.seed(1)
  img <- array(sample(1:255, size = 100*100*3, replace = TRUE), 
               dim = c(3,100,100))
  
  # make image array
  imgarray <- ImageArray(img, multiscale = TRUE, axes = c("c", "y", "x"))
  sd <- SpatialData(images = list(test_image = imgarray))
  
  # write to location
  writeSpatialData(sd, "test.zarr", path = td)
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
  expect_identical(meta(imgarray),meta(imgarray2))
})