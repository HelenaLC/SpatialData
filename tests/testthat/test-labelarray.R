test_that("LabelArray()", {
  val <- sample(seq_len(12), 20*20, replace=TRUE)
  mat <- array(val, dim=c(20, 20))
  lblarray <- LabelArray(mat)
  expect_equal(dim(lblarray), dim(mat))
  # invalid
  expect_error(LabelArray(mat, 1))
  expect_error(LabelArray(mat, list()))
  # single scale
  expect_silent(LabelArray(list()))
  expect_silent(LabelArray(list(mat)))
  expect_silent(LabelArray(list(mat), Zattrs()))
  # multiscale
  dim <- lapply(c(20, 10, 5), \(.) rep(., 2))
  lys <- lapply(dim, \(.) array(sample(seq_len(12), prod(.), replace=TRUE), dim=.))
  expect_silent(LabelArray(lys))
})


test_that("data(),LabelArray", {
  dim <- lapply(c(8, 4, 2), \(.) rep(., 2))
  lys <- lapply(dim, \(.) array(0L, dim=.))
  lab <- LabelArray(lys)
  for (. in seq_along(lys))
    expect_identical(data(lab, .), lys[[.]])
  expect_identical(data(lab, Inf), lys[[3]])
  expect_error(data(lab, 0))
  expect_error(data(lab, -1))
  expect_error(data(lab, 99))
  expect_error(data(lab, ""))
  expect_error(data(lab, c(1,2)))
})


test_that("create,LabelArray", {
  
  # create label
  set.seed(1)
  lbl <- array(sample(0:8L, size = 100*100, replace = TRUE), 
               dim = c(100,100))
  
  # make label array
  lblarray <- LabelArray(lbl)
  expect_identical(realize(data(lblarray)), lbl)
  expect_identical(dim(lblarray),dim(lbl))
  
  # coordinate systems
  expect_identical(CTname(lblarray), "global")
  expect_identical(CTtype(lblarray), "identity")
  lblarray_new <- addCT(lblarray, "test", "scale", c(2,2))
  expect_identical(CTname(lblarray_new), c("global", "test"))
  expect_identical(CTtype(lblarray_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(labels = list(test_label = lblarray))
  expect_identical(data(label(sd)), data(lblarray))
  expect_identical(label(sd), lblarray)
  expect_identical(label(sd, 1), lblarray)
})

td <- tempdir()
zarr.store <- "test.zarr"
zarr.path <- file.path(td, zarr.store)
unlink(zarr.path, recursive = TRUE)

test_that("write,LabelArray", {
  
  # create label
  set.seed(1)
  lbl <- array(sample(0:8L, size = 100*100, replace = TRUE), 
               dim = c(100,100))
  
  # make label array
  lblarray <- LabelArray(lbl)
  sd <- SpatialData(labels = list(test_label = lblarray))
  
  # write to location
  writeSpatialData(sd, "test.zarr", path = td)
  expect_true(dir.exists(zarr.path))
  
  # read back and compare
  sd2 <- readSpatialData(zarr.path)
  lblarray2 <- label(sd2)
  expect_identical(realize(data(lblarray)), 
                   realize(data(lblarray2)))
  expect_equal(meta(lblarray),meta(lblarray2))
})

test_that("create multiscale,LabelArray", {
  
  # create label
  set.seed(1)
  lbl <- array(sample(0:8L, size = 100*100, replace = TRUE), 
               dim = c(100,100))
  
  # make label array
  lblarray <- LabelArray(lbl, scale_factors = c(2,2,2))
  expect_identical(realize(data(lblarray)), lbl)
  expect_identical(dim(lblarray),dim(lbl))
  
  # coordinate systems
  expect_identical(CTname(lblarray), "global")
  expect_identical(CTtype(lblarray), "identity")
  lblarray_new <- addCT(lblarray, "test", "scale", c(2,2))
  expect_identical(CTname(lblarray_new), c("global", "test"))
  expect_identical(CTtype(lblarray_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(labels = list(test_label = lblarray))
  expect_identical(data(label(sd)), data(lblarray))
  expect_identical(data(label(sd),2), data(lblarray,2))
  expect_identical(data(label(sd),3), data(lblarray,3))
  expect_identical(label(sd), lblarray)
  expect_identical(label(sd, 1), lblarray)
})

td <- tempdir()
zarr.store <- "test.zarr"
zarr.path <- file.path(td, zarr.store)
unlink(zarr.path, recursive = TRUE)

test_that("write multiscale,LabelArray", {
  
  # create label
  set.seed(1)
  lbl <- array(sample(0:8L, size = 100*100, replace = TRUE), 
               dim = c(100,100))
  
  # make label array
  lblarray <- LabelArray(lbl, scale_factors = c(2,2,2))
  sd <- SpatialData(labels = list(test_label = lblarray))
  
  # write to location
  writeSpatialData(sd, "test.zarr", path = td)
  expect_true(dir.exists(zarr.path))
  
  # read back and compare
  sd2 <- readSpatialData(zarr.path)
  lblarray2 <- label(sd2)
  expect_identical(realize(data(lblarray)), 
                   realize(data(lblarray2)))
  expect_identical(realize(data(lblarray, 2)), 
                   realize(data(lblarray2, 2)))
  expect_identical(realize(data(lblarray, 3)), 
                   realize(data(lblarray2, 3)))
  expect_equal(meta(lblarray),meta(lblarray2))
})