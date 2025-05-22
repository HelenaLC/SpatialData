library(Rarr)

test_that("create zarr/group", {
  
  dir.create(td <- tempfile())
  name <- "test"
  output_zarr <- file.path(td, paste0(name, ".zarr"))
  
  # open zarr
  create_zarr(dir = td, prefix = name)
  expect_true(dir.exists(output_zarr))
  expect_true(file.exists(file.path(output_zarr, ".zgroup")))
  
  # create group one group
  create_zarr_group(store = output_zarr, name = "group1")
  expect_true(dir.exists(file.path(output_zarr, "group1")))
  expect_true(file.exists(file.path(output_zarr, "group1", ".zgroup")))
  
  # create nested two groups
  create_zarr_group(store = output_zarr, name = "group2/subgroup1")
  expect_true(dir.exists(file.path(output_zarr, "group2")))
  expect_true(file.exists(file.path(output_zarr, "group2", ".zgroup")))
  expect_true(dir.exists(file.path(output_zarr, "group2/subgroup1")))
  expect_true(file.exists(file.path(output_zarr, "group2/subgroup1", ".zgroup")))
  
  # create nested three groups
  create_zarr_group(store = output_zarr, name = "group3/subgroup1/subsubgroup1")
  expect_true(dir.exists(file.path(output_zarr, "group3")))
  expect_true(file.exists(file.path(output_zarr, "group3", ".zgroup")))
  expect_true(dir.exists(file.path(output_zarr, "group3/subgroup1")))
  expect_true(file.exists(file.path(output_zarr, "group3/subgroup1", ".zgroup")))
  expect_true(dir.exists(file.path(output_zarr, "group3/subgroup1/subsubgroup1")))
  expect_true(file.exists(file.path(output_zarr, "group3/subgroup1/subsubgroup1", ".zgroup")))
  
  # version 3 and other entries
  dir.create(td <- tempfile())
  name <- "test"
  output_zarr <- file.path(td, paste0(name, ".zarr"))
  expect_error(create_zarr(dir = td, prefix = name, version = "v4"), pattern = "only zarr v2 is supported")
})


# create zarr array
dir.create(td <- tempfile())
path <- file.path(td, "test.zarr")
x <- array(runif(n = 10), dim = c(2, 5))
Rarr::write_zarr_array(
  x = x, zarr_array_path = path,
  chunk_dim = c(2, 5)
)

test_that("read/write zattrs", {
  
  # add .zattrs to /
  zattrs <- list(foo = "foo", bar = "bar")
  write_zattrs(path = path, new.zattrs = zattrs)
  expect_true(file.exists(file.path(path, ".zattrs")))
  
  # check .zattrs
  read.zattrs <- read_zattrs(path)
  expect_equal(read.zattrs, zattrs)
  
  # add new elements to .zattrs
  zattrs.new.elem <- list(foo2 = "foo")
  write_zattrs(path = path, new.zattrs = zattrs.new.elem)
  read.zattrs <- read_zattrs(path)
  expect_equal(read.zattrs, c(zattrs,zattrs.new.elem))
  
  # overwrite
  zattrs.new.elem <- list(foo2 = "foo2")
  write_zattrs(path = path, new.zattrs = zattrs.new.elem)
  read.zattrs <- read_zattrs(path)
  zattrs[names(zattrs.new.elem)] <- zattrs.new.elem
  expect_equal(read.zattrs, c(zattrs))
  
  # overwrite = FALSE
  zattrs.new.elem <- list(foo2 = "foo")
  write_zattrs(path = path, new.zattrs = zattrs.new.elem, overwrite = FALSE)
  read.zattrs <- read_zattrs(path)
  zattrs[names(zattrs.new.elem)] <- "foo2"
  expect_equal(read.zattrs, c(zattrs))
  
})