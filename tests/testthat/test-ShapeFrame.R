library(arrow)
library(geoarrow)

# make shape data
df <- arrow_table(
  geometry = geoarrow::as_geoarrow_vctr(
    c(
      "POLYGON ((4.53 2.11, 5.55 1.43, 5.78 1.33, 6.89 9.10, 4.30 4.15, 3.06 4.29, 4.53 2.11))",
      "POLYGON ((4.71 3.73, 7.62 2.48, 9.43 1.09, 9.33 4.99, 6.04 9.35, 4.60 4.85, 4.71 3.73))",
      "POLYGON ((1.65 1.09, 5.24 0.64, 7.02 0.62, 7.88 1.70, 3.17 7.55, 2.78 6.20, 1.65 1.09))",
      "POLYGON ((1.81 3.73, 2.99 0.28, 3.82 4.77, 2.57 8.80, 1.69 7.71, 1.92 5.27, 1.81 3.73))"
    )
  )
)

test_that("create polygon", {
  
  # make point frame
  pf <- ShapeFrame(df)
  expect_identical(data(pf), df)
  expect_identical(dim(pf),dim(df))
  expect_identical(names(pf), names(df))
  # TODO: they are not identical, why ? 
  expect_equal(data(pf[1:4, 1]), df[1:4,1])
  
  # coordinate systems
  expect_identical(CTname(pf), "global")
  expect_identical(CTtype(pf), "identity")
  pf_new <- addCT(pf, "test", "scale", c(2,2))
  expect_identical(CTname(pf_new), c("global", "test"))
  expect_identical(CTtype(pf_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(shapes = list(test_shapes = pf))
  expect_identical(data(shape(sd)), data(pf))
  expect_identical(shape(sd), pf)
  expect_identical(shape(sd, 1), pf)
})

td <- tempdir()
zarr.store <- "test.zarr"
zarr.path <- file.path(td, zarr.store)
unlink(zarr.path, recursive = TRUE)

test_that("write polygon", {
  
  # make sd data
  pf <- ShapeFrame(df)
  sd <- SpatialData(shapes = list(test_shapes = pf))
  
  # write to location
  writeSpatialData(sd, "test.zarr", path = td)
  expect_true(dir.exists(zarr.path))
  
  # read back and compare
  sd2 <- readSpatialData(zarr.path)
  pf2 <- shape(sd2)
  # TODO: they are not identical, why ? 
  expect_equal(data(pf) |> collect(), 
               data(pf2) |> collect())
  expect_identical(meta(pf),meta(pf2))
  expect_identical(names(pf), names(pf2))
  # TODO: they are not identical, why ? 
  expect_equal(data(pf[1:2, 1]), data(pf2[1:2,1]))
})

# make shape data
df <- arrow_table(
  geometry = geoarrow::as_geoarrow_vctr(
    c(
      "POINT (36.382774 24.6331748)",
      "POINT (32.378292 46.4148383)",
      "POINT (24.3715883 25.5517166)",
      "POINT (18.7407733 23.5779362)"
    )
  ),
  radius = c(4,4,4,4)
)

test_that("create radius shapes", {
  
  # make point frame
  pf <- ShapeFrame(df)
  expect_identical(data(pf), df)
  expect_identical(dim(pf),dim(df))
  expect_identical(names(pf), names(df))
  # TODO: they are not identical, why ? 
  expect_equal(data(pf[1:4, 1]), df[1:4,1])
  
  # coordinate systems
  expect_identical(CTname(pf), "global")
  expect_identical(CTtype(pf), "identity")
  pf_new <- addCT(pf, "test", "scale", c(2,2))
  expect_identical(CTname(pf_new), c("global", "test"))
  expect_identical(CTtype(pf_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(shapes = list(test_shapes = pf))
  expect_identical(data(shape(sd)), data(pf))
  expect_identical(shape(sd), pf)
  expect_identical(shape(sd, 1), pf)
})

td <- tempdir()
zarr.store <- "test.zarr"
zarr.path <- file.path(td, zarr.store)
unlink(zarr.path, recursive = TRUE)

test_that("write radius shapes", {
  
  # make sd data
  pf <- ShapeFrame(df)
  sd <- SpatialData(shapes = list(test_shapes = pf))
  
  # write to location
  writeSpatialData(sd, "test.zarr", path = td)
  expect_true(dir.exists(zarr.path))
  
  # read back and compare
  sd2 <- readSpatialData(zarr.path)
  pf2 <- shape(sd2)
  # TODO: they are not identical, why ? 
  expect_equal(data(pf) |> collect(), 
               data(pf2) |> collect())
  expect_identical(meta(pf),meta(pf2))
  expect_identical(names(pf), names(pf2))
  # TODO: they are not identical, why ? 
  expect_equal(data(pf[1:2, 1]), data(pf2[1:2,1]))
})
