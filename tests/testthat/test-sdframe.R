require(sf, quietly=TRUE)
require(dplyr, quietly=TRUE)
require(duckspatial, quietly=TRUE)

x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("new,PointFrame", {
    # 1. Empty construction
    expect_silent(p0 <- PointFrame())
    expect_s4_class(p0, "PointFrame")
    expect_equal(nrow(p0), 0)
    
    # 2. Construction from data.frame with x, y
    df <- data.frame(x=1:5, y=1:5, genes=letters[1:5], cell_id=1:5)
    expect_silent(p1 <- PointFrame(df))
    expect_equal(nrow(p1), 5)
    expect_true("geometry" %in% names(p1))
    
    # 3. Supplying ik and fk
    expect_silent(p2 <- PointFrame(df, ik="cell_id", fk="genes"))
    expect_equal(instance_key(p2), "cell_id")
    expect_equal(feature_key(p2), "genes")
    
    # 4. Geometry validation (must be POINT)
    # Use sf object to force non-POINT geometry
    poly <- st_sfc(st_polygon(list(matrix(c(0,1,1,0,0,0,0,1,1,0), ncol=2))))
    df_poly <- st_sf(data.frame(a=1), geometry=poly)
    expect_error(PointFrame(df_poly), "only 'POINT' geometries supported")
})

test_that("new,ShapeFrame", {
    # 1. Empty construction
    expect_silent(s0 <- ShapeFrame())
    expect_s4_class(s0, "ShapeFrame")
    expect_equal(nrow(s0), 0)
    
    # 2. Construction from data.frame with x, y (points)
    df_pts <- data.frame(x=1:5, y=1:5)
    expect_silent(s1 <- ShapeFrame(df_pts))
    expect_equal(nrow(s1), 5)
    expect_equal(geom_type(s1), "POINT")
    
    # 3. Polygon creation from x, y, i
    df_poly <- data.frame(
        x = c(0, 1, 1, 0, 0,  2, 3, 3, 2, 2),
        y = c(0, 0, 1, 1, 0,  2, 2, 3, 3, 2),
        i = c(1, 1, 1, 1, 1,  2, 2, 2, 2, 2)
    )
    expect_silent(s2 <- ShapeFrame(df_poly))
    expect_equal(nrow(s2), 2)
    expect_equal(geom_type(s2), "POLYGON")
    expect_setequal(rownames(as.data.frame(s2)), c("1", "2"))
})

test_that("names", {
    y <- names(p <- point(x))
    expect_is(y, "character")
    expect_identical(y, colnames(data(p)))
})

test_that("$,[[", {
    # names
    nms <- .DollarNames(p <- point(x))
    expect_is(nms, "character")
    expect_length(nms, ncol(p))
    expect_identical(nms, colnames(data(p)))
    # valid
    lapply(seq_len(ncol(p)), \(i) {
        j <- names(p)[i]
        y <- do.call(`$`, list(p, j))
        z <- pull(data(p), j)
        expect_identical(y, z)
        expect_identical(y, z <- do.call(`[[`, list(p, i)))
        expect_identical(z, do.call(`[[`, list(p, j)))
    })
    # invalid
    expect_error(p[[0]])
    expect_error(p[[ncol(p) + 1]])
})

test_that("filter", {
    n <- length(p <- point(x))
    expect_length(filter(p), n)
    expect_length(filter(p, genes == "x"), 0)
    f <- \() filter(p, z == 1)
    expect_error(show(f()))
})

test_that("select", {
    p <- point(x)
    replicate(3, {
        n <- sample(ncol(p), 1)
        i <- sample(names(p), n)
        y <- select(p, all_of(i))
        z <- data(p) |> select(all_of(i))
        expect_equal(collect(data(y)), collect(z))
    })
})

test_that("as.data.frame", {
    y <- as.data.frame(p <- point(x))
    expect_is(y, "data.frame")
    expect_equal(dim(y), dim(p))
    expect_equal(names(y), names(p))
    expect_identical(y, as.data.frame(collect(data(p))))
})

# make point data
set.seed(1)
df <- data.frame(x = runif(100), y = runif(100))

test_that("create, PointFrame", {
  
  # make point frame
  pf <- PointFrame(df)
  expect_identical(st_coordinates(st_as_sf(data(pf))), 
                   {
                     dfm <- as.matrix(df)
                     colnames(dfm) <- c("X", "Y")
                     dfm
                   })
  expect_equal(dim(pf), c(100,1)) # geometry column of POINT
  expect_identical(names(pf), "geometry")

  # coordinate systems
  expect_identical(CTname(pf), "global")
  expect_identical(CTtype(pf), "identity")
  pf_new <- addCT(pf, "test", "scale", c(2,2))
  expect_identical(CTname(pf_new), c("global", "test"))
  expect_identical(CTtype(pf_new), c("identity", "scale"))
  
  # make spatial data
  sd <- SpatialData(points = list(test_points = pf))
  expect_identical(data(point(sd)), data(pf))
  expect_identical(point(sd), pf)
  expect_identical(point(sd, 1), pf)
})

td <- tempdir()
zarr.store <- "test.zarr"
zarr.path <- file.path(td, zarr.store)
unlink(zarr.path, recursive = TRUE)

test_that("write, PointFrame", {
  
  # make sd data
  pf <- PointFrame(df)
  sd <- SpatialData(points = list(test_points = pf))
  
  # write to location
  writeSpatialData(sd, "test.zarr", path = td)
  expect_true(dir.exists(zarr.path))
  
  # read back and compare
  sd2 <- readSpatialData(zarr.path)
  pf2 <- point(sd2)
  # attr(data(pf), "source_table") is not identical, obviously
  expect_equal(
    ddbs_collect(data(pf)),
    ddbs_collect(data(pf2))
  )
  expect_identical(st_coordinates(st_as_sf(data(pf))), 
                   st_coordinates(st_as_sf(data(pf2))))
  expect_identical(meta(pf),meta(pf2))
  expect_identical(names(pf), names(pf2))
})

library(arrow)
library(geoarrow)

# make shape data
# TODO: can we do this conversion inside ShapeFrame ?
df <- duckspatial::as_duckspatial_df(
  st_as_sf(
    arrow_table(
      geometry = geoarrow::as_geoarrow_vctr(
        c(
          "POLYGON ((4.53 2.11, 5.55 1.43, 5.78 1.33, 6.89 9.10, 4.30 4.15, 3.06 4.29, 4.53 2.11))",
          "POLYGON ((4.71 3.73, 7.62 2.48, 9.43 1.09, 9.33 4.99, 6.04 9.35, 4.60 4.85, 4.71 3.73))",
          "POLYGON ((1.65 1.09, 5.24 0.64, 7.02 0.62, 7.88 1.70, 3.17 7.55, 2.78 6.20, 1.65 1.09))",
          "POLYGON ((1.81 3.73, 2.99 0.28, 3.82 4.77, 2.57 8.80, 1.69 7.71, 1.92 5.27, 1.81 3.73))"
        )
      )
    ) 
  ),
  conn = duckspatial::ddbs_create_conn(dbdir = "memory"),
  wkt = "wkt",
  geom_col = "geometry",
  remove = TRUE
)

test_that("create polygon, ShapeFrame", {
  
  # make point frame
  pf <- ShapeFrame(df)
  expect_identical(data(pf), df)
  expect_identical(dim(pf),dim(ddbs_collect(df)))
  expect_identical(names(pf), colnames(df))
  expect_identical(ddbs_collect(data(pf[1:2,1])),
                   ddbs_collect(df)[1:2,1])
               
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

test_that("write polygon, ShapeFrame", {
  
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
  expect_identical(data(pf[1:2, 1]) |> collect(), 
                   data(pf2[1:2,1]) |> collect())
})

# make shape data
# TODO: can we do this conversion inside ShapeFrame ?
df <- duckspatial::as_duckspatial_df(
  st_as_sf(
    arrow_table(
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
  ),
  conn = duckspatial::ddbs_create_conn(dbdir = "memory"),
  wkt = "wkt",
  geom_col = "geometry",
  remove = TRUE
)

test_that("create circle, ShapeFrame", {
  
  # make point frame
  pf <- ShapeFrame(df)
  expect_identical(data(pf), df)
  expect_identical(dim(pf),dim(ddbs_collect(df)))
  expect_identical(names(pf), colnames(df))
  expect_identical(ddbs_collect(data(pf[1:2,1])),
                   ddbs_collect(df)[1:2,1])
  
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

test_that("write circle, ShapeFrame", {
  
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
  expect_identical(data(pf[1:2, 1]) |> collect(), 
                   data(pf2[1:2,1]) |> collect())
})