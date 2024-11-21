x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, tables=FALSE)

test_that("axes", {
    # image
    y <- axes(image(x))
    expect_is(y, "data.frame")
    expect_equal(dim(y), c(3, 2))
    # label
    y <- axes(label(x))
    expect_is(y, "data.frame")
    expect_equal(dim(y), c(2, 2))
    # shape
    y <- axes(shape(x))
    expect_is(y, "character")
    expect_length(y, 2)
    # point
    y <- axes(point(x))
    expect_is(y, "character")
    expect_length(y, 2)
})

test_that("rmvCT", {
    y <- label(x)
    # invalid index/name
    expect_error(rmvCT(y, 100))
    expect_error(rmvCT(y, "."))
    expect_error(rmvCT(y, c(".", CTname(y)[1])))
    # by name
    i <- sample(CTname(y), 2) 
    expect_identical(CTname(rmvCT(y, i)), setdiff(CTname(y), i))
    # by index
    i <- sample(seq_along(CTname(y)), 2) 
    expect_identical(CTname(rmvCT(y, i)), CTname(y)[-i])
})

test_that("addCT", {
    # get 1st element from each layer
    ls <- setdiff(.LAYERS, "tables")
    es <- lapply(ls, \(.) x[.,1][[.]][[1]])
    .check_data <- \(z, x) {
        expect_true("." %in% CTname(z))
        ct <- CTdata(z)[CTname(z) == ".", ]
        expect_identical(ct[[t]][[1]], x)
    }
    for (y in es) {
        t <- "identity"
        expect_error(addCT(y, ".", t, 12345))
        expect_silent(z <- addCT(y, ".", t, v <- NULL))
        .check_data(z, v)
        t <- "rotate"
        expect_error(addCT(y, ".", t, -12345)) # negative
        expect_error(addCT(y, ".", t, c(1,1))) # too many
        expect_error(addCT(y, ".", t, ".")) # not a number
        expect_silent(z <- addCT(y, ".", t, v <- 1)) 
        .check_data(z, v)
        t <- "scale"
        d <- ifelse(is(y, "ImageArray"), 3, 2)
        expect_error(addCT(y, ".", t, numeric(d))) # zeroes
        expect_error(addCT(y, ".", t, 1+numeric(d+1))) # too many
        expect_error(addCT(y, ".", t, character(d))) # not a number
        expect_silent(z <- addCT(y, ".", t, v <- 1+numeric(d)))
        .check_data(z, v)
        t <- "translation"
        expect_error(addCT(y, ".", t, numeric(d+1))) # too many
        expect_error(addCT(y, ".", t, character(d))) # not a number
        expect_silent(z <- addCT(y, ".", t, v <- numeric(d)))
        .check_data(z, v)
    }
})

test_that(".coord2graph", {
    # object-wide
    g <- .coord2graph(x)
    expect_is(g, "graph")
    # graph should contain node for
    # every element & transformation
    ns <- lapply(setdiff(.LAYERS, "tables"), 
        \(l) lapply(names(x[[l]]), 
        \(e) c(e, CTname(x[[l]][[e]]))))
    ns <- sort(unique(unlist(ns)))
    expect_true(all(ns %in% sort(nodes(g))))
    # element-wise
    for (l in setdiff(.LAYERS, "tables")) 
        for (e in names(x[[l]])) {
            y <- x[[l]][[e]]
            g <- .coord2graph(y)
            expect_is(g, "graph")
            expect_true("self" %in% nodes(g))
        }
})