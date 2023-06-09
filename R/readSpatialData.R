#' @rdname readSpatialData
#' @title Read `SpatialData` OME-Zarr
#' @description ...
#'
#' @param path A character string specifying the path to an
#'   OME-Zarr file adhering to \code{SpatialData} specification.
#' @param ... Further arguments to be passed to or from other methods.
#'
#' @return \code{\link{SpatialData}}
#'
#' @examples
#' path <- file.path("extdata", "blobs")
#' path <- system.file(path, package="SpatialData")
#' (spd <- readSpatialData(path))
#'
#' @author Constantin Ahlmann-Eltze, Helena L. Crowell
#'
#' @export
readSpatialData <- function(path, ...) {
    layers <- list.dirs(path, recursive=FALSE)

    # used internally only atm
    dots <- list(...)
    i <- if (is.null(n <- dots$n)) TRUE else
        if (length(n) == 1) seq_len(n) else n

    images <- if ("images" %in% basename(layers)) {
        images <- list.dirs(file.path(path, "images"), recursive=FALSE)
        names(images) <- basename(images)
        lapply(images[i], readArray)
    } else list()

    labels <- if ("labels" %in% basename(layers)) {
        labels <- list.dirs(file.path(path, "labels"), recursive=FALSE)
        names(labels) <- basename(labels)
        lapply(labels[i], readArray)
    } else list()

    shapes <- if ("shapes" %in% basename(layers)) {
        shapes <- list.dirs(file.path(path, "shapes"), recursive=FALSE)
        names(shapes) <- basename(shapes)
        lapply(shapes[i], readShapes)
    } else list()

    points <- if ("points" %in% basename(layers)) {
        points <- list.dirs(file.path(path, "points"), recursive=FALSE)
        names(points) <- basename(points)
        lapply(points[i], readPoints)
    } else list()

    table <- if ("table" %in% basename(layers)) {
        tryCatch(
            error=function(e) NULL,
            readTable(file.path(path, "table/table")))
    }

    SpatialData(
        images=images,
        labels=labels,
        shapes=shapes,
        points=points,
        table=table)
}
