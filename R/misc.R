#' @name misc
#' @title Miscellaneous `Miro` methods
#' @description ...
#'
#' @param object \code{\link{SpatialData}} object or one of its 
#'   elements, i.e., an Image/LabelArray or Point/ShapeFrame.
#'
#' @return \code{NULL}
#'
#' @author Helena L. Crowell
#'
#' @examples
#' # TODO
NULL

#' @importFrom RBGL sp.between
#' @importFrom S4Vectors coolcat
#' @importFrom graph nodeData nodes
.showSpatialData <- function(object) {
    cat("class: SpatialData\n")
    i <- imageNames(object)
    l <- labelNames(object)
    p <- pointNames(object)
    s <- shapeNames(object)
    t <- tableNames(object)
    # helper function for concise printing
    f <- \(l, e, d) {
        cat(sprintf("- %s(%s):\n", l, n <- length(e)))
        for (. in seq_len(n)) {
            if (. == 4) cat("    ...\n")
            if (. < 4 || . == n) cat(sprintf("  - %s (%s)\n", e[.], d[.]))
        }
    }
    # images
    d <- lapply(images(object), dim)
    d <- lapply(d, paste, collapse=",")
    f("images", i, d)
    # labels
    d <- lapply(labels(object), dim)
    d <- lapply(d, paste, collapse=",")
    f("labels", l, d)
    # points
    d <- lapply(points(object), length)
    f("points", p, d)
    # shapes
    nc <- vapply(shapes(object), ncol, numeric(1))
    geom <- ifelse(nc == 1, "polygon", "circle")
    d <- vapply(shapes(object), nrow, numeric(1))
    d <- paste(d, unname(geom), sep=",")
    f("shapes", s, d)
    # tables
    d <- lapply(tables(object), dim)
    d <- lapply(d, paste, collapse=",")
    f("tables", t, d)
    # spaces
    cat("spaces:\n")
    e <- c(i, l, s, p)
    g <- CTgraph(object)
    t <- nodeData(g, nodes(g), "type")
    n <- nodes(g)[t == "space"]
    for (. in seq_along(n)) {
        if (. == 4) cat("...\n")
        if (. < 4 || . == length(n)) {
            pa <- suppressWarnings(sp.between(g, e, c <- n[[.]]))
            ex <- vapply(pa, \(.) is.na(.$length), logical(1))
            ss <- strsplit(names(pa), ":"); ss <- ss[!ex]
            coolcat(
                paste0("- ", c, "(%d): %s"),
                vapply(ss, \(.) .[1], character(1)))
            }
    }
}

#' @rdname misc
setMethod("show", "SpatialData", .showSpatialData)

#' @importFrom S4Vectors coolcat
.showImageArray <- function(object) {
    n.object <- length(object@data)
    cat("class: ImageArray", ifelse(n.object > 1, "(MultiScale)", ""),"\n")
    scales <- vapply(object@data, \(x) sprintf("(%s)", paste0(dim(x), collapse=",")), character(1))
    coolcat("Scales (%d): %s", scales)
}

#' @rdname misc
setMethod("show", "ImageArray", .showImageArray)

#' @importFrom S4Vectors coolcat
.showLabelArray <- function(object) {
    cat("class: LabelArray\n")
    cat("dim:", dim(object@data))
}

#' @rdname misc
setMethod("show", "LabelArray", .showLabelArray)

#' @importFrom S4Vectors coolcat
.showPointFrame <- function(object) {
    cat("class: PointFrame\n")
    cat("count:", length(object), "\n")
    coolcat("data(%d): %s\n", names(object))
}

#' @rdname misc
setMethod("show", "PointFrame", .showPointFrame)

#' @importFrom S4Vectors coolcat
.showShapeFrame <- function(object) {
    cat("class: ShapeFrame\n")
    cat("count:", length(object), "\n")
    coolcat("data(%d): %s\n", names(object))
}

#' @rdname misc
setMethod("show", "ShapeFrame", .showShapeFrame)