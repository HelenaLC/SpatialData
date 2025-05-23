#' @name table-utils
#' @title \code{SpatialData} annotations
#' @aliases hasTable getTable setTable valTable
#' 
#' @param x \code{\link{SpatialData}} object.
#' @param i character string; name of the
#'   element for which to get/set a \code{table}.
#' @param j character string; \code{colData} column, 
#'   or row name to retrieve \code{assay} data.
#' @param drop logical; should observations (columns) 
#'   that don't belong to \code{i} be filtered out?
#' @param name logical; should the \code{table} 
#'   name be returned instead of TRUE/FALSE?
#' @param assay character string or scalar integer; 
#'   specifies which \code{assay} to use when \code{j} is a row name.
#' @param rk,ik character string; region and instance key (the latter will be 
#'   ignored if an instance key is already specified within element \code{i}).
#' @param ... \code{data.frame} or list of data generation function(s) 
#'   that accept an argument for the number of observations; see examples.
#'   
#' @examples
#' library(SingleCellExperiment)
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x, anndataR=FALSE)
#' 
#' # check if element has a 'table'
#' hasTable(x, "blobs_points")
#' hasTable(x, "blobs_labels")
#' 
#' # retrieve 'table' for element 'i'
#' sce <- getTable(x, i="blobs_labels")
#' head(colData(sce))
#' meta(sce)
#' 
#' # get values from 'table' 
#' valTable(x, 
#'   i="blobs_labels", 
#'   j="channel_0_sum")
#' 
#' # add 'table' annotating an element 'i'
#' # (w/ or w/o supplying additional data)
#' 
#' # labels
#' y <- x; tables(y) <- list()
#' y <- setTable(y, i <- "blobs_labels")
#' head(colData(sce <- getTable(y, i)))
#' 
#' # points
#' y <- setTable(x, i <- "blobs_points")
#' head(colData(sce <- getTable(y, i)))
#' 
#' # labels
#' y <- setTable(x, i <- "blobs_circles")
#' head(colData(sce <- getTable(y, i)))
#' 
#' # list of data generating functions
#' f <- list(
#'   numbers=\(n) runif(n),
#'   letters=\(n) sample(letters, n, TRUE))
#' 
#' args <- c(list(x, i <- "blobs_points"), f)
#' y <- do.call(setTable, args)
#' head(colData(getTable(y, i)))
#' 
#' # passing a preconstructed 'data.frame'
#' id <- unique(point(x, i)$instance_id)
#' df <- data.frame(n=runif(length(id)))
#' 
#' y <- setTable(x, i, df)
#' head(colData(getTable(y, i)))
NULL

#' @rdname table-utils
#' @export
setMethod("meta", c("SingleCellExperiment"), 
    \(x) int_metadata(x)$spatialdata_attrs)

.invalid_i <- \() stop(
    "invalid 'i'; should be a character ",
    "string specifying an element in 'x'")

# has ----

#' @rdname table-utils
#' @export
setMethod("hasTable", c("SpatialData", "ANY"), \(x, i) .invalid_i())

#' @rdname table-utils
#' @export
setMethod("hasTable", c("SpatialData", "character"), \(x, i, name=FALSE) {
    stopifnot(
        isTRUE(name) || isFALSE(name),
        length(i) == 1, is.character(i))
    # check that 'i' is a non-'table' element name
    nms <- colnames(x)
    idx <- setdiff(names(nms), "tables")
    match.arg(i, unlist(nms[idx]))
    # count occurrences
    t <- lapply(tables(x), \(t) meta(t)$region)
    n <- vapply(seq_along(t), \(.) i %in% t[[.]], numeric(1))
    nan <- all(n == 0)
    # failure when no/many matches
    if (name) {
        dup <- length(unique(n)) != length(n)
        if (nan) stop("no 'table' found for 'i'")
        if (dup) stop("multiple 'table's found for 'i'")
        return(names(t)[n == 1])
    } else return(!nan)
})

# get ----

#' @rdname table-utils
#' @export
setMethod("getTable", c("SpatialData", "ANY"), \(x, i, drop=TRUE) .invalid_i())

#' @rdname table-utils
#' @importFrom SingleCellExperiment int_colData
#' @export
setMethod("getTable", c("SpatialData", "character"), \(x, i, drop=TRUE) {
    stopifnot(isTRUE(drop) || isFALSE(drop))
    # get 'table' annotating 'i', if any
    t <- table(x, hasTable(x, i, name=TRUE))
    # only keep observations belonging to 'i' (optional)
    if (drop) {
        rk <- meta(t)$region_key
        # TODO: check the replacement below, search colData as well?
        # t <- t[, int_colData(t)[[rk]] == i]
        coldata <-
          if(rk %in% names(cd <- int_colData(t))) cd[[rk]] else colData(t)[[rk]]
        t <- t[, coldata == i]
    }
    return(t)
})

# set ----

#' @rdname table-utils
#' @export
setMethod("setTable", c("SpatialData", "ANY"), \(x, i, ..., name=NULL, rk="rk", ik="ik") .invalid_i())

# it seems pull below dispatches to arrow, and a warning on as_vector was being produced
#' @rdname table-utils
#' @importFrom methods as
#' @importFrom dplyr pull     
#' @importFrom sf st_as_sf
#' @importFrom S4Vectors make_zero_col_DFrame 
#' @importFrom SingleCellExperiment SingleCellExperiment 
#'   int_colData int_colData<- int_metadata<- 
#' @export
setMethod("setTable", 
    c("SpatialData", "character"), 
    # TODO: 'assay' data argument
    \(x, i, ..., name=NULL, rk="rk", ik="ik") {
    dots <- list(...)
    if (length(dots) && !is.function(dots[[1]])) dots <- dots[[1]]
    stopifnot(
        length(i) == 1, is.character(i),
        length(rk) == 1, is.character(rk),
        length(ik) == 1, is.character(ik))
    if (!i %in% unlist(colnames(x))) 
        stop(dQuote(i), " is not an element of 'x'")
    if (length(dots)) stopifnot(is.data.frame(dots) || 
        all(vapply(dots, is.function, logical(1))))
    # make up 'name' if not provided
    if (is.null(name)) {
        nt <- length(tables(x))
        name <- paste0("table", nt+1)
    } else if (name %in% tableNames(x)) 
        stop("'table' with name ", dQuote(name),
            " exists; use 'table<-' to replace it.")
    # get element type
    for (l in rownames(x)) 
        for (e in colnames(x)[[l]])
            if (i == e) typ <- l
    sda <- "spatialdata_attrs"
    sce <- switch(typ, 
        labels={
            y <- label(x, i)
            md <- meta(y)[[sda]]
            ki <- md$instance_key
            z <- as(data(y), "DelayedMatrix")
            is <- setdiff(unique(c(z)), 0)
            n <- length(is <- sort(is))
            if (!is.null(ki)) ik <- ki
        },
        points={
            y <- point(x, i)
            md <- meta(y)[[sda]]
            ik <- md$instance_key
            is <- pull(data(y), ik, as_vector=TRUE)  # needed to scotch new warning
            n <- length(is <- unique(is))
        },
        shapes={
            n <- nrow(y <- shape(x, i))
            ex <- c("geometry", "radius")
            ki <- setdiff(names(y), ex)
            if (length(ki)) {
                is <- pull(data(y), ik <- ki, as_vector=TRUE)
            } else {
                # in case of missing 'instance_key', make one
                df <- st_as_sf(data(y))
                is <- seq_len(nrow(df))
                df[[ik]] <- is
                y@data <- df
                shape(x, i) <- y
            }
        }, stop("can't add 'table' for elements in '", typ, "' layer"))
    icd <- cd <- make_zero_col_DFrame(n)
    icd[[rk]] <- i; icd[[ik]] <- is
    # additional data generation (optional)
    if (length(dots) && is.data.frame(dots)) {
        stopifnot(nrow(dots) == nrow(cd))
        cd <- cbind(cd, dots)
    } else for (. in names(dots)) {
        cd[[.]] <- dots[[.]](n)
    }
    # stash 'spatialdata_attrs'
    sce <- SingleCellExperiment(colData=cd)
    int_colData(sce) <- cbind(int_colData(sce), icd)
    md <- list(region=i, region_key=rk, instance_key=ik)
    int_metadata(sce)[[sda]] <- md
    table(x, name) <- sce
    return(x)
})

# val ----

#' @rdname table-utils
#' @importFrom SummarizedExperiment assay colData
#' @export
setMethod("valTable", "SpatialData", \(x, i, j, assay=1, drop=TRUE) {
    stopifnot(length(j) == 1, is.character(j))
    t <- getTable(x, i, drop)
    rs <- j %in% rownames(t)
    cd <- j %in% names(colData(t))
    if (!(rs || cd)) stop("invalid 'j'")
    if (cd) return(t[[j]])
    assay(t, assay)[j, ]
})
