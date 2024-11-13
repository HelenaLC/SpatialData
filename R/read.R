#' @name readSpatialData
#' @title Reading `SpatialData`
#'
#' @aliases readImage readLabel readPoint readShape readTable
#'
#' @param x
#'   For \code{readImage/Label/Point/Shape/Table},
#'   path to a \code{SpatialData} element.
#'   For \code{readSpatialData},
#'   path to a \code{SpatialData}-.zarr store.
#' @param images,labels,points,shapes,tables
#'   Control which elements should be read for each layer.
#'   The default, NULL, reads all elements; alternatively, may be FALSE
#'   to skip a layer, or a integer vector specifying which elements to read.
#'
#' @return
#' \itemize{
#' \item{For element readers, a \code{ImageArray}, \code{LabelArray},
#' \code{PointFrame}, \code{ShapeFrame}, or \code{SingleCellExperiment}.}
#' \item{For \code{readSpatialData}, a \code{SpatialData}.}}
#'
#' @examples
#' tf <- tempfile()
#' dir.create(tf)
#' base <- unzip_merfish_demo(tf)
#' (x <- readSpatialData(base))
NULL

#' @importFrom Rarr ZarrArray
.checkAndReadZAttr <- function(x) {
    fpa <- file.path(x, ".zattrs")
    stopifnot(file.exists(fpa))
    md <- fromJSON(fpa)
    return(md)
}

#' @importFrom jsonlite fromJSON
.checkAndReadZAttrs0File <- function(x) {
    md <- .checkAndReadZAttr(x)
    fp0 <- file.path(x, "0")
    stopifnot(file.exists(fp0))
    za <- ZarrArray(fp0)
    return(list(za=za, md=md))
}
#' @rdname readSpatialData
#' @export
readImage <- function(x) {
    lzamd <- .checkAndReadZAttrs0File(x)
    ImageArray(data=lzamd$za, meta=Zattrs(lzamd$md))
}

#' @export
readLabel <- function(x) {
    lzamd <- .checkAndReadZAttrs0File(x)
    LabelArray(data=lzamd$za, meta=Zattrs(lzamd$md))
}

#' @importFrom arrow open_dataset
.checkAndReadGeoParquet <- function(x) {
    require(geoarrow, quietly=TRUE)
    # TODO: previously had read_parquet(),
    # but that doesn't work with geoparquet? -> especially for readShape
    pq <- list.files(x, "\\.parquet$", full.names=TRUE)
    stopifnot(file.exists(pq))
    pqdata <- open_dataset(pq)
    return(pqdata)
}
#' @rdname readSpatialData
#' @export
readPoint <- function(x) {
    md <- .checkAndReadZAttr(x)
    pqdata <- .checkAndReadGeoParquet(x)
    PointFrame(data=pqdata, meta=Zattrs(md))
}

#' @rdname readSpatialData
#' @importFrom jsonlite fromJSON
#' @importFrom arrow open_dataset
#' @export
readShape <- function(x) {
    md <- .checkAndReadZAttr(x)
    pqdata <- .checkAndReadGeoParquet(x)
    ShapeFrame(data=pqdata, meta=Zattrs(md))
}

#' @importFrom basilisk BasiliskEnvironment
.env <- BasiliskEnvironment(
    pkgname="SpatialData", envname="anndata_env",
    packages=c("anndata==0.9.1", "zarr==2.14.2"))

#' @rdname readSpatialData
#' @importFrom reticulate import
#' @importFrom zellkonverter AnnData2SCE
#' @importFrom basilisk basiliskStart basiliskStop basiliskRun
#' @export
readTable <- function(x) {
    # TODO: temporary, until AnnDataR supports
    # reading AnnData from .zarr (.h5ad only atm)
    proc <- basiliskStart(.env)
    on.exit(basiliskStop(proc))
    basiliskRun(proc, zarr=x, \(zarr) {
        ad <- import("anndata")
        ad <- ad$read_zarr(zarr)
        AnnData2SCE(ad)
    })
}

#' @rdname readSpatialData
#' @export
readSpatialData <- function(x, images=NULL, labels=NULL, points=NULL, shapes=NULL, tables=NULL) {
    # TODO: validity checks
    stopifnot(dir.exists(x))
    args <- as.list(environment())[.LAYERS]
    skip <- vapply(args, isFALSE, logical(1))
    lapply(.LAYERS[!skip], \(i) {
        j <- list.files(file.path(x, i), full.names=TRUE)
        if (is.numeric(args[[i]])) j <- j[args[[i]]]
        i <- paste0(toupper(substr(i, 1, 1)), substr(i, 2, nchar(i)-1))
        setNames(lapply(j, get(paste0("read", i))), basename(j))
    }) |> do.call(what=SpatialData)
}
