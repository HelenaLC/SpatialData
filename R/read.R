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
#' @param anndataR, logical, default FALSE
#'   Uses \code{anndataR} to read tables if TRUE.
#'   Uses \code{basilisk}, \code{anndata} and \code{zellkonverter} to read tables if FALSE.
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
readImage <- function(x, ...) {
    md <- fromJSON(file.path(x, ".zattrs"))
    paths <- .get_multiscales_dataset_paths(md)
    za_list <- lapply(paths, function(ps)
        ZarrArray(file.path(x, as.character(ps))))
    meta <- Zattrs(md)
    ImageArray(data=za_list, meta=meta, ...)
}

#' @export
readLabel <- function(x, ...) {
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
readPoint <- function(x, ...) {
    md <- .checkAndReadZAttr(x)
    pqdata <- .checkAndReadGeoParquet(x)
    PointFrame(data=pqdata, meta=Zattrs(md))
}

#' @rdname readSpatialData
#' @importFrom jsonlite fromJSON
#' @importFrom arrow open_dataset
#' @export
readShape <- function(x, ...) {
    md <- .checkAndReadZAttr(x)
    pqdata <- .checkAndReadGeoParquet(x)
    ShapeFrame(data=pqdata, meta=Zattrs(md))
}

#' @importFrom basilisk BasiliskEnvironment
.env <- BasiliskEnvironment(
    pkgname="SpatialData", envname="anndata_env",
    packages=c("anndata==0.9.1", "zarr==2.14.2", "spatialdata==0.2.3", "spatialdata-io==0.1.4"))

#' @importFrom reticulate import
#' @importFrom zellkonverter AnnData2SCE
#' @importFrom SingleCellExperiment int_metadata<-
#' @importFrom basilisk basiliskStart basiliskStop basiliskRun
.readTable_basilisk <- function(x) {
    proc <- basiliskStart(.env)
    on.exit(basiliskStop(proc))
    sce <- basiliskRun(proc, zarr=x, \(zarr) {
        ad <- import("anndata")
        ad <- ad$read_zarr(zarr)
        AnnData2SCE(ad)
    })
    nm <- names(md <- metadata(sce))
    int_metadata(sce)[[nm]] <- md[[nm]]
    metadata(sce) <- list()
    return(sce)
}

#' @rdname readSpatialData
#' @importFrom anndataR read_zarr to_SingleCellExperiment
.readTable_anndataR <- function(x) {
    if (!requireNamespace('anndataR', quietly=TRUE)) {
        stop("To use this function, install the 'anndataR' package via\n",
            "`BiocManager::install(\"keller-mark/anndataR\", ref=\"spatialdata\")`")
    }
    if (!requireNamespace('pizzarr', quietly=TRUE)) {
        stop("To use this function, install the 'pizzarr' package via\n",
            "`BiocManager::install(\"keller-mark/pizzarr\")`")
    }
    adata <- anndataR::read_zarr(x)
    anndataR::to_SingleCellExperiment(adata)
}

#' @rdname readSpatialData
#' @export
readTable <- function(x, anndataR=FALSE) {
    if (anndataR) {
        .readTable_anndataR(x)
    } else {
        .readTable_basilisk(x)
    }
}

#' @rdname readSpatialData
#' @export
readSpatialData <- function(x, 
    images=NULL, labels=NULL, points=NULL, 
    shapes=NULL, tables=NULL, anndataR=FALSE) {
    args <- as.list(environment())[.LAYERS]
    skip <- vapply(args, isFALSE, logical(1))
    lapply(.LAYERS[!skip], \(i) {
        j <- list.files(file.path(x, i), full.names=TRUE)
        if (is.numeric(args[[i]])) j <- j[args[[i]]]
        i <- paste0(toupper(substr(i, 1, 1)), substr(i, 2, nchar(i)-1))
        args <- if (i == "tables") list(anndataR=anndataR)
        lapply(j, \(.) {
            f <- get(paste0("read", i))
            out <- do.call(f, c(list(.), args))
        }) |> 
            setNames(basename(j)) 
    }) |> do.call(what=SpatialData)
}
