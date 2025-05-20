#' @name writeSpatialData
#' @title Writing `SpatialData`
#' 
#' @aliases writeImage writeLabel writePoint writeShape writeTable
#'
#' @param x 
#'   For \code{writeImage/Label/Point/Shape/Table}, 
#'   path to a \code{SpatialData} element.
#'   For \code{writeSpatialData},
#'   path to a \code{SpatialData}-.zarr store.
#' @param ... option arguments passed to and from other methods.
#'
#' @return 
#' \itemize{
#' \item{For \code{writeSpatialData}, a \code{SpatialData}.},
#' \item{For element writers, a \code{ImageArray}, \code{LabelArray}, 
#' \code{PointFrame}, \code{ShapeFrame}, or \code{SingleCellExperiment}.}}
#'
#' @examples
#' 
NULL

#' @rdname writeSpatialData
#' @importFrom jsonlite fromJSON
#' @importFrom arrow open_dataset
#' @export
writePoint <- function(x, ...) {
  md <- fromJSON(file.path(x, ".zattrs"))
  pq <- list.files(x, "\\.parquet$", full.names=TRUE)
  PointFrame(data=open_dataset(pq), meta=Zattrs(md))
}