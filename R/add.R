#' @name addSpatialData
#' @title Adding `SpatialElement` objects to `SpatialData`
#' 
#' @aliases addImage addLabel addPoint addShape addTable
#'
#' @param x a \code{SpatialData} object
#' @param ... option arguments passed to and from other methods.
#'
#' @return 
#' \itemize{
#' \item{For \code{addSpatialData}, a \code{SpatialData}.},
#' \item{For element adders, a \code{ImageArray}, \code{LabelArray}, 
#' \code{PointFrame}, \code{ShapeFrame}, or \code{SingleCellExperiment}.}}
#'
#' @examples
#' 
NULL

#' @rdname writeSpatialData
#' @importFrom jsonlite fromJSON
#' @importFrom arrow open_dataset
#' @export
addPoint <- function(x, ...) {
  md <- fromJSON(file.path(x, ".zattrs"))
  pq <- list.files(x, "\\.parquet$", full.names=TRUE)
  PointFrame(data=open_dataset(pq), meta=Zattrs(md))
}