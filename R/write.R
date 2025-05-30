#' @name writeSpatialData
#' @title Writing `SpatialData`
#' 
#' @aliases 
#' writeSpatialData 
#' writeImage writeLabel 
#' writePoint writeShape writeTable
#' 
#' @param x 
#'   For \code{writeSpatialData},
#'   a \code{SpatialData}
#'   For \code{writeImage/Label/Point/Shape/Table}, 
#'   a \code{ImageArray},\code{LabelArray}, 
#'   \code{PointFrame}, \code{ShapeFrame}
#' @param name
#'   For \code{writeSpatialData},
#'   name of the zarr store
#'   For \code{writeImage/Label/Point/Shape/Table}, 
#'   name of spatial element to write in the zarr store
#' @param path path to zarr store.
#' @param replace if TRUE, existing elements with the same name will be
#' replaced with the given element
#' @param version zarr version, v2 or v3 (only v2 is supported now)
#' @param ... option arguments passed to and from other methods.
#'
#' @return 
#' \itemize{
#' \item{For \code{writeSpatialData}, a \code{SpatialData}.},
#' \item{For element writers, a \code{ImageArray}, \code{LabelArray}, 
#' \code{PointFrame}, \code{ShapeFrame}, or \code{SingleCellExperiment}.}}
#'
NULL

#' @rdname writeSpatialData
#' @export
writeSpatialData <- function(x, name, path, replace = TRUE, version = "v2", 
                             ...) {
  zarr.path <- .replace_zarr(name, path, replace, version)
  
  # write points only for now
  . <- lapply(pointNames(x), \(.){
    writePoint(point(x, .),., path = zarr.path, replace = replace)
  })
}

#' @rdname writeSpatialData
#' @export
writePoint <- function(x, name, path, replace = TRUE, version = "v2") {
  # if no PointFrames were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, path, replace, version)
  # write meta
  write_zattrs(path = zarr.group, meta(x))
  # write data
  arrow::write_dataset(data(x), file.path(zarr.group, "points.parquet"))
}

.replace_zarr <- function(name, path, replace, version = "v2")
{
  zarr.path <- file.path(path,name)
  if (dir.exists(zarr.path) && !replace)
    stop("zarr store with name ", zarr.path ," doesnt exist")
  if (!replace)
    stop("Directory \"", zarr.path, "\" already exists. ",
         "Use 'replace=TRUE' to replace it. ",
         "Its content will be lost!")
  if (unlink(zarr.path, recursive=TRUE) != 0L)
    stop("failed to delete directory \"", dir, "\"")
  create_zarr(name, path, version = version)
  return(zarr.path)
}

.make_zarr_group <- function(x, name, path, replace, version){
  gd <- file.path(path, "points")
  if(!dir.exists(gd))
    dir.create(gd)
  ng <- file.path(gd, name)
  if(replace){
    unlink(ng, recursive = TRUE)
  } else {
    nms <- list.dirs(file.path(gd), full.names = FALSE)
    if(name %in% nms)
      stop("Directory \"", ng, "\" already exists. ",
           "Use 'replace=TRUE' to replace it. ",
           "Its content will be lost!")
  }
  create_zarr_group(gd, name, version)
  return(ng)
}