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
  
  # write points
  . <- lapply(pointNames(x), \(.){
    writePoint(point(x, .),., path = zarr.path, replace = replace)
  })
  
  # write shapes
  . <- lapply(shapeNames(x), \(.){
    writeShape(shape(x, .),., path = zarr.path, replace = replace)
  })
  
  # write images
  . <- lapply(imageNames(x), \(.){
    writeImage(image(x, .),., path = zarr.path, replace = replace)
  })
  
  # write labels
  . <- lapply(labelNames(x), \(.){
    writeLabel(label(x, .),., path = zarr.path, replace = replace)
  })
}

#' @rdname writeSpatialData
#' @export
writePoint <- function(x, name, path, replace = TRUE, version = "v2") {
  # if no PointFrames were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, file.path(path, "points"), replace, version)
  # write meta
  write_zattrs(path = zarr.group, meta(x))
  # write data
  arrow::write_dataset(data(x), file.path(zarr.group, "points.parquet"))
}

#' @rdname writeSpatialData
#' @export
writeShape <- function(x, name, path, replace = TRUE, version = "v2") {
  # if no ShapeFrames were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, file.path(path, "shapes"), replace, version)
  # write meta
  write_zattrs(path = zarr.group, meta(x))
  # write data
  arrow::write_dataset(data(x), file.path(zarr.group, "shapes.parquet"))
}

#' @rdname writeSpatialData
#' @export
writeImage <- function(x, name, path, replace = TRUE, version = "v2") {
  # if no ImageArray were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, file.path(path, "images"), replace, version)
  # write meta
  write_zattrs(path = zarr.group, meta(x))
  # write data
  lapply(
    .get_multiscales_dataset_paths(meta(x)),
    \(.){
      da <- data(x, . + 1)
      Rarr::write_zarr_array(realize(da), 
                             zarr_array_path = file.path(zarr.group, .), 
                             chunk_dim = dim(da))
    }
  )
}

#' @rdname writeSpatialData
#' @export
writeLabel <- function(x, name, path, replace = TRUE, version = "v2") {
  # if no LabelArray were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, file.path(path, "labels"), replace, version)
  # write meta
  write_zattrs(path = zarr.group, meta(x))
  # write data
  lapply(
    .get_multiscales_dataset_paths(meta(x)),
    \(.){
      da <- data(x, . + 1)
      Rarr::write_zarr_array(realize(da), 
                             zarr_array_path = file.path(zarr.group, .), 
                             chunk_dim = dim(da))
    }
  )
}