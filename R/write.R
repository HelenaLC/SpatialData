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

  # write root-level spatialdata_attrs for v3 (Python uses this to pick the read path)
  if (version == "v3")
    Rarr::write_zarr_attributes(zarr.path,
      new.zattrs = list(spatialdata_attrs = list(version = "0.2")))

  # write points
  . <- lapply(pointNames(x), \(.){
    writePoint(point(x, .),., path = zarr.path, replace = replace, version = version)
  })

  # write shapes
  . <- lapply(shapeNames(x), \(.){
    writeShape(shape(x, .),., path = zarr.path, replace = replace, version = version)
  })

  # write images
  . <- lapply(imageNames(x), \(.){
    writeImage(image(x, .),., path = zarr.path, replace = replace, version = version)
  })

  # write labels
  . <- lapply(labelNames(x), \(.){
    writeLabel(label(x, .),., path = zarr.path, replace = replace, version = version)
  })

  # write labels group metadata listing all label names (required by spatialdata spec)
  # v2: {"labels": [...]}, v3: {"ome": {"labels": [...]}}
  lnames <- labelNames(x)
  if (length(lnames) > 0L) {
    labels.dir <- file.path(zarr.path, "labels")
    lnames_zattrs <- if (version == "v3")
      list(ome = list(labels = as.list(lnames))) else
      list(labels = as.list(lnames))
    Rarr::write_zarr_attributes(labels.dir, new.zattrs = lnames_zattrs)
  }
}

#' @rdname writeSpatialData
#' @export
writePoint <- function(x, name, path, replace = TRUE, version = "v2") {
  
  # if no PointFrames were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, file.path(path, "points"), replace, version)
  
  # write meta
  zattrs <- as.list(meta(x))
  if (version == "v3") zattrs$spatialdata_attrs$version <- "0.2"
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = zattrs)
  
  # write data
  arrow::write_dataset(data(x), file.path(zarr.group, "points.parquet"),
                       basename_template = "part.{i}.parquet")
}

#' @rdname writeSpatialData
#' @export
writeShape <- function(x, name, path, replace = TRUE, version = "v2") {
  
  # if no ShapeFrames were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, file.path(path, "shapes"), replace, version)
  
  # write meta
  zattrs <- as.list(meta(x))
  if (version == "v3") zattrs$spatialdata_attrs$version <- "0.3"
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = zattrs)
  
  # write data as a single parquet file (matches Python spatialdata convention)
  arrow::write_parquet(data(x), file.path(zarr.group, "shapes.parquet"))
}

#' @rdname writeSpatialData
#' @importFrom Rarr write_zarr_array
#' @importFrom DelayedArray realize
#' @export
writeImage <- function(x, name, path, replace = TRUE, version = "v2") {
  
  # if no ImageArray were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, file.path(path, "images"), 
                                 replace, version)
  # dimension_names <- .get_multiscale_axes(meta(x))
  dimension_names <- vapply(axes(meta(x)), \(.) .$name, character(1))

  # write meta: for v3, OME-NGFF content goes under "ome" key in attributes
  zattrs <- .wrap_ome_for_v3(meta(x), version)
  if (version == "v3") zattrs$spatialdata_attrs$version <- "0.3"
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = zattrs)
  
  # write data
  lapply(
    .get_multiscales_dataset_paths(meta(x)),
    \(.){
      arr <- realize(data(x, . + 1))
      # Rarr reads names(dimnames(x)) to write dimension_names in v3 zarr.json
      if (!is.null(dimension_names))
        dimnames(arr) <- setNames(vector("list", length(dim(arr))), dimension_names)
      Rarr::write_zarr_array(arr,
                             zarr_array_path = file.path(zarr.group, .),
                             chunk_dim = dim(arr),
                             order = "C",
                             dimension_separator = "/",
                             zarr_version = if (version == "v3") 3L else 2L)
      if (version == "v3")
        .normalize_v3_array_metadata(file.path(zarr.group, .))
    }
  )
}

#' @rdname writeSpatialData
#' @importFrom Rarr write_zarr_array
#' @importFrom DelayedArray realize
#' @export
writeLabel <- function(x, name, path, replace = TRUE, version = "v2") {
  
  # if no LabelArray were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, file.path(path, "labels"), 
                                 replace, version)
  # dimension_names <- .get_multiscale_axes(meta(x))
  dimension_names <- vapply(axes(meta(x)), \(.) .$name, character(1))
  
  # write meta: for v3, OME-NGFF content goes under "ome" key in attributes
  zattrs <- .wrap_ome_for_v3(meta(x), version)
  if (version == "v3") zattrs$spatialdata_attrs$version <- "0.3"
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = zattrs)
  
  # write data
  lapply(
    .get_multiscales_dataset_paths(meta(x)),
    \(.){
      arr <- realize(data(x, . + 1))
      if (!is.null(dimension_names))
        dimnames(arr) <- setNames(vector("list", length(dim(arr))), dimension_names)
      Rarr::write_zarr_array(arr,
                             zarr_array_path = file.path(zarr.group, .),
                             chunk_dim = dim(arr),
                             order = "C",
                             dimension_separator = "/",
                             zarr_version = if (version == "v3") 3L else 2L)
      if (version == "v3")
        .normalize_v3_array_metadata(file.path(zarr.group, .))
    }
  )
}
