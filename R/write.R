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

# For zarr v3, OME-NGFF content (multiscales, omero, image-label) must be
# nested under an "ome" key inside "attributes"; spatialdata_attrs stays at top.
# If the metadata was read from a v3 store it already has "ome", so skip wrapping.
.wrap_ome_for_v3 <- function(zattrs, version) {
  if (version != "v3" || "ome" %in% names(zattrs)) return(as.list(zattrs))
  ome_keys <- setdiff(names(zattrs), "spatialdata_attrs")
  ome_content <- as.list(zattrs)[ome_keys]
  # Strip v2-only fields from each multiscales entry
  if (!is.null(ome_content$multiscales)) {
    ome_content$multiscales <- lapply(ome_content$multiscales, function(ms) {
      ms[setdiff(names(ms), c("version", "metadata"))]
    })
  }
  list(
    ome = c(list(version = "0.5-dev-spatialdata"), ome_content),
    spatialdata_attrs = zattrs[["spatialdata_attrs"]]
  )
}

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
  zarr.group <- .make_zarr_group(x, name, file.path(path, "images"), replace, version)
  zarr_version <- if (version == "v3") 3L else 2L
  # write meta: for v3, OME-NGFF content goes under "ome" key in attributes
  zattrs <- .wrap_ome_for_v3(meta(x), version)
  if (version == "v3") zattrs$spatialdata_attrs$version <- "0.3"
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = zattrs)
  # write data
  lapply(
    .get_multiscales_dataset_paths(meta(x)),
    \(.){
      da <- data(x, . + 1)
      Rarr::write_zarr_array(realize(da),
                             zarr_array_path = file.path(zarr.group, .),
                             chunk_dim = dim(da),
                             order = "C",
                             dimension_separator = "/",
                             zarr_version = zarr_version)
    }
  )
}

#' @rdname writeSpatialData
#' @importFrom Rarr write_zarr_array
#' @importFrom DelayedArray realize
#' @export
writeLabel <- function(x, name, path, replace = TRUE, version = "v2") {
  # if no LabelArray were written before, update zarr store
  zarr.group <- .make_zarr_group(x, name, file.path(path, "labels"), replace, version)
  zarr_version <- if (version == "v3") 3L else 2L
  # write meta: for v3, OME-NGFF content goes under "ome" key in attributes
  zattrs <- .wrap_ome_for_v3(meta(x), version)
  if (version == "v3") zattrs$spatialdata_attrs$version <- "0.3"
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = zattrs)
  # write data
  lapply(
    .get_multiscales_dataset_paths(meta(x)),
    \(.){
      da <- data(x, . + 1)
      Rarr::write_zarr_array(realize(da),
                             zarr_array_path = file.path(zarr.group, .),
                             chunk_dim = dim(da),
                             order = "C",
                             dimension_separator = "/",
                             zarr_version = zarr_version)
    }
  )
}