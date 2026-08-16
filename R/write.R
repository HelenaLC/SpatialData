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
#' @param path path to zarr store.
#' @param replace if TRUE, existing elements with the same name will be
#' replaced with the given element
#' @param version SpatialData version, 0.1 (zarr v2) or 0.2 (zarr v3)
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
writeSpatialData <- function(x, path, replace = TRUE, version = "0.2",
                             ...) {
    fmt <- sdFormat(version)
    zs <- .write_replace_zarr_group(path, 
                                    "",
                                    replace, 
                                    version=zarr_version(fmt))

    # write root-level spatialdata_attrs for v3 
    # (scverse/spatialdata uses this to pick the read path)
    if (version == "0.2")
        Rarr::write_zarr_attributes(zs, new.zattrs=list(
            spatialdata_attrs=list(version=version),
            spatialdata_software_version=
                paste0("SpatialData v", packageVersion("spatialdataR"))))

    # helper for layer writing
    .writeLayer <- \(l) {
        s <- substr(l, 1, nchar(l)-1)
        g <- match.fun(s)
        f <- match.fun(paste0("write", 
                              toupper(substr(s, 1, 1)), 
                              substr(s, 2, nchar(s))))
        nms <- match.fun(paste0(s, "Names"))(x)
        lapply(nms, \(.) f(g(x, .), ., path=zs, replace=replace, format=fmt))
    }

    invisible(lapply(.LAYERS, .writeLayer))
}

#' @rdname writeSpatialData
#' @export
writePoint <- function(x, name, path, replace = TRUE, 
                       format = sdFormat("0.1")) {
  
  # if no PointFrames were written before, update zarr store
  zarr.group <- .write_replace_zarr_group(file.path(path, "points"),
                                          name, 
                                          replace, 
                                          version = zarr_version(format))
  
  # write meta
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = meta(x))
  
  # version
  version(x) <- point(format)
  
  # write data
  arrow::write_dataset(.point_to_xy(data(x)), 
                       file.path(zarr.group, "points.parquet"),
                       basename_template = "part.{i}.parquet")
}

#' @rdname writeSpatialData
#' @importFrom duckspatial ddbs_write_dataset
#' @importFrom Rarr write_zarr_attributes
#' @export
writeShape <- function(x, name, path, replace = TRUE, 
                       format = sdFormat("0.1")) {
  
  # if no ShapeFrames were written before, update zarr store
  zarr.group <- .write_replace_zarr_group(file.path(path, "shapes"), 
                                          name,
                                          replace, 
                                          version = zarr_version(format))
  
  # write meta
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = meta(x))
  
  # version
  version(x) <- shape(format)
  
  # write data as a single parquet file (matches Python spatialdata convention)
  duckspatial::ddbs_write_dataset(
    data(x),
    file.path(zarr.group, "shapes.parquet"),
    overwrite = TRUE,
    quiet = TRUE
  )}

#' @rdname writeSpatialData
#' @importFrom Rarr write_zarr_array write_zarr_attributes
#' @export
writeImage <- function(x, name, path, replace = TRUE, 
                       format = sdFormat("0.1")) {
  
  # if no ImageArray were written before, update zarr store
  zarr.group <- .write_replace_zarr_group(file.path(path, "images"), 
                                          name,
                                          replace, 
                                          version = zarr_version(format))
  
  # write meta:
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = meta(x))
  
  # version
  version(x) <- image(format)
  
  # write data
  dimension_names <- vapply(axes(meta(x)), \(.) .$name, character(1))
  lapply(
    as.numeric(datasets(meta(x))),
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
                             zarr_version = zarr_version(format))
    }
  )
}

#' @rdname writeSpatialData
#' @importFrom Rarr write_zarr_array write_zarr_attributes
#' @export
writeLabel <- function(x, name, path, replace = TRUE, 
                       format = sdFormat("0.1")) {
  
  # if no LabelArray were written before, update zarr store
  zarr.group <- .write_replace_zarr_group(file.path(path, "labels"),
                                          name,
                                          replace,
                                          version = zarr_version(format))
  
  # write meta:
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = meta(x))
  
  # version
  version(x) <- label(format)
  
  # write data
  dimension_names <- vapply(axes(meta(x)), \(.) .$name, character(1))
  lapply(
    as.numeric(datasets(meta(x))),
    \(.){
      arr <- realize(data(x, . + 1))
      if (!is.null(dimension_names))
        dimnames(arr) <- setNames(vector("list", length(dim(arr))), dimension_names)
      Rarr::write_zarr_array(arr,
                             zarr_array_path = file.path(zarr.group, .),
                             chunk_dim = dim(arr),
                             order = "C",
                             dimension_separator = "/",
                             zarr_version = zarr_version(format))
    }
  )
}

#' @rdname writeSpatialData
#' @importFrom Rarr write_zarr_attributes
#' @importFrom anndataR write_zarr
#' @export
writeTable <- function(x, name, path, replace = TRUE, 
                       format = sdFormat("0.1")) {
  
  # if no Table were written before, update zarr store
  zarr.group <- .write_replace_zarr_group(file.path(path, "tables"), 
                                          name, 
                                          replace,
                                          version = zarr_version(format))
  
  # write meta:
  Rarr::write_zarr_attributes(zarr.group, new.zattrs = meta(x))
  
  # version
  version(x) <- table(format)
  
  # write data
  if(zarr_version(format) == 3)
    stop("Write support for anndata v3 zarr is not supported yet!")
  anndataR::write_zarr(x, path = zarr.group, mode = "a")
}

# utils ----

#' @noRd
#' @importFrom Rarr write_zarr_group
.write_replace_zarr_group <- function(path, name, replace, version){
  ng <- file.path(path, name)
  
  # create element parent dir
  if(!dir.exists(ng))
    dir.create(ng, recursive = TRUE)
  
  # check element dir
  if(replace){
    unlink(ng, recursive = TRUE)
  } else {
    stop("Directory \"", ng, "\" already exists. ",
         "Use 'replace=TRUE' to replace it. ",
         "Its content will be lost!")
  }
  
  # create group
  write_zarr_group(path, name, zarr_version = version)
  
  return(ng)
}

#' @importFrom dplyr bind_cols tibble
.point_to_xy <- function(data) {
  data %>%
    st_as_sf() %>%
    {
      coords <- st_coordinates(.)
      
      bind_cols(
        tibble(
          x = coords[,1],
          y = coords[,2]
        ),
        .
      )
    } %>%
    select(-geometry)
}
