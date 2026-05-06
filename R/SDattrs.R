#' @name SDattrs
#' @title \code{SpatialData} attributes
#' 
#' @param x depends on which attributes are available; 
#'   specifically, \code{PointFrame} (\code{feature/instance_key}), or
#'   \code{SingleCellExperiment} (\code{region}, \code{region/instance_key}),
#' 
#' @return character string
#'
#' @examples
#' x <- file.path("extdata", "blobs.zarr")
#' x <- system.file(x, package="SpatialData")
#' x <- readSpatialData(x)
#' 
#' region(table(x))
#' region_key(table(x))
#' 
#' instance_key(point(x))
#' fk <- feature_key(point(x))
#' base::table(point(x)[[fk]])
NULL

#' @export
#' @rdname SDattrs
setMethod("feature_key", "PointFrame", \(x) feature_key(meta(x)))
#' @export
#' @rdname SDattrs
setMethod("feature_key", "Zattrs", \(x) x$spatialdata_attrs$feature_key)
#' @export
#' @rdname SDattrs
setReplaceMethod("feature_key", c("Zattrs", "character"), 
    \(x, value) { x$spatialdata_attrs$feature_key <- value; x })

# region(s) ----

#' @export
#' @rdname SDattrs
setMethod("region_key", "SingleCellExperiment", \(x) meta(x)$region_key)

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region_key", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(length(value) == 1, nchar(value) > 0)
    int_metadata(x)$spatialdata_attrs$region_key <- value
    return(x)
})

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region_key", c("SingleCellExperiment", "NULL"), \(x, value) {
    int_metadata(x)$spatialdata_attrs$region_key <- value
    return(x)
})

#' @export
#' @rdname SDattrs
setMethod("region", "SingleCellExperiment", \(x) {
    rk <- region_key(x)
    if (is.null(rk)) return(NULL)
    meta(x)[[rk]]
})

#' @export
#' @rdname SDattrs
#' @importFrom SingleCellExperiment int_colData
setMethod("regions", "SingleCellExperiment", \(x) {
    rk <- region_key(x)
    if (is.null(rk)) return(NULL)
    int_colData(x)[[rk]]
})

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(all(nchar(value) > 0, na.rm=TRUE))
    if (is.null(rk <- region_key(x))) 
        rk <- region_key(x) <- "region"
    int_metadata(x)$spatialdata_attrs[[rk]] <- sort(unique(value))
    return(x)
})

# internal use only!
#' @noRd
#' @importFrom SingleCellExperiment int_metadata<-
setReplaceMethod("region", c("SingleCellExperiment", "NULL"), \(x, value) {
    if (!is.null(rk <- region_key(x)))
        int_metadata(x)$spatialdata_attrs[[rk]] <- value
    return(x)
})

#' @export
#' @rdname SDattrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("regions", c("SingleCellExperiment", "character"), \(x, value) {
    stopifnot(length(value) %in% c(1, ncol(x)))
    stopifnot(all(nchar(value) > 0, na.rm=TRUE))
    if (is.null(rk <- region_key(x))) region_key(x) <- "region"
    int_metadata(x)$spatialdata_attrs[[rk]] <- sort(unique(value))
    int_colData(x)[[rk]] <- value
    return(x)
})

#' @export
#' @rdname SDattrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("regions", c("SingleCellExperiment", "NULL"), \(x, value) {
    if (!is.null(rk <- region_key(x))) {
        int_metadata(x)$spatialdata_attrs[[rk]] <- value
        int_colData(x)[[rk]] <- value
    }
    region_key(x) <- value
    return(x)
})

# instances ----

# NOTE: does not apply to images
#' @export
#' @rdname SDattrs
setMethod("instance_key", "list", \(x) x$instance_key)
#' @export
#' @rdname SDattrs
setMethod("instance_key", "SingleCellExperiment", \(x) instance_key(meta(x)))
#' @export
#' @rdname SDattrs
setMethod("instance_key", "sdFrame", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SDattrs
setMethod("instance_key", "LabelArray", \(x) instance_key(meta(x)$spatialdata_attrs))
#' @export
#' @rdname SDattrs
setReplaceMethod("instance_key", c("Zattrs", "character"), \(x, value) {
    x$spatialdata_attrs$instance_key <- value
    return(x)
})
#' @export
#' @rdname SDattrs
setReplaceMethod("instance_key", c("SingleCellExperiment", "character"), \(x, value) {
    int_metadata(x)$spatialdata_attrs$instance_key <- value
    return(x)
})

#' @export
#' @rdname SDattrs
setMethod("instances", "LabelArray", \(x) {
    # unique values in first scale, excluding 0
    z <- data(x, 1)
    as.integer(setdiff(unique(as.vector(z)), 0))
})
#' @export
#' @rdname SDattrs
#' @importFrom dplyr pull
setMethod("instances", "PointFrame", \(x) pull(data(x), instance_key(x)))
#' @export
#' @rdname SDattrs
setMethod("instances", "ShapeFrame", \(x) {
    ik <- tryCatch(instance_key(x), error=\(e) NULL)
    if (is.null(ik)) return(seq_len(nrow(x)))
    pull(data(x), ik)
})
#' @export
#' @rdname SDattrs
#' @importFrom SingleCellExperiment int_colData
setMethod("instances", "SingleCellExperiment", \(x) {
    if (is.null(ik <- instance_key(x))) 
        stop("no 'instance_key' found in 'x'")
    int_colData(x)[[ik]]
})

#' @export
#' @rdname SDattrs
#' @importFrom SingleCellExperiment int_colData<-
setReplaceMethod("instances", c("SingleCellExperiment", "ANY"), \(x, value) {
    ik <- instance_key(x)
    if (is.null(ik)) 
        ik <- "instance_id"
    int_colData(x)[[ik]] <- value
    return(x)
})

# elements ----

setMethod("version", c("SpatialDataElement"), \(x) {
  version(meta(x))
})

setMethod("version", c("SingleCellExperiment"), \(x) {
  meta(x)$version
})

setMethod("version", "Zattrs", \(x) {
  x$spatialdata_attrs$version
})

setReplaceMethod("version", c("sdFrame"), \(x, value) {
  if(!version %in% c("0.2", "0.3"))
    stop("Unknown version for shape/point! Must be 0.2 or 0.3.")
  meta(x)$spatialdata_attrs$version <- value
  x
})

setReplaceMethod("version", c("sdArray"), \(x, value) {
  mt <- meta(x)
  if(value == "0.3"){
    if(is.null(mt$ome)){
      print("here")
      mt$ome = list(omero = mt$omero, 
                    multiscales = mt$multiscales)
      mt$omero <- NULL
      mt$multiscales <- NULL 
    }
  } else if(value == "0.2"){
    if(is.null(mt$multiscales)){
      mt$omero <- mt$ome$omero
      mt$multiscales <- mt$ome$multiscales
      mt[["ome"]] <- NULL
    }
  } else {
    stop("Unknown version for image/label! Must be 0.2 or 0.3.")
  }
  mt$spatialdata_attrs$version <- value
  meta(x) <- mt
  x
})