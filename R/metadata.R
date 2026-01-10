.make_pointshape_meta <- function(x, 
                                  encoding_type = "ngff:points",
                                  feature_key = NULL, 
                                  instance_key = NULL, 
                                  version = 0.1){
  meta <- list()
  ax <- "axes"
  ct <- "coordinateTransformations"
  sa <- "spatial_attrs"
  
  # axis
  meta[[ax]] <- c("x", "y")
  meta[[ax]] <- if(ncol(x) == 3) c(meta[[ax]], "z") else meta[[ax]]
  
  # encoding type
  meta[["encoding-type"]] <- encoding_type
  
  # spatialdata_attrs
  meta[[sa]] <-  list(version = version)
  if(!is.null(feature_key)) 
    meta[[sa]][["feature_key"]] <- feature_key
  if(!is.null(instance_key)) 
    meta[[sa]][["instance_key"]] <- instance_key
  
  # coordinate transformations
  meta[[ct]] <- .make_empty_ct(meta[[ax]])
  
  # update json list
  meta <- fromJSON(toJSON(meta, auto_unbox = TRUE), simplifyVector = TRUE)
  Zattrs(meta)
}

.make_image_meta <- function(x, 
                                  axes = c("c", "y", "x"),
                                  version = 0.4){
  meta <- list()
  ax <- "axes"
  ct <- "coordinateTransformations"
  ds <- "datasets"
  mt <- "metadata"
  v <- "version"
  n <- "name"
  
  # axis
  meta[[ax]] <- .make_axes_meta(axes, unit = FALSE)
  
  # coordinate transformations
  # TODO: shall we do coordinate transformations only
  # without datasets:coordinateTransformations
  # see https://ngff.openmicroscopy.org/0.4/index.html#multiscale-md
  meta[[ct]] <- .make_empty_ct(axes)
  
  # datasets 
  meta[[ds]] <- .make_datasets(x, axes)
  
  # metadata
  meta[[mt]] <- list(omero = list(
    channels = lapply(seq_len(length(axes))-1, \(.) 
                      list(label = .))
  ))
  
  # name
  meta[[n]] <- ""
  
  # version
  meta[[v]] <-  list(version = version)
  
  # multiscales
  meta <- list(multiscales = list(meta), 
               omero = list(
                 channels = lapply(seq_len(length(axes))-1, \(.) 
                                   list(label = .))
               ), 
               spatialdata_attrs = list(version = "0.1"))
  
  # update json list
  meta <- fromJSON(toJSON(meta, auto_unbox = TRUE), simplifyVector = TRUE)
  Zattrs(meta)
}

.make_label_meta <- function(x, 
                             axes = NULL,
                             version = 0.4){
  meta <- list()
  ax <- "axes"
  ct <- "coordinateTransformations"
  ds <- "datasets"
  v <- "version"
  n <- "name"
  
  # axis
  if(is.null(axes)){
    axes <- c("y", "x")
    axes <- if(length(dim(x)) == 3) c("z", axes) else axes
  }
  meta[[ax]] <- .make_axes_meta(axes, unit = FALSE)
  
  # coordinate transformations
  # TODO: shall we do coordinate transformations only
  # without datasets:coordinateTransformations
  # see https://ngff.openmicroscopy.org/0.4/index.html#multiscale-md
  meta[[ct]] <- .make_empty_ct(axes)
  
  # datasets 
  meta[[ds]] <- .make_datasets(x, axes)
  
  # name
  meta[[n]] <- ""
  
  # version
  meta[[v]] <-  list(version = version)
  
  # multiscales
  meta <- list(`image-label`= list(version = version), 
               multiscales = list(meta), 
               spatialdata_attrs = list(version = "0.1"))
  
  # update json list
  meta <- fromJSON(toJSON(meta, auto_unbox = TRUE), simplifyVector = TRUE)
  Zattrs(meta)
}
