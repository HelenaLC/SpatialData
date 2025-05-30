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
  meta[[ct]] <- .make_empty_ct()
  
  # update json list
  meta <- fromJSON(toJSON(meta, auto_unbox = TRUE), simplifyVector = TRUE)
  Zattrs(meta)
}