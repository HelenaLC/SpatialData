.createPointmeta <- function(x, 
                             encoding_type = "ngff:points",
                             feature_key = NULL, 
                             instance_key = NULL, 
                             version = 0.1){
  meta <- list()
  ax <- "axes"
  ct <- "coordinateTransformations"
  # axis
  meta[[ax]] <- c("x", "y")
  meta[[ax]] <- if(ncol(x) == 3) c(meta[[ax]], "z") else meta[[ax]]
  # encoding type
  meta[["encoding-type"]] <- encoding_type
  # spatialdata_attrs
  sa <-  list(version = version)
  if(!is.null(feature_key)) 
    sa[["feature_key"]] <- feature_key
  if(!is.null(instance_key)) 
    sa[["instance_key"]] <- instance_key
  # coordinate transformations
  meta[[ct]] <- .make_ct()
  meta <- Zattrs(meta)
  meta <- addCT(meta, name = "test_ct")
  
  # return
  meta
}