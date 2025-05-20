
.create_trans_metadata <- function(x){
}

.create_point_metadata <- function(x, 
                                   encoding_type = "ngff:points",
                                   feature_key = NULL, 
                                   instance_key = NULL, 
                                   version = 0.1){
  meta <- list()
  # axis
  meta[["axis"]] <- c("x", "y")
  meta[["axis"]] <- if(ncol(x) == 3) c(meta[["axis"]], "z")
  # encoding type
  meta[["encoding-type"]] <- encoding_type
  # spatialdata_attrs
  sa <-  list(version = version)
  if(!is.null(feature_key)) sa[["feature_key"]] <- feature_key
  if(!is.null(instance_key)) sa[["instance_key"]] <- instance_key
  # coordinate transformations
  meta[["coordinateTransformations"]] <-
    .create_trans_metadata(x, meta)
  # return
  meta
}