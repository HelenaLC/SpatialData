#' @importFrom Rarr write_zarr_group

#' @noRd
.replace_zarr <- function(zarr.path, replace, version = 2)
{
  if (dir.exists(zarr.path) && !replace)
    stop("zarr store with name ", zarr.path ," doesnt exist")
  if (!replace)
    stop("Directory \"", zarr.path, "\" already exists. ",
         "Use 'replace=TRUE' to replace it. ",
         "Its content will be lost!")
  if (unlink(zarr.path, recursive=TRUE) != 0L)
    stop("failed to delete directory \"", dir, "\"")
  write_zarr_group(zarr.path, group = "", zarr_version = version)
  return(zarr.path)
}

#' @noRd
.make_zarr_group <- function(x, name, path, replace, version){
  
  # create element parent dir
  if(!dir.exists(path))
    dir.create(path)
  
  # check element dir
  ng <- file.path(path, name)
  if(replace){
    unlink(ng, recursive = TRUE)
  } else {
    nms <- list.dirs(file.path(path), full.names = FALSE)
    if(name %in% nms)
      stop("Directory \"", ng, "\" already exists. ",
           "Use 'replace=TRUE' to replace it. ",
           "Its content will be lost!")
  }
  
  # create group
  write_zarr_group(path, name, zarr_version = version)
  
  return(ng)
}