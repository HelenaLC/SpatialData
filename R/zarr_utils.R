#' create_zarr_group
#'
#' create zarr groups
#' 
#' @param store the location of (zarr) store
#' @param name name of the group
#' @param version zarr version
#' @export
create_zarr_group <- function(store, name, version = "v2"){
  split.name <- strsplit(name, split = "\\/")[[1]]
  if(length(split.name) > 1){
    split.name <- vapply(seq_len(length(split.name)),
                         function(x) paste(split.name[seq_len(x)], collapse = "/"),
                         FUN.VALUE = character(1))
    split.name <- rev(tail(split.name,2))
    if(!dir.exists(file.path(store,split.name[2])))
      create_zarr_group(store = store, name = split.name[2], version = version)
  }
  dir.create(file.path(store, split.name[1]), showWarnings = FALSE)
  switch(version,
         v2 = {
           write("{\"zarr_format\":2}", file = file.path(store, split.name[1], ".zgroup"))},
         v3 = {
           write(
             "{\"zarr_format\":3,\"node_type\":\"group\",\"attributes\":{}}",
             file = file.path(store, split.name[1], "zarr.json"))
         },
         stop("version must be 'v2' or 'v3'")
  )

}

#' create_zarr
#'
#' create zarr store
#'
#' @param name prefix of the zarr store, e.g. <name>.zarr
#' @param dir the location of zarr store, e.g. <dir>/<name>.zarr
#' @param version zarr version
#' 
#' @examples
#' dir.create(td <- tempfile())
#' zarr_name <- "test"
#' create_zarr(dir = td, prefix = "test")
#' dir.exists(file.path(td, "test.zarr"))
#' 
#' @export
create_zarr <- function(name, dir, version = "v2"){
  create_zarr_group(store = dir, name = name, version = version)
}


.replace_zarr <- function(name, path, replace, version = "v2")
{
  zarr.path <- file.path(path,name)
  if (dir.exists(zarr.path) && !replace)
    stop("zarr store with name ", zarr.path ," doesnt exist")
  if (!replace)
    stop("Directory \"", zarr.path, "\" already exists. ",
         "Use 'replace=TRUE' to replace it. ",
         "Its content will be lost!")
  if (unlink(zarr.path, recursive=TRUE) != 0L)
    stop("failed to delete directory \"", dir, "\"")
  create_zarr(name, path, version = version)
  return(zarr.path)
}

.make_zarr_group <- function(x, name, path, replace, version){
  # gd <- file.path(path, "points")
  if(!dir.exists(path)) {
    dir.create(path)
    switch(version,
      v2 = write('{"zarr_format":2}', file = file.path(path, ".zgroup")),
      v3 = write('{"zarr_format":3,"node_type":"group","attributes":{}}',
                 file = file.path(path, "zarr.json"))
    )
  }
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
  create_zarr_group(path, name, version)
  return(ng)
}

