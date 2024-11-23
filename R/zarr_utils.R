# TODO: temporary path getter for Rarr::ZarrArray

#' getZarrArrayPath
#' 
#' returns the path of a zarr store associated with ZarrArray
#' 
#' @param a ZarrArray
#' 
#' @importFrom methods slotNames
#' @export
getZarrArrayPath <- \(a){
  if("zarr_array_path" %in% methods::slotNames(a))
    return(a@zarr_array_path)
  return(getZarrArrayPath(a@seed))
}