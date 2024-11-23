# TODO: temporary path getter for Rarr::ZarrArray
#' @importFrom methods slotNames
#' @noRd
getZarrArrayPath <- \(a){
  if("zarr_array_path" %in% methods::slotNames(a))
    return(a@zarr_array_path)
  return(getZarrArrayPath(a@seed))
}