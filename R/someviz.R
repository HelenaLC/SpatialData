# shape rendering support
# could transition to S4 if needed

.shapenames = function(sdobj) {
 stopifnot(is(sdobj, "SpatialData"))
 names(shapes(sdobj))
}

.shapes2sf = function(sdobj, elem) {
 stopifnot(elem %in% .shapenames(sdobj))
 sf::st_as_sf(SpatialData::data(SpatialData::shapes(sdobj)[[elem]]))
}

.pointnames = function(sdobj) {
 stopifnot(is(sdobj, "SpatialData"))
 names(points(sdobj))
}

.txdf = function(sdobj) {
 stopifnot("transcripts" %in% .pointnames(sdobj))
 as.data.frame(data(points(sdobj)$transcripts))
}

.pointdf = function(sdobj, elem) {
 stopifnot(elem %in% .pointnames(sdobj))
 as.data.frame(data(points(sdobj)[[elem]]))
}

.available_transcripts = function(sdobj) {   # maybe too specific?  'points'?
 txdf = .txdf(sdobj)
 as.character(unique(txdf$feature_name)) # valid?  feature_name comes back as *factor*
}
 
#' Use geom_sf to view a shapes component
#' @param sdobj SpatialData instance
#' @param elem character(1) name of a shapes component of sdobj
#' @export
viewShape = function(sdobj, elem) {
 thesf = .shapes2sf(sdobj, elem)
 ggplot2::ggplot(thesf) + geom_sf()
}

#' Use geom_point to enhance a visualization with viewShape
#' @param sdobj SpatialData instance
#' @param featurename character(1) name of a shapes component of sdobj
#' @param size numeric(1) target size for glyph
#' @examples
#' example(use_sdio) # produces br2fov
#' viewShape(br2fov, "cell_boundaries") + add_tx(sdobj, "EPCAM")
#' @export
add_tx = function(sdobj, featurename, size=.1) {
 txdf = .txdf(sdobj) |> dplyr::filter(feature_name == featurename)
 ggplot2::geom_point(data=txdf, aes(x=x, y=y), size=size)
}

#' Use geom_point more generally than add_tx
#' @export
add_points = function(sdobj, featurename, size=.1) {
 pointdf = .pointdf(sdobj) 
 ggplot2::geom_point(data=pointdf, aes(x=x, y=y), size=size)
}
