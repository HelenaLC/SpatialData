# get one ----

setGeneric("image", \(x, ...) standardGeneric("image"))
setGeneric("label", \(x, ...) standardGeneric("label"))
setGeneric("shape", \(x, ...) standardGeneric("shape"))
setGeneric("point", \(x, ...) standardGeneric("point"))
setGeneric("table", \(x, ...) standardGeneric("table"))

# get all ----

setGeneric("images", \(x, ...) standardGeneric("images"))
setGeneric("labels", \(x, ...) standardGeneric("labels"))
setGeneric("shapes", \(x, ...) standardGeneric("shapes"))
setGeneric("points", \(x, ...) standardGeneric("points"))
setGeneric("tables", \(x, ...) standardGeneric("tables"))

# get nms ----

setGeneric("imageNames", \(x, ...) standardGeneric("imageNames"))
setGeneric("labelNames", \(x, ...) standardGeneric("labelNames"))
setGeneric("shapeNames", \(x, ...) standardGeneric("shapeNames"))
setGeneric("pointNames", \(x, ...) standardGeneric("pointNames"))
setGeneric("tableNames", \(x, ...) standardGeneric("tableNames"))

setMethod("images", "SpatialData", \(x) x$images)
setMethod("labels", "SpatialData", \(x) x$labels)
setMethod("shapes", "SpatialData", \(x) x$shapes)
setMethod("points", "SpatialData", \(x) x$points)
setMethod("tables", "SpatialData", \(x) x$tables)

setMethod("imageNames", "SpatialData", \(x) names(images(x)))
setMethod("labelNames", "SpatialData", \(x) names(labels(x)))
setMethod("shapeNames", "SpatialData", \(x) names(shapes(x)))
setMethod("pointNames", "SpatialData", \(x) names(points(x)))
setMethod("tableNames", "SpatialData", \(x) names(tables(x)))

# set one -----

setGeneric("image<-", \(x, i, ..., value) standardGeneric("image<-"))
setGeneric("shape<-", \(x, i, ..., value) standardGeneric("shape<-"))
setGeneric("label<-", \(x, i, ..., value) standardGeneric("label<-"))
setGeneric("point<-", \(x, i, ..., value) standardGeneric("point<-"))
setGeneric("table<-", \(x, i, ..., value) standardGeneric("table<-"))

# set all -----

setGeneric("images<-", \(x, value) standardGeneric("images<-"))
setGeneric("labels<-", \(x, value) standardGeneric("labels<-"))
setGeneric("shapes<-", \(x, value) standardGeneric("shapes<-"))
setGeneric("points<-", \(x, value) standardGeneric("points<-"))
setGeneric("tables<-", \(x, value) standardGeneric("tables<-"))

# trs ----

setGeneric("axes", \(x, ...) standardGeneric("axes"))
setGeneric("CTdata", \(x, ...) standardGeneric("CTdata"))
setGeneric("CTname", \(x, ...) standardGeneric("CTname"))
setGeneric("CTtype", \(x, ...) standardGeneric("CTtype"))

setGeneric("CTpath", \(x, ...) standardGeneric("CTpath"))
setGeneric("CTgraph", \(x, ...) standardGeneric("CTgraph"))

setGeneric("rmvCT", \(x, ...) standardGeneric("rmvCT"))
setGeneric("addCT", \(x, ...) standardGeneric("addCT"))

setGeneric("scale", \(x, t, ...) standardGeneric("scale"))
setGeneric("rotate", \(x, t, ...) standardGeneric("rotate"))
setGeneric("transform", \(x, ...) standardGeneric("transform"))
setGeneric("translation", \(x, t, ...) standardGeneric("translation"))

# uts ----

setGeneric("layer", \(x, i, ...) standardGeneric("layer"))
setGeneric("element", \(x, i, j, ...) standardGeneric("element"))

setGeneric("data", \(x, ...) standardGeneric("data"))
setGeneric("meta", \(x, ...) standardGeneric("meta"))

setGeneric("query", \(x, ...) standardGeneric("query"))
setGeneric("mask", \(x, i, j, ...) standardGeneric("mask"))

setGeneric("channels", \(x, ...) standardGeneric("channels"))

# tbl ----

setGeneric("hasTable", \(x, i, ...) standardGeneric("hasTable"))
setGeneric("getTable", \(x, i, ...) standardGeneric("getTable"))
setGeneric("setTable", \(x, i, ...) standardGeneric("setTable"))
setGeneric("valTable", \(x, i, ...) standardGeneric("valTable"))
