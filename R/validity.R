# TODO: everything...
#
.spd_validity <- function(object) { #To Be Discussed
    msg <- NULL
    # Checks if the points have the x,y coordinates, as they are hard-coded
    # in the plot functions
    if (!is.null(data(point(object))))
    {
        np <- length(points(object))
        for (i in c(1:np)) {
            dfi <- data(point(object, i))
            if (!all(c("x", "y") %in% names(dfi))) {
                msg <- c(msg, paste0("'x' and 'y' missing in data point ", i))
            }
        }
    }
    # Checks for region_key and instance_key in the SCE metadatas
    # cause they are hard-coded in the plot functions
    nt <- length(tables(object))
    for (i in c(1:nt)) {
        sce <- table(object, i)
        if (!is("SingleCellExperiment", sce)) {
            msg <- c(msg, paste0("Table ", i, " is not a SingleCellExperiment"))
        }
        md <- SingleCellExperiment::metadata(se)[[1]]
        if (!all(c("region_key", "instance_key") %in% names(md))) {
            msg <- c(msg, paste0("region_key/instance_key not present in ",
                i, "-th sce metadata"))
        }
    }
    # If present, it checks if the labels and the images have the same
    # dimensions, it doesn't check otherwise
    if (length(images(object))) {
        if (length(labels(object))) {
            for (i in c(1:length(images))) {
                dimsi <- dim(image(object, i))[c(2,3)]
                dimsl <- dim(label(object, i))
                if (!all(dimsi==dimsl)) {
                    msg <- c(msg, paste0("Label ", i, "-th not the same dim as ",
                        i,"-th Image"))
                }
            }
        }
    }
    if (length(msg)) { return(msg) } # inspired by .sce_validity
    return(TRUE)
}

#' @importFrom methods is
.validateSpatialData <- \(x) {
    typ <- c(
        images="ImageArray", 
        labels="LabelArray", 
        points="PointFrame", 
        shapes="ShapeFrame", 
        tables="SingleCellExperiment")
    msg <- NULL
    for (. in names(typ)) if (length(x[[.]])) 
        if (!all(vapply(x[[.]], \(y) is(y, typ[.]), logical(1))))
            msg <- c(msg, sprintf("'%s' should be a list of '%s'", ., typ[.]))
    if (length(msg))
        return(msg)
    return(TRUE)
}

#' @importFrom S4Vectors setValidity2
setValidity2("SpatialData", .validateSpatialData)
