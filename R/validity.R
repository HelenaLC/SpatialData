# TODO: everything...
#
.spd_validity <- function(object) {
    msg <- NULL
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

    # if (!is.null(data(shape(object))))
    # {
    #     ns <- length(shape(object))
    #     for (i in c(1:ns)) {
    #         dfi <- data(shape(object, i))
    #             if (!all(c("x", "y") %in% names(dfi))) {
    #             msg <- c(msg, "'x' and 'y' missing in data shape ", i)
    #         }
    #     }
    # }
    # if (nrow(int_elementMetadata(object))!=nrow(object)) {
    #     msg <- c(msg, "'nrow' of 'int_elementMetadata' not equal to 'nrow(object)'")
    # }
    # if (nrow(int_colData(object))!=ncol(object)) {
    #     msg <- c(msg, "'nrow' of 'int_colData' not equal to 'ncol(object)'")
    # }
    if (length(msg)) { return(msg) } # inspired by .sce_validity
    return(TRUE)
}
