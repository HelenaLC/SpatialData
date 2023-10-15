#' @name Xenium
#' @title Reading 10x Xenium data
#' @description ...
#' 
#' @param path Character string specifying the dataset location;
#'   should point to the (unzipped) \code{_outs/} directors.
#' @param ... Ignored.
#' 
#' @return \code{SpatialData}
#' 
#' @examples
#' NULL
#' 
#' @author Helena L. Crowell
#' 
#' @importFrom SingleCellExperiment altExp<-
#' @importFrom DropletUtils read10xCounts
#' @importFrom S4Vectors DataFrame
#' @importFrom dplyr mutate rename
#' @importFrom arrow read_parquet
#' @export
readXenium <- \(path, ...) {

    za <- Zattrs(list(coordinateTransformations=newCoord("global")))
    
    # names(i) <- i <- paste0("morphology_", c("morphology", "mip"))
    # names(l) <- l <- paste0(c(""))
    
    names(s) <- s <- paste0(c("cell", "nucleus"), "_boundaries")
    shapes <- lapply(s, \(.) {
        fd <- arrow::read_parquet(
            file=paste0(., ".parquet"), 
            as_data_frame=TRUE)
        xy <- c("vertex_x", "vertex_y")
        cs <- unname(split(seq(nrow(fd)), fd$cell_id))
        df <- DataFrame(index=unlist(cs), type="polygon") 
        df$data <- lapply(cs, \(.) unname(as.matrix(fd[., xy])))
        ShapeFrame(data=df[, c("data", "index", "type")], zattrs=za)
    })
    
    names(p) <- p <- "transcripts"
    points <- lapply(p, \(.) {
        fd <- arrow::read_parquet(
            file=paste0(., ".parquet"), 
            as_data_frame=FALSE)
        fd <- fd |>
            dplyr::mutate(`__null_dask_index__`=seq(nrow(fd))) |>
            dplyr::rename(x=x_location, y=y_location, z=z_location)
        PointFrame(data=fd, zattrs=za)
    })
    
    sce <- DropletUtils::read10xCounts("cell_feature_matrix.h5")
    for (. in c("BLANK", "NegControlProbe", "NegControlCodeword")) {
        idx <- grep(., rownames(sce))
        altExp(sce, .) <- sce[idx, ]
        sce <- sce[-idx, ]
    }
    tables <- list(cells=sce)
    
    SpatialData(
        shapes=shapes,
        points=points,
        tables=tables)
}

# spd <- readXenium(".")
# 
# plotPoint(spd, size=0.1,
#     feature_name == "VWF", 
#     col="overlaps_nucleus")
