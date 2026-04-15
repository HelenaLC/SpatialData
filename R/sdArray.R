#' @name sdArray
#' @title Methods for `ImageArray` and `LabelArray` class
#' 
#' @aliases 
#' data_type
#' data,ImageArray-method
#' data,LabelArray-method
#' dim,ImageArray-method
#' dim,LabelArray-method
#' length,ImageArray-method
#' length,LabelArray-method
#' 
#' @param x \code{ImageArray} or  \code{LabelArray}
#' @param k scalar index specifying which scale to extract.
#' 
#' @return \code{ImageArray}
#'
#' @examples
#' library(SpatialData.data)
#' zs <- get_demo_SDdata("merfish")
#' 
#' # helper that gets path to first element in layer 'l' 
#' fn <- \(l) list.files(file.path(zs, l), full.names=TRUE)[1]
#'   
#' # read individual element  
#' (ia <- readImage(fn("images")))
#' 
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
NULL

#' @rdname sdArray
#' @export
setMethod("data", "sdArray", \(x, k=1) {
  if (is.null(k)) return(x@data)
  stopifnot(length(k) == 1, is.numeric(k), k > 0)
  n <- length(x@data) # get number of available scales
  if (is.infinite(k)) k <- n # input of Inf uses lowest
  if (k <= n) return(x@data[[k]]) # return specified scale
  stop("'k=", k, "' but only ", n, " resolution(s) available")
})

#' @rdname sdArray
#' @export
setMethod("dim", "sdArray", \(x) dim(data(x)))

#' @rdname sdArray
#' @export
setMethod("length", "sdArray", \(x) length(data(x, NULL)))

#' @export
#' @rdname sdArray
#' @importFrom S4Vectors metadata
setMethod("data_type", "sdArray", \(x) {
    if (is(y <- data(x), "DelayedArray")) 
        data_type(y) else metadata(x)$data_type
})

#' @export
#' @rdname sdArray
#' @importFrom DelayedArray DelayedArray
#' @importFrom Rarr zarr_overview
#' @importFrom ZarrArray path
setMethod("data_type", "DelayedArray", \(x) zarr_overview(path(x), as_data_frame=TRUE)$data_type)


#' .create_mip
#' 
#' Generate a downsampled pyramid of images.
#'
#' @importFrom EBImage resize
#' @importFrom stats setNames
#' 
#' @inheritParams write_image
#' 
#' @noRd
.generate_multiscale <- function(image,
                                 scalefactor = 2,
                                 axes, 
                                 max_layer = 5, 
                                 method = "image"){
  
  # check dim
  ndim <- length(dim(image))
  if (ndim > 3) {
    stop("Only images of 5D or less are supported")
  }
  
  # validate axes
  axes <- .get_valid_axes(image, 
                          axes = axes)
  
  # get x y dimensions for EBImage
  dim_image <- stats::setNames(dim(image), axes)
  dim_image <- dim_image[c("x", "y")]
  
  # downscale image
  image_list <- list(image)
  if (max_layer > 1) {
    cur_image <- aperm(image, 
                       perm = rev(seq_len(length(axes))))
    for (i in 2:max_layer) {
      dim_image <- ceiling(dim_image / scalefactor)
      image_list[[i]] <- 
        aperm(EBImage::resize(cur_image,
                              w = dim_image[1],
                              h = dim_image[2],
                              filter = switch(method, 
                                              image = "bilinear",
                                              label = "none")), 
              perm = rev(seq_len(length(axes))))
    }
  }
  if (method == "label") {
    image_list <- lapply(image_list, function(x) {
      storage.mode(x) <- "integer"
      x
    })
  }
  image_list
}