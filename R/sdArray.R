#' @name Array-methods
#' @title Methods for `ImageArray` and `LabelArray` class
#' 
#' @aliases 
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
#' # TODO
#' 
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
NULL

#' @rdname Array-methods
#' @export
setMethod("data", "sdArray", \(x, k=1) {
  if (is.null(k)) return(x@data)
  stopifnot(length(k) == 1, is.numeric(k), k > 0)
  n <- length(x@data) # get number of available scales
  if (is.infinite(k)) k <- n # input of Inf uses lowest
  if (k <= n) return(x@data[[k]]) # return specified scale
  stop("'k=", k, "' but only ", n, " resolution(s) available")
})

#' @rdname Array-methods
#' @export
setMethod("dim", "sdArray", \(x) dim(data(x)))

#' @rdname Array-methods
#' @export
setMethod("length", "sdArray", \(x) length(data(x, NULL)))

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
  image_list
}