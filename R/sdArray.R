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
#' 
#' @inheritParams write_image
#' 
#' @noRd
.generate_multiscale <- function(image,
                                 scalefactor = 2,
                                 axes, 
                                 max_layer = 5){
  
  # check dim
  ndim <- length(dim(image))
  if (ndim > 3) {
    stop("Only images of 5D or less are supported")
  }
  
  # validate axes
  axes <- .get_valid_axes(ndim = length(dim(image)), 
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
                              h = dim_image[2]), 
              perm = rev(seq_len(length(axes))))
    }
  }
  image_list
}

#' .get_valid_axes
#' 
#' Get validated axes
#'
#' @inheritParams write_image
#' 
#' @noRd
.get_valid_axes <- function(
    ndim = NULL,
    axes = NULL
) {
  
  # We can guess axes for 2D and 5D data
  if (is.null(axes)) {
    if (!is.null(ndim) && ndim == 2) {
      axes <- c("y", "x")
      message(sprintf("Auto using axes %s for 2D data", 
                      paste(axes, collapse = ", ")))
    } else {
      stop("axes must be provided. Can't be guessed for 3D or 4D data", 
           call. = FALSE)
    }
  }
  
  # axes may be string e.g. "tczyx"
  if (is.character(axes) && length(axes) == 1L) 
    axes <- strsplit(axes, "", fixed = TRUE)[[1]]
  
  if (!is.null(ndim) && length(axes) != ndim) {
    stop(
      sprintf("axes length (%d) must match number of dimensions (%d)", 
              length(axes), ndim),
      call. = FALSE
    )
  }
  
  axes
}