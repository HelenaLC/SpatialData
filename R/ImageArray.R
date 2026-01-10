#' @name ImageArray
#' @title The `ImageArray` class
#' 
#' @param x \code{ImageArray}
#' @param data list of \code{\link[Rarr]{ZarrArray}}s
#' @param meta \code{\link{Zattrs}}
#' @param metadata optional list of arbitrary 
#'   content describing the overall object.
#' @param multiscale if TRUE (and \code{data} is not a list), 
#' multiscale image will be generated.
#' @param axes axes
#' @param i,j indices specifying elements to extract.
#' @param k scalar index specifying which scale to extract.
#' @param drop ignored.
#' @param ... option arguments passed to and from other methods.
#'
#' @return \code{ImageArray}
#'
#' @examples
#' library(SpatialData.data)
#' dir.create(td <- tempfile())
#' pa <- SpatialData.data:::.unzip_merfish_demo(td)
#' pa <- file.path(pa, "images", "rasterized")
#' (ia <- readImage(pa))
#'
#' @importFrom S4Vectors metadata<-
#' @importFrom methods new
#' @importFrom DelayedArray DelayedArray
#' @export
ImageArray <- function(data=list(), meta=Zattrs(), metadata=list(), 
                       multiscale=FALSE, axes = NULL, ...) {
    if(!is.list(data)){
      if(multiscale){
        data <- .generate_multiscale_image(data, axes = axes)
      } else {
        data <- list(DelayedArray::DelayedArray(data))
      }
    }
    if(length(meta) < 1){
      meta <- .make_labelimage_meta(data, 
                                    version = 0.1, 
                                    ...)
    } 
    x <- .ImageArray(data=data, meta=meta, ...)
    metadata(x) <- metadata
    return(x)
}

#' @rdname ImageArray
#' @aliases channels
#' @export
setMethod("channels", "ImageArray", \(x, ...) meta(x)$omero$channels$label)

#' @rdname ImageArray
#' @export
setMethod("channels", "ANY", \(x, ...) stop("only 'images' have channels"))

#' @importFrom S4Vectors isSequence
.get_multiscales_dataset_paths <- function(md) {
  
    # validate multiscales attributes
    .validate_multiscales_dataset_path(md)
  
    # get paths
    paths <- md$multiscales$datasets[[1]]$path
    paths <- suppressWarnings({as.numeric(sort(paths, decreasing=FALSE))})
  
    # TODO: how to check if a vector of values here are integers
    # check paths and return
    # if(all(paths %% 0 == 0)){
    #   if(S4Vectors::isSequence(paths))
    #     return(paths) 
    # }
    return(paths)
  
    # stop if not a sequence of integers
    stop("ImageArray paths are ill-defined, should be e.g. 0,1,2, ..., n")
}

#' @noRd
.validate_multiscales_dataset_path <- function(md) {
    # validate 'multiscales' 
    if ("multiscales" %in% names(md)) {
        ms <- md[["multiscales"]]
    
        # validate 'datasets' 
        if("datasets" %in% names(ms)) {
          ds <- ms[["datasets"]]
          
          # validate 'paths'
          valid <- vapply(ds, \(ds) "path" %in% colnames(ds), logical(1))
          
          if (!all(valid)) {
            stop("'ImageArray' paths are ill-defined,",
                " no 'path' attribute under 'multiscale-datasets'")
          } 
          
        } else {
            stop("'ImageArray' paths are ill-defined,",
                " no 'datasets' attribute under 'multiscale'")
        }
    } else {
        stop("'ImageArray' paths are ill-defined,",
            " no 'multiscales' attribute under '.zattrs'")
    }
}

.check_jk <- \(x, .) {
    if (isTRUE(x)) return()
    tryCatch(
        stopifnot(
            is.numeric(x), x == round(x),
            diff(range(x)) == length(x)-1,
            (y <- abs(x)) == seq(min(y), max(y))
        ),
        error=\(e) stop(sprintf("invalid '%s'", .))
    )
}

#' @rdname ImageArray
#' @importFrom utils head tail
#' @exportMethod [
setMethod("[", "ImageArray", \(x, i, j, k, ..., drop=FALSE) {
    if (missing(i)) i <- TRUE
    if (missing(j)) j <- TRUE else if (isFALSE(j)) j <- 0 else .check_jk(j, "j")
    if (missing(k)) k <- TRUE else if (isFALSE(k)) k <- 0 else .check_jk(k, "k")
    ijk <- list(i, j, k)
    n <- length(data(x, NULL))
    d <- dim(data(x))
    x@data <- lapply(seq_len(n), \(.) {
        j <- if (isTRUE(j)) seq_len(d[2]) else j
        k <- if (isTRUE(k)) seq_len(d[3]) else k
        jk <- lapply(list(j, k), \(jk) {
            fac <- 2^(.-1)
            seq(floor(head(jk, 1)/fac), 
                ceiling(tail(jk, 1)/fac))
        })
        data(x, .)[i, jk[[1]], jk[[2]], drop=FALSE]
    })
    x
})

#' .create_mip
#' 
#' Generate a downsampled pyramid of images.
#'
#' @importFrom EBImage resize
#' 
#' @inheritParams write_image
#' 
#' @noRd
.generate_multiscale_image <- function(image,
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