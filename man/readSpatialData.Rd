% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{readSpatialData}
\alias{readSpatialData}
\title{Read `SpatialData` OME-Zarr}
\usage{
readSpatialData(path, ...)
}
\arguments{
\item{path}{A character string specifying the path to an
OME-Zarr file adhering to \code{SpatialData} specification.}

\item{...}{Further arguments to be passed to or from other methods.}
}
\value{
\code{\link{SpatialData}}
}
\description{
...
}
\examples{
dir <- file.path("extdata", "blobs")
dir <- system.file(dir, package="SpatialData")
(spd <- readSpatialData(dir))

}
\author{
Constantin Ahlmann-Eltze, Helena L. Crowell
}
