% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readTable.R
\name{readTable}
\alias{readTable}
\title{`SingleCellExperiment` from `AnnData`-Zarr}
\usage{
readTable(path)
}
\arguments{
\item{path}{A character string specifying
the path to a `table/` subdirectory.}
}
\value{
\code{SingleCellExperiment}
}
\description{
...
}
\examples{
path <- file.path("extdata", "blobs", "table", "table")
path <- system.file(path, package="SpatialData")
(sce <- readTable(path))

}
\author{
Constantin Ahlmann-Eltze
}
