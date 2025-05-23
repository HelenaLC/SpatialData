% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_utils.R
\name{table-utils}
\alias{table-utils}
\alias{hasTable}
\alias{getTable}
\alias{setTable}
\alias{valTable}
\alias{meta,SingleCellExperiment-method}
\alias{hasTable,SpatialData,ANY-method}
\alias{hasTable,SpatialData,character-method}
\alias{getTable,SpatialData,ANY-method}
\alias{getTable,SpatialData,character-method}
\alias{setTable,SpatialData,ANY-method}
\alias{setTable,SpatialData,character-method}
\alias{valTable,SpatialData-method}
\title{\code{SpatialData} annotations}
\usage{
\S4method{meta}{SingleCellExperiment}(x)

\S4method{hasTable}{SpatialData,ANY}(x, i)

\S4method{hasTable}{SpatialData,character}(x, i, name = FALSE)

\S4method{getTable}{SpatialData,ANY}(x, i, drop = TRUE)

\S4method{getTable}{SpatialData,character}(x, i, drop = TRUE)

\S4method{setTable}{SpatialData,ANY}(x, i, ..., name = NULL, rk = "rk", ik = "ik")

\S4method{setTable}{SpatialData,character}(x, i, ..., name = NULL, rk = "rk", ik = "ik")

\S4method{valTable}{SpatialData}(x, i, j, assay = 1, drop = TRUE)
}
\arguments{
\item{x}{\code{\link{SpatialData}} object.}

\item{i}{character string; name of the
element for which to get/set a \code{table}.}

\item{name}{logical; should the \code{table} 
name be returned instead of TRUE/FALSE?}

\item{drop}{logical; should observations (columns) 
that don't belong to \code{i} be filtered out?}

\item{...}{\code{data.frame} or list of data generation function(s) 
that accept an argument for the number of observations; see examples.}

\item{rk, ik}{character string; region and instance key (the latter will be 
ignored if an instance key is already specified within element \code{i}).}

\item{j}{character string; \code{colData} column, 
or row name to retrieve \code{assay} data.}

\item{assay}{character string or scalar integer; 
specifies which \code{assay} to use when \code{j} is a row name.}
}
\description{
\code{SpatialData} annotations
}
\examples{
library(SingleCellExperiment)
x <- file.path("extdata", "blobs.zarr")
x <- system.file(x, package="SpatialData")
x <- readSpatialData(x, anndataR=FALSE)

# check if element has a 'table'
hasTable(x, "blobs_points")
hasTable(x, "blobs_labels")

# retrieve 'table' for element 'i'
sce <- getTable(x, i="blobs_labels")
head(colData(sce))
meta(sce)

# get values from 'table' 
valTable(x, 
  i="blobs_labels", 
  j="channel_0_sum")

# add 'table' annotating an element 'i'
# (w/ or w/o supplying additional data)

# labels
y <- x; tables(y) <- list()
y <- setTable(y, i <- "blobs_labels")
head(colData(sce <- getTable(y, i)))

# points
y <- setTable(x, i <- "blobs_points")
head(colData(sce <- getTable(y, i)))

# labels
y <- setTable(x, i <- "blobs_circles")
head(colData(sce <- getTable(y, i)))

# list of data generating functions
f <- list(
  numbers=\(n) runif(n),
  letters=\(n) sample(letters, n, TRUE))

args <- c(list(x, i <- "blobs_points"), f)
y <- do.call(setTable, args)
head(colData(getTable(y, i)))

# passing a preconstructed 'data.frame'
id <- unique(point(x, i)$instance_id)
df <- data.frame(n=runif(length(id)))

y <- setTable(x, i, df)
head(colData(getTable(y, i)))
}
