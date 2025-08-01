% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{melt}
\alias{melt}
\title{Replacement generic for \code{\link[data.table:melt.data.table]{data.table::melt()}}}
\usage{
melt(data, ...)
}
\arguments{
\item{data}{an object to melt}

\item{...}{arguments passed to other methods}
}
\value{
An unkeyed \code{data.table} containing the molten data.
}
\description{
As of the CRAN release of the 1.14.8 version of \strong{data.table}, the
\code{\link[data.table:melt.data.table]{data.table::melt()}} function is not a generic. This function aims to
temporary provide a generic to this function, so that \code{\link[=melt.sentopicsmodel]{melt.sentopicsmodel()}}
can be effectively dispatched when used. Expect this function to disappear
shortly after the release of \strong{data.table} 1.14.9.
}
\seealso{
\code{\link[data.table:melt.data.table]{data.table::melt()}}, \code{\link[=melt.sentopicsmodel]{melt.sentopicsmodel()}}
}
