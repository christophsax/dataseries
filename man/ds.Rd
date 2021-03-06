% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.R
\name{ds}
\alias{ds}
\alias{cache_ls}
\alias{cache_rm}
\title{Download Time Series from \href{http://www.dataseries.org}{www.dataseries.org}}
\usage{
ds(id, class = c("data.frame", "ts", "xts"))

cache_ls()

cache_rm()
}
\arguments{
\item{id}{one ore more IDs, as given by
\href{http://www.dataseries.org}{www.dataseries.org}.}

\item{class}{class of the return value, either a \code{"data.frame"} (default) or
an \code{"xts"} object (requires the \code{xts} package to be installed).}
}
\description{
\code{ds} downloads time series from
\href{http://www.dataseries.org}{www.dataseries.org}. Data can be imported as a
\code{data.frame} or an \code{xts} object.
}
\details{
Downloaded series are \strong{cached in memory} as long as the
R session is open. If you want to force an update, either restart the R
session or run \code{cache_rm} to emtpy the cache. \code{cache_ls} is a utility
function that lists all cached objects (see examples).
}
\examples{
\dontrun{
ds(c("CCI.AIK", "CCI.ASSS"))               # data.frame
ds(c("CCI.AIK", "CCI.ASSS"), "ts")         # "ts" object
ds(c("CCI.AIK", "CCI.ASSS"), "xts")        # "xts" object

# list cached objects
cache_ls()

# empty in-memory cache, which forces a fresh download
cache_rm()
}

}
