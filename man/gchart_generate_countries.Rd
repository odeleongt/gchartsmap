% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gcharts_global_areas.R
\name{gchart_generate_countries}
\alias{gchart_generate_countries}
\title{Generate Google Charts spatial data for all countries}
\usage{
gchart_generate_countries(
  countries = "all",
  server = "https://www.gstatic.com/charts/geochart/10/mapfiles/",
  cache = gchart_get_cache_path(),
  verbose = TRUE
)
}
\arguments{
\item{countries}{Which countries to process. Defaults to "all".
Use country codes from \code{gchartsmap::gchart_countries()}}

\item{server}{Google geochart server to access.}

\item{cache}{Path to store downloaded data.}

\item{verbose}{Whether to show messages during processing.}
}
\value{
Returns a simple features `data.frame` with class `sf`, representing
the spatial data for all countries from the Google Charts servers,
using the WGS84 (epsg = 4326) coordinate reference system.
You need to first run `gchart_set_cache()` so the package knows where to
store the downloaded data.
}
\description{
This function queries 'Google Charts' resources to identify countries for
which geographical data is available, and generates the simple features for
local use.
}
\examples{

library(package = "gchartsmap")

# set the cache path to your system's cache path
gchartsmap::gchart_set_cache(path = tempdir())

# GET and process all countries
gchartsmap::gchart_generate_countries(countries = "GT")

# clean up
list.files(
  tempdir(), all.files = TRUE, full.names = TRUE, recursive = TRUE
)

}
