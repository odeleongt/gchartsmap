% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gcharts_us_areas.R
\name{gchart_get_us_areas}
\alias{gchart_get_us_areas}
\title{Get Google Charts data for US areas}
\usage{
gchart_get_us_areas(
  areas,
  server = "https://www.gstatic.com/charts/geochart/10/mapfiles/",
  cache = gchart_get_cache_path(),
  limit = 1000
)
}
\arguments{
\item{areas}{Area codes to get. Should be integers.}

\item{server}{Google geochart server to access.}

\item{cache}{Path to store downloaded data.}

\item{limit}{Maximum number of areas to look for}
}
\description{
Access the Google Charts geochart data for US areas
}
\details{
The function invisibly returns the file path for successful requests or
the response status code for failed requests, in a character vector with the
area name for each element.
}
