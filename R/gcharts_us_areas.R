#' Generate Google Charts spatial data for US areas
#' @description
#' This function queries 'Google Charts' resources to identify the US geographic
#' areas used in services like Google Trends, and uses geographic data from the
#' US Census Bureau to provide those areas with subdivisions at the county level.
#' @param areas Area codes to get. Should be integers.
#' @param limit Maximum number of areas to look for.
#'
#' @returns Returns a simple features `data.frame` with class `sf`, representing
#' the spatial data for all areas with a valid id between 1 and a 1000 from the
#' Google Charts servers, using the WGS84 (epsg = 4326) coordinate reference
#' system.
#' You need to first run `gchart_set_cache()` so the package knows where to
#' store the downloaded data.
#'
#' @examples
#'
#' library(package = "gchartsmap")
#'
#' # set the cache path to your system's cache path
#' gchartsmap::gchart_set_cache(path = tempdir())
#'
#' # GET and process area 500
#' gchartsmap::gchart_generate_us_areas(500L)
#'
#' # clean up
#' list.files(
#'   tempdir(), all.files = TRUE, full.names = TRUE, recursive = TRUE
#' )
#'
#' @export
gchart_generate_us_areas <- function(
    areas = 1:1000L,
    limit = 1000
){
  # areas should be integers
  if(!is.integer(areas)) stop("Provide area codes as integers.")

  # at least one area
  if(length(areas) < 1) stop("Provide at least one area code.")

  # Caution if too many areas are requested
  if(length(areas) > limit){
    stop("Can't fetch more than ", limit, " areas!")
  }

  gchart_get_us_areas(areas)
  gchart_us_areas <- gchart_available_areas(type = "us-areas") |>
    names() |>
    as.integer() |>
    intersect(areas) |>
    gchart_process_areas(type = "us-areas")

  return(gchart_us_areas)
}


#' Get Google Charts data for US areas
#' @description
#' Access the Google Charts geochart data for US areas
#'
#' @details
#' The function invisibly returns the file path for successful requests or
#' the response status code for failed requests, in a character vector with the
#' area name for each element.
#'
#' @param areas Area codes to get. Should be integers.
#' @param server Google geochart server to access.
#' @param cache Path to store downloaded data.
#' @param limit Maximum number of areas to look for
#' @importFrom httr GET
#' @importFrom httr content
gchart_get_us_areas <- function(
    areas,
    server = "https://www.gstatic.com/charts/geochart/10/mapfiles/",
    cache = gchart_get_cache_path(),
    limit = 1000
){
  # needs to use cache
  if(cache == "") stop("Cache should be used. See `gchart_set_cache`.")

  # areas should be integers
  if(!is.integer(areas)) stop("Provide area codes as integers.")

  # at least one area
  if(length(areas) < 1) stop("Provide at least one area code.")

  # Caution if too many areas are requested
  if(length(areas) > limit){
    stop("Can't fetch more than ", limit, " areas!")
  }


  # use the folder for US area files
  area_path <- file.path(cache, "us-areas")
  dir.create(area_path, recursive = TRUE, showWarnings = FALSE)

  # track request results
  areas <- as.character(areas)
  results <- character(length(areas))
  names(results) <- areas

  # download area files
  sapply(
    areas,
    function(area){
      url <- paste0(server, "US-", area, "_METROS.js")
      file <- file.path(area_path, paste0(area, ".js"))
      if(file.exists(file)){
        results[area] <<- file
      } else {
        response <- httr::GET(url)

        if(response$status_code == 200){
          content <- response |>
            httr::content(as = "text", encoding = "UTF-8") |>
            cat(file = file)

          results[area] <<- file
        } else {
          cat("error:", response$status_code, file = file)
          results[area] <<- response$status_code
        }
      }
    }
  )

  return(invisible(results))
}
