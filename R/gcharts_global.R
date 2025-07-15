#' Generate Google Charts spatial data for all countries
#' @description
#' This function queries 'Google Charts' resources to identify countries for
#' which geographical data is available, and generates the simple features for
#' local use.
#' @param countries Which countries to process. Defaults to "all".
#' Use country codes from \code{gchartsmap::gchart_countries()}
#' @param server Google geochart server to access.
#' @param cache Path to store downloaded data.
#'
#' @returns Returns a simple features `data.frame` with class `sf`, representing
#' the spatial data for all countries from the Google Charts servers,
#' using the WGS84 (epsg = 4326) coordinate reference system.
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
#' # GET and process all countries
#' gchartsmap::gchart_generate_countries(countries = "GT")
#'
#' # clean up
#' list.files(
#'   tempdir(), all.files = TRUE, full.names = TRUE, recursive = TRUE
#' )
#'
#' @importFrom utils download.file
#' @export
gchart_generate_countries <- function(
    countries = "all",
    server = "https://www.gstatic.com/charts/geochart/10/mapfiles/",
    cache = gchart_get_cache_path()
){

  if(identical(countries, "all")){
    country_codes <- gchart_countries()
    countries <- country_codes$code
  } else {
    country_codes <- data.frame(code = countries)
  }

  gchart_get_countries(
    countries = countries,
    server = server,
    cache = cache
  )

  country_geo_data <- gchart_process_areas(
    areas = countries,
    type = "countries"
  )

  return(country_geo_data)
}





gchart_get_countries <- function(
    countries = "all",
    server = "https://www.gstatic.com/charts/geochart/10/mapfiles/",
    cache = gchart_get_cache_path()
){
  if(identical(countries, "all")){
    country_codes <- gchart_countries()
  } else {
    country_codes <- data.frame(code = countries)
  }


  different_file <- c(
    "AI", "AQ", "AS", "AQ", "AW", "AX", "BM", "BV", "IO", "VG",
    "KY", "CX", "CC", "CK", "CW", "FK", "FO", "GF", "PF", "TF", "GI",
    "GP", "GU", "GG", "HM", "HK", "IM", "JE", "KE", "MO", "MT", "MQ",
    "YT", "MC", "MS", "NC", "NU", "NF", "MP", "PS", "PN", "PR", "RE",
    "MF", "SG", "SX", "GS", "BL", "PM", "SJ", "TW", "TK", "TC", "VI",
    "UM", "VA", "WF", "EH"
  )



  # default file name for most countries in the server
  country_codes$file <- paste0(country_codes$code, "_PROVINCES.js")

  # different file name for some countries
  country_codes$file[country_codes$code %in% different_file] <-
    paste0(
      country_codes$code[country_codes$code %in% different_file],
      "_COUNTRIES.js"
    )

  mapply(
    country_codes$code, country_codes$file,
    FUN = function(code, file){
      local_file <- paste0(cache, "/countries/", code, ".js")
      if(!file.exists(local_file)){
        tryCatch(
          download.file(
            url = paste0(server, file),
            destfile = local_file
          ),
          error = function(e) "Not found"
        )
      }
    }
  )

  country_codes$found <- country_codes$code %in% gsub(
    "^([^.]+).+",
    "\\1",
    list.files("data/countries")
  )

  return(country_codes)
}
