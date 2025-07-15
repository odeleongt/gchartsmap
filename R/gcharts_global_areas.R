#' Generate Google Charts spatial data for all countries
#' @description
#' This function queries 'Google Charts' resources to identify countries for
#' which geographical data is available, and generates the simple features for
#' local use.
#' @param countries Which countries to process. Defaults to "all".
#' Use country codes from \code{gchartsmap::gchart_countries()}
#' @param server Google geochart server to access.
#' @param cache Path to store downloaded data.
#' @param verbose Whether to show messages during processing.
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
#' @export
gchart_generate_countries <- function(
    countries = "all",
    server = "https://www.gstatic.com/charts/geochart/10/mapfiles/",
    cache = gchart_get_cache_path(),
    verbose = TRUE
){

  if(identical(countries, "all")){
    country_codes <- gchart_countries()
    countries <- country_codes$code
  } else {
    country_codes <- data.frame(code = countries)
  }

  if(verbose) message("Downloading country data... ", appendLF = FALSE)
  gchart_get_countries(
    countries = countries,
    server = server,
    cache = cache
  )
  if(verbose) message("Done.")

  if(verbose) message("Processing geographic data... ", appendLF = FALSE)
  country_geo_data <- gchart_process_areas(
    areas = countries,
    type = "countries"
  )
  if(verbose) message("Done.")

  return(country_geo_data)
}




#' Get Google Charts data for countries
#' @description
#' Access the Google Charts geochart data for countries
#'
#' @details
#' The function invisibly returns the file path for successful requests or
#' the response status code for failed requests, in a character vector with the
#' country code for each element.
#'
#' @param countries Country codes to get.
#' @param server Google geochart server to access.
#' @param cache Path to store downloaded data.
#' @importFrom httr GET
#' @importFrom httr content
gchart_get_countries <- function(
    countries = "all",
    server = "https://www.gstatic.com/charts/geochart/10/mapfiles/",
    cache = gchart_get_cache_path()
){
  if(identical(countries, "all")){
    countries <- gchart_countries$code
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
  files <- paste0(countries, "_PROVINCES.js")

  # different file name for some countries
  files[countries %in% different_file] <-
    paste0(
      countries[countries %in% different_file],
      "_COUNTRIES.js"
    )


  # track request results
  results <- character(length(countries))
  names(results) <- countries

  # use the folder for country files
  country_path <- file.path(cache, "countries")
  dir.create(country_path, recursive = TRUE, showWarnings = FALSE)


  mapply(
    countries, files,
    FUN = function(code, file){
      local_file <- file.path(country_path, paste0(code, ".js"))
      if(file.exists(local_file)){
        results[code] <<- local_file
      } else {
        response <- httr::GET(paste0(server, file))

        if(response$status_code == 200){
          results[code] <<- local_file
          content <- response |>
            httr::content(as = "text", encoding = "UTF-8") |>
            cat(file = local_file)
        } else {
          cat("error:", response$status_code, file = local_file)
          results[code] <<- response$status_code
        }
      }
    }
  )

  return(invisible(results))
}




#' Extracts the list of available countries from the Google Charts documentation
#' @param server Location for the relevant Google Charts documentation.
#' Use the default.
#' @param cache Path to store downloaded data.
#' @param format Which format to use for storing the countries table.
#' @param verbose Whether to show messages during processing.
#' @details
#' Extracts the information of countries available in the Google Charts geochart
#' service from the online documentation
#' @returns A \code{data.frame} with information for all countries available in
#' the service.
#' @importFrom httr GET
#' @importFrom httr content
#' @export
gchart_countries <- function(
    server = "https://developers.google.com/chart/interactive/docs/gallery/geochart",
    cache = gchart_get_cache_path(),
    format = c("rds", "rda"),
    verbose = TRUE
){

  # use a single format
  format <- format[1]

  # set up export
  export_fun <- switch(
    format,
    rds = saveRDS,
    rda = save
  )

  file <- file.path(cache, paste0("countries.", format))

  if(file.exists(file)){
    message("Reading cached country table.")
    countries <- switch(
      format,
      rds = readRDS(file),
      rda = load(file)
    )
  } else {
    message("Getting updated country table.")
    # get the country information from the Google Charts documentation
    document <- httr::GET(server)
    code <- document$status_code

    if(code != 200L) stop("Documentation not found at ", url, "!")

    # manually process html table
    table_html_text <- document |>
      # get html content as text
      httr::content(as = "text") |>
      # select the only table with data
      sub(
        pattern = ".*<table>",
        replacement = "",
        x = _
      ) |>
      sub(
        pattern = "(.+?)</table>.*",
        replacement = "\\1",
        x = _
      ) |>
      # simplify all html tags
      gsub(
        pattern = "<(table|tr|td|th)[[:space:]]*[^>]*>[[:space:]]*",
        replacement = "\\1:",
        x = _
      ) |>
      gsub(
        pattern = "</[^>]+>",
        replacement = "",
        x = _
      ) |>
      # keep the href attributes
      gsub(
        pattern = "<a href=\"([^\"]+)\"[^>]*>([^,<[:space:]]+)([,<[:space:]])",
        replacement = "\\2;\\1\\3",
        x = _
      ) |>
      gsub(
        pattern = "<[^>]+>",
        replacement = "",
        x = _
      ) |>
      gsub(
        pattern = "[[:space:]]{2,}",
        replacement = "",
        x = _
      ) |>
      sub(
        pattern = "^tr:",
        replacement = "",
        x = _
      )


    # extract the html table data
    countries <- table_html_text |>
      sub(
        pattern = "^th.+?td:",
        replacement = "td:",
        x = _
      ) |>
      strsplit(split = "tr:") |>
      unlist() |>
      sub(
        pattern = "^td:",
        replacement = "",
        x = _
      ) |>
      strsplit(split = "td:") |>
      lapply(
        FUN = function(row){
          if(length(row) == 2) row <- c(NA_character_, row)

          # process country codes
          dfr <- row |>
            getElement(3) |>
            strsplit(split = ",") |>
            unlist() |>
            strsplit(split = ";") |>
            lapply(t) |>
            do.call(what = rbind, args = _) |>
            as.data.frame()

          names(dfr) <- c("code", "url")

          # add regional groupings
          dfr$continent <- row[1]
          dfr$sub_continent <- row[2]
          dfr <- dfr[, c("continent", "sub_continent", "code", "url")]

          # add tibble classes for tidyverse users
          class(dfr) <- c("tbl_df", "tbl", class(dfr))

          return(dfr)
        }
      ) |>
      do.call(what = rbind, args = _)


    # fill in the empty continents
    available <- which(!is.na(countries$continent))

    countries$continent <- rep(
      x = countries$continent[available],
      times = diff(c(available, length(countries$continent)+1))
    )

    # remove codes that are no longer valid
    deprecated <- c(
      "ZR", "DD", "FX", "SU", "YU", "AN", "BU", "TP", "NT", "YD"
    )
    countries <- subset(countries, subset = ! code %in% deprecated)

    export_fun(countries, file)
  }

  return(countries)
}
