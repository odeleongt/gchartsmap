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



#' Process Google Charts data for US areas
#' @description
#' Process the downloaded Google Charts geochart data for US areas
#'
#' @details
#' Google Charts data is served as JavaScript code that defines objects with the
#' desired data. This function processes the locally-available Google Charts js
#' files to generate spatial objects.
#'
#' @param areas Area codes to get. Should be integers. If not provided, all
#' available areas are processed.
#' @param type Type of geo-resource. Defaults to countries.
#' @param cache Path where the downloaded data is stored.
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_linestring
#' @importFrom sf st_cast
#' @importFrom sf st_multipolygon
#' @importFrom sf st_sf
#' @importFrom sf st_as_sf
#' @importFrom sf st_transform
#' @importFrom sf st_centroid
#' @importFrom sf st_geometry
#' @importFrom sf st_join
#' @importFrom sf sf_use_s2
#' @importFrom tigris counties
#' @importFrom tigris states
gchart_process_areas <- function(
    areas,
    type = c("countries", "us-areas"),
    cache = gchart_get_cache_path()
){
  # needs to use cache
  if(cache == "") stop("Cache should be used. See `gchart_set_cache`.")

  # at least one area
  if(length(areas) < 1) stop("Provide at least one area code.")

  # only one geo-resource type
  type <- type[1]

  # get all available areas
  available <- gchart_available_areas(type = type)

  # all areas should be available
  areas <- as.character(areas)
  missing <- setdiff(areas, names(available))
  areas <- setdiff(areas, missing)

  if(length(missing) > 0){
    stop(
      "All requested areas should be downloaded first.\n",
      "See `?gchartsmap::gchart_get_us_areas` for more information.\n",
      "Missing:\n", paste(missing, collapse = ", ")
    )
  }

  # get all data
  area_files <- available[areas]
  content <- suppressWarnings(lapply(area_files, readLines))
  content <- content[!grepl("error", content)]

  # extract data from JSON
  json_data <- content |>
    lapply(sub, pattern = ".*?\\[(.*)\\].*", replacement = "\\1")

  if("US" %in% areas){
    # fix multipart for us
    json_data[["US"]] <- paste0("{\"views\": [", json_data[["US"]], "]}")
  }

  # read json data
  area_data <- json_data |>
    lapply(jsonlite::fromJSON)


  if("US" %in% areas){
    # fix multipart for US
    area_data[["US"]]$features <- do.call(
      rbind, area_data[["US"]]$views$features
    )

    # fix Alaska longitudes (was stored as positive)
    area_data[["US"]]$features$polygons[[50]]$shell <- lapply(
      area_data[["US"]]$features$polygons[[50]]$shell,
      function(x){
        x <- apply(x, 2, as.numeric)
        x[,2] <- x[,2] - 360
        x
      }
    )

  }



  # extract geographic information and reconstruct polygons
  areas_spatial <- vector(mode = "list", length = length(areas))
  names(areas_spatial) <- areas

  for(area in areas){
    # isolate the area
    region <- area_data[[area]]$features$polygons[
      grep(area, area_data[[area]]$features$id)
    ]

    areas_spatial[[area]] <- data.frame(
      id = grep(area, area_data[[area]]$features$id, value = TRUE),
      geometry = lapply(
        lapply(
          lapply(
            region,
            function(x){
              x <- lapply(
                x[[1]],
                function(x){
                  x <- apply(x, 2, as.numeric)

                  # approximate transformation to WGS84
                  # exact coordinate reference system not documented by Google
                  # but indicated that it is the projected Mercator space:
                  # https://groups.google.com/g/google-visualization-api/c/KVGu--jjUpk/m/n_QOoqSKs9YJ

                  n <- 0
                  x[, 1] <- 90 * (
                    4 * n + (
                      (4 * atan(exp(pi * x[, 1] / 180))) / pi
                    ) - 1
                  )

                  # store with longitude first
                  x <- x[, c(2, 1)]

                  if (nrow(x) == 3) {
                    x <- rbind(x, x[1, ])
                  }

                  x
                }
              )

              x[[which.max(sapply(x, nrow))]]
            }
          ),
          sf::st_linestring
        ), sf::st_cast, to = "POLYGON"
      ) |>
        sf::st_as_sfc()
    ) |>
      sf::st_sf() |>
      sf::st_set_crs(value = 4326)
  }

  # compile all areas
  gchart_areas <- do.call(rbind, areas_spatial)

  names_to_lower <- function(x){
    names(x) <- tolower(names(x))
    x
  }

  # additional information for US areas
  if(type == "us-areas"){
    # get all contiguous states
    states <- tigris::states(cb = TRUE) |>
      suppressMessages() |>
      names_to_lower() |>
      sf::st_transform(crs = 4326)

    states$contiguous <- states$statefp < 60 & !states$statefp %in% c("02", "15")

    # get all us counties
    counties <- tigris::counties(cb = TRUE) |>
      suppressMessages() |>
      names_to_lower() |>
      sf::st_transform(crs = 4326) |>
      merge(
        y = as.data.frame(states)[, c("statefp", "contiguous")]
      )

    # disable s2 if necessary
    olds2 <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))

    # only keep the area number as id
    gchart_areas$id <- sub("US-", "", gchart_areas$id)

    gchart_counties <- gchart_areas |>
      sf::st_join(
        counties |>
          sf::st_centroid() |>
          suppressWarnings() |>
          base::subset(
            select = c("statefp", "countyfp", "geoid", "name", "contiguous")
          )
      ) |>
      suppressMessages() |>
      as.data.frame() |>
      base::subset(
        subset = !is.na(area),
        select = c("id", "statefp", "countyfp", "geoid", "name", "contiguous")
      ) |>
      merge(
        counties |>
          base::subset(
            select = c("geoid", "geometry")
          )
      ) |>
      sf::st_sf()

    return(gchart_counties)
  } else{
    # otherwise return the collected sf data
    return(gchart_areas)
  }
}




#' Get available areas
#' @description Get a list of areas that have been downloaded
#' @param type Type of geo-resource. Defaults to countries.
#' @param cache Path where the downloaded data is stored.
gchart_available_areas <- function(
    type = c("countries", "us-areas"),
    cache = gchart_get_cache_path()
){
  # needs to use cache
  if(cache == "") stop("Cache should be used. See `gchart_set_cache`.")

  # return only one type of geo-resource
  type <- type[1]

  # get all available areas
  available <- list.files(
    path = file.path(cache, type),
    full.names = TRUE,
    pattern = "[.]js"
  )
  names(available) <- sub(".*/([^/]+).js", "\\1", available)

  return(available)
}
