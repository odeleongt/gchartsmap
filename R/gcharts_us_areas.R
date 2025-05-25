#' Generate Google Charts spatial data for US areas
#' @description
#' This function queries Google Charts resources to identify the US geographic
#' areas used in services like Google Trends, and uses geographic data from the
#' US Census Bureau to provide those areas with subdivisions at the county level.
#' @param areas Area codes to get. Should be integers.
#' @param limit Maximum number of areas to look for.
#'
#' @examples
#'
#' library(package = "gchartsmap")
#'
#' gchartsmap::gchart_generate_us_areas(500L)
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
  gchart_us_areas <- gchart_available_areas() |>
    names() |>
    as.integer() |>
    gchart_process_us_areas()

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
gchart_process_us_areas <- function(
    areas,
    cache = gchart_get_cache_path()
){
  # needs to use cache
  if(cache == "") stop("Cache should be used. See `gchart_set_cache`.")

  # areas should be integers
  if(!is.integer(areas)) stop("Provide area codes as integers.")

  # at least one area
  if(length(areas) < 1) stop("Provide at least one area code.")

  # get all available areas
  available <- gchart_available_areas()

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
  area_data <- content |>
    lapply(sub, pattern = ".*?\\[(.*)\\].*", replacement = "\\1") |>
    lapply(jsonlite::fromJSON)

  # extract geographic information and reconstruct polygons
  areas_spatial <- vector(mode = "list", length = length(areas))
  names(areas_spatial) <- areas

  for(area in areas){
    # isolate the area
    region <- area_data[[area]]$features$polygons[[
      grep(area, area_data[[area]]$features$id)
    ]]

    multipolygon <- region[[1]] |>
      lapply(
        apply,
        2,
        as.numeric
      ) |>
      lapply(
        function(x){
          # approximate transformation to WGS84
          # exact coordinate reference system not documented by Google
          # but indicated that it is the projected Mercator space:
          # https://groups.google.com/g/google-visualization-api/c/KVGu--jjUpk/m/n_QOoqSKs9YJ

          n <- 0
          x[,1] <- 90 * (
            4*n + (
              (4*atan(exp(pi*x[,1]/180))) / pi
            ) - 1
          )

          # store with longitude first
          x[,c(2,1)]
        }
      ) |>
      lapply(
        function(x){
          if(nrow(x) == 3){
            x <- rbind(x, x[1,])
          } else {
            x
          }
        }
      ) |>
      lapply(sf::st_linestring) |>
      lapply(sf::st_cast, to = "POLYGON") |>
      sf::st_multipolygon() |>
      sf::st_geometry()

    areas_spatial[[area]] <- data.frame(
      area = area,
      geometry = multipolygon
    ) |>
      sf::st_as_sf(crs = 4326)
  }

  names_to_lower <- function(x){
    names(x) <- tolower(names(x))
    x
  }

  # compile all areas
  gchart_us_areas <- do.call(rbind, areas_spatial)

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

  gchart_counties <- gchart_us_areas |>
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
      select = c("area", "statefp", "countyfp", "geoid", "name", "contiguous")
    ) |>
    merge(
      counties |>
        base::subset(
          select = c("geoid", "geometry")
        )
    ) |>
    sf::st_sf()

  return(gchart_counties)
}




#' Get available areas
#' @description Get a list of areas that have been downloaded
#' @param cache Path where the downloaded data is stored
gchart_available_areas <- function(
    cache = gchart_get_cache_path()
){
  # needs to use cache
  if(cache == "") stop("Cache should be used. See `gchart_set_cache`.")

  # get all available areas
  available <- list.files(
    path = file.path(cache, "us-areas"),
    full.names = TRUE,
    pattern = "[.]js"
  )
  names(available) <- sub(".*/([^/]+).js", "\\1", available)

  return(available)
}
