
#' Set up a cache directory
#' @description Ensures that the directory exists and sets the environment
#'     variable for access.
#' @param path Path to use for the package cache.
#' @param install if TRUE, will install the cache path in your \code{.Renviron}
#' file for use in future sessions. Defaults to FALSE.
#' @param overwrite If this is set to TRUE, it will overwrite an existing
#' cache path that you already have in your \code{.Renviron} file.
#' @param home Path for the \code{.Renviron} file. Defaults to "HOME".
#'
#' @returns Sets and returns the path to the cache where downloaded data will be
#' stored. Is used for the side effect of setting the `R_GOOGLE_CHART_CACHE`
#' environment variable, and can store the path in `.Renviron` for use in
#' future R sessions if requested.
#'
#' @examples
#'
#' library(package = "gchartsmap")
#'
#' # set the cache path to your system's cache path
#' gchartsmap::gchart_set_cache()
#'
#' \donttest{
#' # save the cache path in your home .Renviron file
#' gchartsmap::gchart_set_cache(install = TRUE)
#' }
#'
#' @export
gchart_set_cache <- function(
    path = tools::R_user_dir(package = "gchartsmap", which = "cache"),
    install = FALSE, overwrite = FALSE, home = "HOME"
){

  # verify the path
  path <- gchart_cache_dir(path = path)
  home <- path_existing_root(path = home)

  if(install){

    renv <- file.path(home, ".Renviron")

    # Backup original .Renviron before doing anything else here.
    if(file.exists(renv)){
      file.copy(renv, file.path(path, ".Renviron_backup"))
    }

    if(!file.exists(renv)){
      file.create(renv)
    } else {
      oldenv <- readLines(renv)
      if(isTRUE(overwrite)){
        message(
          paste(
            "Your original .Renviron was backed up and stored in your",
            "HOME directory in case you need to restore it."
          )
        )
        gchart_remove_cache_path(remove = TRUE, path = path)
      } else {
        if(any(grepl("R_GOOGLE_CHART_CACHE", oldenv))){
          warning(
            paste(
              "A R_GOOGLE_CHART_CACHE path already exists.",
              "You can overwrite it with the argument overwrite=TRUE"
            ),
            call.=FALSE
          )
        }
      }
    }

    pathconcat <- paste0("R_GOOGLE_CHART_CACHE='", path, "'")
    # Append API key to .Renviron file
    write(pathconcat, renv, sep = "\n", append = TRUE)
    message(
      paste(
        'The cache path has been stored in your .Renviron and can be accessed',
        'by Sys.getenv("R_GOOGLE_CHART_CACHE").'
      )
    )
  } else {
    message(
      paste(
        "To install the cache path for use in future sessions, run this",
        "function with `install = TRUE`."
      )
    )
  }

  # since the path exists and is writeable, set the environment variable and
  # return the path
  Sys.setenv(R_GOOGLE_CHART_CACHE = path)
  return(invisible(path))
}




#' Verify cache directory
#' @description Ensure that the cache directory exists
#' @param path Path to verify
#' @importFrom tools R_user_dir
gchart_cache_dir <- function(
    path = tools::R_user_dir(package = "gchartsmap", which = "cache")
){

  if(path == "HOME") path <- Sys.getenv("HOME")

  if(!file.exists(path)){
    # check if path can be created
    existing_path <- path_existing_root(path)
    if(file.access(existing_path, mode = 2) == 0){
      dir.create(path, recursive = TRUE)
    } else {
      stop(
        "The directory '", path, "' doesn't exist and can't be created. ",
        "The existing directory '", existing_path, "' is not writeable. ",
        "Please provide a writeable directory to store cached chart data."
      )
    }
  }

  # make sure the path is writeable
  if(!file.access(path, mode = 2) == 0){
    stop(
      "The directory '", path, "' is not writeable. ",
      "Please provide a writeable directory to store cached chart data."
    )
  }

  # since the path exists and is writeable return the path
  return(path_existing_root(path))
}




#' Find existing path
#' @description Find the deepest path component that exists.
#' @param path Path to analyze.
#' @keywords internal
path_existing_root <- function(
    path
){

  if(path == "HOME") path <- Sys.getenv("HOME")

  # get the components of the path
  hierarchy <- strsplit(path, split = "/|[\\]+") |>
    unlist()

  # list all paths along the hierarchy, ignoring the system root
  all_paths <- sapply(
    1:length(hierarchy),
    function(x) paste(hierarchy[1:x], collapse = "/")
  )[2:length(hierarchy)]

  # check the deepest path that exists
  exists <- file.exists(all_paths)

  # return the existing base path
  all_paths[max(which(exists))]
}




#' Delete a saved cache path
#' @description This function will uninstall the cache path from the environment
#' variables. If a path is provided, it will
#' be used to remove the variable from .Renviron file in that path.
#' @param remove Whether to remove the path.
#' @param path Path to look for an .Renviron file.
#' @keywords internal
gchart_remove_cache_path <- function(
    remove = FALSE,
    path = NULL
){
  if(remove){
    Sys.unsetenv("R_GOOGLE_CHART_CACHE")

    if(!is.null(path)){
      # verify path
      path <- gchart_cache_dir(path)

      renv <- file.path(path, ".Renviron")

      if(file.exists(renv)){
        # Backup original .Renviron before doing anything else here.
        file.copy(renv, file.path(path, ".Renviron_backup"))

        oldenv <- readLines(renv)

        newenv <- grep(
          "R_GOOGLE_CHART_CACHE",
          oldenv, value = TRUE, invert = TRUE
        )
        writeLines(newenv, renv, sep = "\n")
      }
    }
  }
}


#' Get the cache path
#' @description Get the saved cache path
#' @param path Path to use as cache
#'
#' @returns Returns the path to the local cache as set in the
#' `R_GOOGLE_CHART_CACHE` environment variable. If that is not set, gets the
#' system's default cache path for the package as provided by
#' `tools::R_user_dir()`.
#'
#' @examples
#'
#' library(package = "gchartsmap")
#'
#' # set the cache path to your system's cache path
#' gchartsmap::gchart_set_cache()
#'
#' # check the set cache
#' gchartsmap::gchart_get_cache_path()
#'
#' @export
gchart_get_cache_path <- function(
    path = NULL
){
  if(is.null(path)){
    Sys.getenv("R_GOOGLE_CHART_CACHE")
  } else {
    gchart_cache_dir(path)
  }
}
