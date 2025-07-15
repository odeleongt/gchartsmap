#' Extracts the list of available countries from the Google Charts documentation
#' @param url Location for the relevant Google Charts documentation.
#' Use the default.
#' @param cache Whether the countries table should be stored in disk
#' @param path Place to store the countries table if it is being cached
#' @param format Which format to use for storing the countries table
#' @details
#' Extracts the information of countries available in the Google Charts geochart
#' service from the online documentation
#' @returns A \code{data.frame} with information for all countries available in
#' the service.
#' @importFrom httr GET
#' @importFrom httr content
#' @export
gcharts_countries <- function(
    url = "https://developers.google.com/chart/interactive/docs/gallery/geochart",
    cache = FALSE,
    path = NULL,
    format = NULL
){
  # get the country information from the Google Charts documentation
  document <- httr::GET(url)

  if(document$status_code != 200L) stop("Documentation not found at ", url, "!")

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

  return(countries)
}
