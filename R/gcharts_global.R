

country_codes <- gcharts_countries()


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

country_codes$url <- paste0(
  "https://www.gstatic.com/charts/geochart/10/mapfiles/",
  country_codes$file
)


mapply(
  country_codes$code, country_codes$file,
  FUN = function(code, file){
    local_file <- paste0("data/countries/", code, ".js")
    if(!file.exists(local_file)){
      tryCatch(
        download.file(
          url = paste0(
            "https://www.gstatic.com/charts/geochart/10/mapfiles/",
            file
          ),
          destfile = local_file
        ),
        error = function(e) "Not found"
      )
    }
  }
)



country_codes$found <- country_codes$iso2c %in% gsub(
  "^([^_]+).+",
  "\\1",
  list.files("data/test")
)


country_codes[!country_codes$found, ]
