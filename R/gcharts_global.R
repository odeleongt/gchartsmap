

country_codes <- subset(
  x = countrycode::codelist,
  select = c("continent", "region23", "country.name.en", "iso2c"),
  subset = !is.na(iso2c)
)



country_codes$file <- sapply(
  country_codes$iso2c,
  switch,
  AI = "AI_COUNTRIES.js",
  AQ = "AQ_COUNTRIES.js",
  AS = "AS_COUNTRIES.js",
  AQ = "AQ_COUNTRIES.js",
  AW = "AW_COUNTRIES.js",
  AX = "AX_COUNTRIES.js",
  BM = "BM_COUNTRIES.js",
  BV = "BV_COUNTRIES.js",
  IO = "IO_COUNTRIES.js",
  VG = "VG_COUNTRIES.js",
  KY = "KY_COUNTRIES.js",
  CX = "CX_COUNTRIES.js",
  CC = "CC_COUNTRIES.js",
  CK = "CK_COUNTRIES.js",
  CW = "CW_COUNTRIES.js",
  FK = "FK_COUNTRIES.js",
  FO = "FO_COUNTRIES.js",
  GF = "GF_COUNTRIES.js",
  PF = "PF_COUNTRIES.js",
  TF = "TF_COUNTRIES.js",
  GI = "GI_COUNTRIES.js",
  GP = "GP_COUNTRIES.js",
  GU = "GU_COUNTRIES.js",
  GG = "GG_COUNTRIES.js",
  HM = "HM_COUNTRIES.js",
  HK = "HK_COUNTRIES.js",
  IM = "IM_COUNTRIES.js",
  JE = "JE_COUNTRIES.js",
  KE = "KE_COUNTRIES.js",
  MO = "MO_COUNTRIES.js",
  MT = "MT_COUNTRIES.js",
  MQ = "MQ_COUNTRIES.js",
  YT = "YT_COUNTRIES.js",
  MC = "MC_COUNTRIES.js",
  MS = "MS_COUNTRIES.js",
  NC = "NC_COUNTRIES.js",
  NU = "NU_COUNTRIES.js",
  NF = "NF_COUNTRIES.js",
  MP = "MP_COUNTRIES.js",
  PS = "PS_COUNTRIES.js",
  PN = "PN_COUNTRIES.js",
  PR = "PR_COUNTRIES.js",
  RE = "RE_COUNTRIES.js",
  MF = "MF_COUNTRIES.js",
  SG = "SG_COUNTRIES.js",
  SX = "SX_COUNTRIES.js",
  GS = "GS_COUNTRIES.js",
  BL = "BL_COUNTRIES.js",
  PM = "PM_COUNTRIES.js",
  SJ = "SJ_COUNTRIES.js",
  TW = "TW_COUNTRIES.js",
  TK = "TK_COUNTRIES.js",
  TC = "TC_COUNTRIES.js",
  VI = "VI_COUNTRIES.js",
  UM = "UM_COUNTRIES.js",
  VA = "VA_COUNTRIES.js",
  WF = "WF_COUNTRIES.js",
  EH = "EH_COUNTRIES.js",
  NA_character_
)


country_codes$file[is.na(country_codes$file)] <- paste0(
  country_codes$iso2c[is.na(country_codes$file)], "_PROVINCES.js"
)

country_codes$url <- paste0(
  "https://www.gstatic.com/charts/geochart/10/mapfiles/",
  country_codes$file
)


mapply(
  country_codes$iso2c, country_codes$file,
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
