#' Validate naomi population dataset
#'
#' @param area_level area level(s) at which population is supplied
#'
#' @return Invisibly TRUE or raises error.
#'
#' @details
#' Check that:
#' * Column names match schema
#' * Population stratification has exactly area_id / sex / age_group for
#'   each year data are supplied
#'
#' @export
validate_naomi_population <- function(population, areas, area_level) {

  population_cols <- c("area_id", "area_name", "calendar_quarter", "sex", "age_group", "population", "asfr")

  stopifnot(population_cols %in% names(population))


  cq <- unique(population$calendar_quarter)
  area_ids <- areas$area_id[areas$area_level %in% area_level]
  sexes <- c("female", "male")
  age_groups <- naomi::get_five_year_age_groups()

  check_strata <- expand.grid(calendar_quarter = cq,
                              area_id = area_ids,
                              sex = sexes,
                              age_group = age_groups)

  ## TODO: Elaborate to return useful error messages
  stopifnot(
    dplyr::anti_join(population, check_strata,
                     by = names(check_strata)) == 0
  )
  
  stopifnot(
    dplyr::anti_join(population, check_strata,
                     by = names(check_strata)) == 0
  )

  invisible(TRUE)
}



#' Extract WorldPop raster data
#'
#' @param areas Naomi area hierarchy dataset with boundaries.
#' @param iso3 ISO3 country code.
#' @param years Years to extract WorldPop data
#'
#' @return A data frame formatted as Naomi population dataset
#'
#' @details
#'
#' Raster files are downloaded from the WorldPop FTP. Some files are very large.
#' It is recommended to run this on a fast internet connection.
#'
#' @export
naomi_extract_worldpop <- function(areas, iso3 = areas$area_id[areas$area_level == 0],
                                   years = c(2010, 2015, 2020)) {

  stopifnot(inherits(areas, "sf"))
  stopifnot(grepl("^[A-Z]{3}$", iso3))
  stopifnot(years %in% 2000:2020)

  wp_ages <- c("0" = "Y000_004",
               "1" = "Y000_004",
               "5" = "Y005_009",
               "10" = "Y010_014",
               "15" = "Y015_019",
               "20" = "Y020_024",
               "25" = "Y025_029",
               "30" = "Y030_034",
               "35" = "Y035_039",
               "40" = "Y040_044",
               "45" = "Y045_049",
               "50" = "Y050_054",
               "55" = "Y055_059",
               "60" = "Y060_064",
               "65" = "Y065_069",
               "70" = "Y070_074",
               "75" = "Y075_079",
               "80" = "Y080_999")
  wp_sexes <- c("m" = "male", "f" = "female")


  grid <- expand.grid(iso3 = iso3,
                      year = years,
                      wp_age = names(wp_ages),
                      wp_sex = names(wp_sexes),
                      stringsAsFactors = FALSE)

  pop_list <- do.call(Map, c(f = worldpop_extract_one, areas = list(list(areas)), grid))
  pop <- dplyr::bind_rows(pop_list)
  pop$age_group <- dplyr::recode(pop$wp_age, !!!wp_ages)
  pop$sex <- dplyr::recode(pop$wp_sex, !!!wp_sexes)
  pop$calendar_quarter <- paste0("CY", pop$year, "Q2")
  pop$source <- "WorldPop"

  pop <- pop %>%
    dplyr::left_join(
             dplyr::select(sf::st_drop_geometry(areas), area_id, area_name),
             by = "area_id"
           )

  pop <- dplyr::count(pop, area_id, area_name, source, calendar_quarter, sex, age_group,
                      wt = population, name = "population")
  pop$asfr <- NA_real_

  validate_naomi_population(pop, areas, unique(areas$area_level))
  pop
}

worldpop_ftp_path <- function(iso3, year, wp_sex, wp_age) {
  wp_ftp <- "ftp://ftp.worldpop.org.uk/GIS/AgeSex_structures/Global_2000_2020"
  filename <- paste0(tolower(iso3), "_", wp_sex, "_", wp_age, "_", year, ".tif")
  file.path(wp_ftp, year, toupper(iso3), filename)
}

worldpop_extract_one <- function(areas, iso3, year, wp_sex, wp_age) {
  url <- worldpop_ftp_path(iso3, year, wp_sex, wp_age)
  tmpf <- tempfile(fileext = ".tif")
  on.exit(unlink(tmpf, force = TRUE))

  download.file(url, tmpf, mode = "wb")
  rast <- raster::raster(tmpf)
  population <- exactextractr::exact_extract(rast, areas, "sum")

  data.frame(area_id = areas$area_id,
             year,
             wp_sex,
             wp_age,
             population,
             stringsAsFactors = FALSE)
}


#' Extract Gridded Population of the World (GPW) raster data
#'
#' @param areas Naomi area hierarchy dataset with boundaries.
#' @param gpw_path Local path to GPW v4.11 raster files.
#'
#' @return A data frame formatted as Naomi population dataset.
#'
#' @details
#' This function relies on accessing GPW population files via a local path
#' to the GPW v4.11 rasters because the files are very large.
#'
#' Datasets are downloaded from:
#'
#' * Age/sex stratified populations for 2010: https://sedac.ciesin.columbia.edu/data/set/gpw-v4-basic-demographic-characteristics-rev11/data-download (each file ~2GB).
#' * Total population in 2000, 2005, 2010, 2015, 2020 (unraked): https://sedac.ciesin.columbia.edu/data/set/gpw-v4-population-count-rev11/data-download (each file ~400MB).
#'
#' Downloaded datasets should be saved in the following directory structure under
#' `gpw_path`:
#'
#' ~/Data/population/GPW 4.11/
#' ├── Demographic characteristics
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a000_004_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a005_009_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a010_014_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a015_019_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a020_024_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a025_029_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a030_034_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a035_039_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a040_044_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a045_049_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a050_054_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a055_059_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a060_064_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a065_069_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a070_074_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a075_079_2010_30_sec_tif
#' │   ├── gpw-v4-basic-demographic-characteristics-rev11_a080_084_2010_30_sec_tif
#' │   └── gpw-v4-basic-demographic-characteristics-rev11_a085plus_2010_30_sec_tif
#' └── Unraked
#'     ├── gpw-v4-population-count-rev11_2000_30_sec_tif
#'     ├── gpw-v4-population-count-rev11_2005_30_sec_tif
#'     ├── gpw-v4-population-count-rev11_2010_30_sec_tif
#'     ├── gpw-v4-population-count-rev11_2015_30_sec_tif
#'     └── gpw-v4-population-count-rev11_2020_30_sec_tif
#' 
#' @export
#' 
naomi_extract_gpw <- function(areas, gpw_path = "~/Data/population/GPW 4.11/") {

  stopifnot(inherits(areas, "sf"))

  gpw_ages <- c("0" = "Y000_004",
                "5" = "Y005_009",
                "10" = "Y010_014",
                "15" = "Y015_019",
                "20" = "Y020_024",
                "25" = "Y025_029",
                "30" = "Y030_034",
                "35" = "Y035_039",
                "40" = "Y040_044",
                "45" = "Y045_049",
                "50" = "Y050_054",
                "55" = "Y055_059",
                "60" = "Y060_064",
                "65" = "Y065_069",
                "70" = "Y070_074",
                "75" = "Y075_079",
                "80" = "Y080_999",
                "85" = "Y080_999")

  gpw_years <- c(2000, 2005, 2010, 2015, 2020)

  pop_agesex_list <- Map(gpw_extract_agesex_one, list(areas), names(gpw_ages), gpw_path)
  pop_year_list <- Map(gpw_extract_year_one, list(areas), gpw_years, gpw_path)

  pop_agesex <- dplyr::bind_rows(pop_agesex_list)
  pop_agesex <- dplyr::group_by(pop_agesex, area_id)
  pop_agesex <- dplyr::mutate(pop_agesex, prop = pop_agesex / sum(pop_agesex))

  pop <- dplyr::bind_rows(pop_year_list)
  pop <- dplyr::left_join(pop, pop_agesex, by = "area_id")
  pop$population <- pop$population * pop$prop

  pop$age_group <- dplyr::recode(pop$age_start, !!!gpw_ages)
  pop$calendar_quarter <- paste0("CY", pop$year, "Q2")
  pop$source <- "GPW v4.11"

  pop <- pop %>%
    dplyr::left_join(
             dplyr::select(sf::st_drop_geometry(areas), area_id, area_name),
             by = "area_id"
           )

  pop <- dplyr::count(pop, area_id, area_name, source, calendar_quarter, sex, age_group,
                      wt = population, name = "population")
  pop$asfr <- NA_real_

  validate_naomi_population(pop, areas, unique(areas$area_level))
  pop
}

gpw_extract_agesex_one <- function(areas, age_start, gpw_path) {

  message("age_start = ", age_start)

  age_start <- as.integer(age_start)
  if(age_start == 85) {
    age_code <- "a085plus"
  } else {
    age_code <- sprintf("a%03d_%03d", age_start, age_start + 4)
  }
  
  dir <- paste0("gpw-v4-basic-demographic-characteristics-rev11_",
                age_code, "_2010_30_sec_tif")
  dir <- file.path(gpw_path, "Demographic characteristics", dir)

  file_m <- list.files(dir, "mt", full.names = TRUE)
  rast_m <- raster::raster(file_m)
  pop_m <- exactextractr::exact_extract(rast_m, areas, "sum")

  file_f <- list.files(dir, "ft", full.names = TRUE)
  rast_f <- raster::raster(file_f)
  pop_f <- exactextractr::exact_extract(rast_f, areas, "sum")

  pop_m <- data.frame(area_id = areas$area_id,
                      sex = "male",
                      age_start = age_start,
                      pop_agesex = pop_m,
                      stringsAsFactors = FALSE)

  pop_f <- data.frame(area_id = areas$area_id,
                      sex = "female",
                      age_start = age_start,
                      pop_agesex = pop_f,
                      stringsAsFactors = FALSE)

  dplyr::bind_rows(pop_m, pop_f)
}

gpw_extract_year_one <- function(areas, year, gpw_path) {

  message("year = ", year)

  file <- list.files(file.path(gpw_path, "Unraked"),
                     recursive = TRUE,
                     pattern = paste0(year, ".*tif$"),
                     full.names = TRUE)
  stopifnot(length(file) == 1)
  
  rast <- raster::raster(file)
  pop <- exactextractr::exact_extract(rast, areas, "sum")

  data.frame(area_id = areas$area_id,
             year = year,
             population = pop,
             stringsAsFactors = FALSE)
}

