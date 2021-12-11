#' Read Multiple Shape Files in ZIP Archive
#'
#' Reads all files in ZIP archive `zfile` matching `pattern` with
#' function `read_fn` and returns as a list.
#'
#' @param zfile path to a zip directory
#' @param pattern string pattern passed to [`list.files`].
#' @param read_fn function used to read matched files.
#'
#' @export
read_sf_zip_list <- function(zfile, pattern = "\\.shp$", read_fn = sf::read_sf) {
  tmpd <- tempfile()
  on.exit(unlink(tmpd))
  unzip(zfile, exdir = tmpd)
  f <- list.files(tmpd, pattern, recursive = TRUE, full.names = TRUE)
  lapply(f, sf::read_sf)
}

#' Read shape file from ZIP
#'
#' @param zfile Path to zip file
#' @param pattern Pattern to read files for from zip, defaults to files ending
#' with 'shp'
#'
#' @export
read_sf_zip <- function(zfile, pattern = "shp$") {
 tmpd <- tempfile()
 on.exit(unlink(tmpd))
 utils::unzip(zfile, exdir = tmpd)
 sf::read_sf(list.files(tmpd, pattern, recursive = TRUE, full.names = TRUE))
}


#' Check full and aggregated boundaries
#'
#' This function is useful for checking level of coarseness
#' of a simplified versus raw shapefile and any slivers
#' in a shapefile.
#'
#' @param sh1 Bottom shapefile with red boundaries
#' @param sh2 Top shapefile with red boundaries
#'
#' @export
check_boundaries <- function(sh1, sh2 = NULL){
  gridExtra::arrangeGrob(
    compare_boundaries(sh1, sh2, aggregate = TRUE),
    compare_boundaries(sh1, sh2, aggregate = FALSE),
    nrow = 1)
}


#' Compare boundaries of two shapefiles by overlaying them
#'
#' @param sh1 is bottom shapefile with red boundaries
#' @param sh2 is top shapefile with red boundaries
#' @param aggregate whether to aggregate shapefiles
#'
#' @export
compare_boundaries <- function(sh1, sh2 = NULL, aggregate = FALSE) {

  if(aggregate){
    sh1 <- dplyr::summarise(sh1)
    if(!is.null(sh2))
      sh2 <- dplyr::summarise(sh2)
  }
  p <- ggplot2::ggplot() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank())

  p <- p + ggplot2::geom_sf(data = sh1, color = "red")
  if(!is.null(sh2))
    p <- p + ggplot2::geom_sf(data = sh2, color = "blue", fill = NA)

  p
}


#' Plot area hierarchy levels
#'
#' @param areas area hierarchy sf object
#' @param nrow number of rows, integer.
#'
#' @return A ggplot2 object illustrating the area hierarchy
#'
#' @export
plot_area_hierarchy_summary <- function(areas, nrow = 1) {

  df <- areas %>%
    dplyr::group_by(area_level) %>%
    dplyr::mutate(
             label = sprintf("%s (%d)", area_level_label, dplyr::n())
           ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(label = forcats::fct_reorder(label, area_level))

  ggplot2::ggplot(df) +
    ggplot2::geom_sf() +
    ggplot2::facet_wrap(~label, nrow = nrow) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text = ggplot2::element_blank())
}

#' Save sf object to zipped ESRI .shp file
#'
#' Save an sf object as a zipped archive with the four ESRI shape
#' file components `.shp`, `.dbf`, `.prj`, `.shx`. This wraps
#' [`sf::write_sf()`].
#'
#' @param obj an object of class `sf`.
#' @param zipfile path to write zip output file. Must have file extension .zip.
#' @param overwrite logical whether to overwrite `zipfile` if it already exists.
#'
#' @return Return value of `file.copy()`, `TRUE` if file successfully written.
#'
#' @examples
#' nc <- read_sf(system.file("shape/nc.shp", package="sf"))
#' write_sf_shp_zip(nc, "nc.zip")
#'
#' @export
write_sf_shp_zip <- function(obj, zipfile, overwrite = FALSE) {

  stopifnot(tools::file_ext(zipfile) == "zip")

  if (file.exists(zipfile) && !overwrite) {
    stop(paste0("File already exists: ", zipfile,
                "\nUse 'overwrite = TRUE' to overwrite the existing file."))
  }

  tmp_zip <- basename(zipfile)
  shp_name <- paste0(tools::file_path_sans_ext(tmp_zip), ".shp")

  ## Temporary directory to write .shp file
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE))

  sf::write_sf(obj, file.path(tmp, shp_name), delete_layer = TRUE)
  withr::with_dir(tmp, zip(tmp_zip, list.files()))

  file.copy(file.path(tmp, tmp_zip), zipfile, overwrite = overwrite)
}

#' Data validation for input population data
#'
#' Checks for:
#' (1) consistent aread IDs between pop data and boundaries file
#' (2) pop data age groups consistent with naomi age groups
#'
#' @param pop_data population dataframe
#' @param boundaries boundary file conatining area IDs
#'
#' @return If unique area IDs are present between the
#'  two datasets, an error will be generated along
#'  with a map of mismatching IDs. If unique age groups area present,
#'  an error will be generated.
#'
#' @export
assert_pop_data_check <- function(pop_data, boundaries) {

  x <- pop_data["area_id"]
  y <- boundaries["area_id"]

  if (nrow(setdiff(x, y)) != 0 || nrow(setdiff(y, x)) != 0 ) {
    x1 <- setdiff(x, y)
    x1$source <- "pop_data"
    y1 <- setdiff(y, x)
    y1$source <- "boundaries"

    var_map <- rbind(x1, y1)
    print(var_map, n=nrow(var_map))
  }

  if(nrow(setdiff(x,y))!=0 || nrow(setdiff(y, x)) != 0) stop("Area hierarchy IDs do not match population data")

  if(!all(pop_data$age_group %in% naomi::get_five_year_age_groups())) stop("All age groups not present in population data")


}

#' Checks for consistent area IDs between two datasets
#'
#' @param df1 a dataframe containing area_id
#' @param df2 a dataframe containing area_id
#' @param key list of columns to compare
#'
#' @return If unique area IDs are present between the
#'  two datasets, an error will be generated along
#'  with a map of mismatching IDs
#'
#' @export
assert_area_id_check <- function(df1, df2, key) {
  x <- df1[key]
  y <- df2[key]

  if (nrow(setdiff(x, y)) != 0) {
    x1 <- setdiff(x, y)
    x1$source <- paste(substitute(df1), collapse = " ")
    y1 <- setdiff(y, x)
    y1$source <- paste(substitute(df2), collapse = " ")

    var_map <- rbind(x1, y1)
    print(var_map, n=nrow(var_map))
  }

  if(nrow(setdiff(x,y))!=0) stop("Area hierarchy mismatch")
}

#' Generate single Naomi area id
#'
#' Generate a Naomi area ID consisting of ISO3, area level and
#' a random `nchar` digit alpha numeric.
#'
#' @param iso3 three character ISO3 code
#' @param level area level as an integer
#' @param nchar number of alpha numeric digits to generate
#'
#' @return An area_id in the format `<ISO3>_<level>_<xyz12>`.
#'
#' @details
#'
#' This function is not vectorized. It generates a single area ID.
#' 
#' This function does not set the seed. Ensure to set the seed
#' before calling the function if you want to reproduce the
#' same results.
#'
#' @examples
#'
#' generate_area_id("ISO", 1)
#'
#' @export
generate_area_id <- function(iso3, level, nchar = 5) {

  stopifnot(grepl("[A-Z]{3}", iso3))
  stopifnot(!is.na(as.integer(level)))
  stopifnot(length(iso3) == 1)
  stopifnot(length(level) == 1)

  random_str <- paste0(sample(c(letters, 0:9), nchar, replace = TRUE), collapse = "")
  paste0(iso3, "_", level, "_", random_str)
}


#' Convert nested hierarchy from wide to long format
#'
#' @param x Wide format nested hierarchy.
#'
#' @export
gather_areas <- function(x) {

  val <- x %>%
    dplyr::group_by(area_id = id0,
             area_name = name0,
             area_level = 0,
             parent_area_id = NA,
             spectrum_region_code =
               if(n_distinct(id0) == n_distinct(id0, spectrum_region_code)) spectrum_region_code else NA
             ) %>%
    dplyr::summarise(.groups = "drop")

  for(i in 1:6) {

    if(exists(paste0("id", i), x) && !is.na(x[[paste0("id", i)]])) {

      new_level <- x %>%
          dplyr::rename(area_id = paste0("id", i),
                        area_name = paste0("name", i),
                        parent_area_id = paste0("id", i-1)) %>%
          dplyr::mutate(
            area_level = i,
            area_id = factor(area_id, unique(area_id)),   # ensure same order
            area_name = factor(area_name, unique(area_name)),
            spectrum_region_code =
              if(dplyr::n_distinct(area_id) == n_distinct(area_id, spectrum_region_code))
                spectrum_region_code else NA
          ) %>%
          dplyr::group_by(area_id, area_name, area_level, parent_area_id,
                          spectrum_region_code) %>%
          dplyr::summarise(.groups = "drop")
    
      val <- rbind(val, new_level)
    }
  }

  dplyr::ungroup(val)
}


#' Checks valid age groups
#'
#' @param var a value in dataframe extracted using `$` notation
#'
#' @return If additional age groups are present or missing values
#' for age group, an error will be generated
#'
#' @export
assert_pop_age_group <- function(var) {

  age_groups <- naomi::get_five_year_age_groups()

  stopifnot(var %in% age_groups)
}

