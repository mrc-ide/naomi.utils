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

  population_cols <- c("area_id", "calendar_quarter", "sex", "age_group", "population", "asfr")

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
