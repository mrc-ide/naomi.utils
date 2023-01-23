#' Data frame of age groups
#'
#' Return a data frame consisting of master age groups
#'
#' @return data frame
#' @export
get_age_groups <- function() {
  groups <- readr::read_csv(system_file("metadata", "meta_age_group.csv"))
  groups$age_group_span <- as.numeric(groups$age_group_span)
  groups
}
