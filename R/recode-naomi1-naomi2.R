#' Recode age group from Naomi 1 to Naomi 2
#'
#' @param x Character vector of age groups in Naomi 1 format
#'
#' @return Character vector of age groups in Naomi 2 format
#'
#' @examples
#' recode_age_group(c("15-19", "15+", "00+"))
#'
#' @export
recode_naomi1_age_group <- function(x) {

  val <- x
  val <- sub("([0-9]{2})-([0-9]{2})", "Y0\\1_0\\2", val)
  val <- sub("([0-9]{2})\\+", "Y0\\1_999", val)

  if ( !all(val %in% naomi::get_age_groups()$age_group) ) {
    stop("Invalid age_groups found: ",
         paste(unique(x[!val %in% naomi::get_age_groups()$age_group]), collapse = ", "))
  }
    
  val
}
