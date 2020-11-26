#' Recode age group from Naomi 1 to Naomi 2
#'
#' @param x Character vector of age groups in Naomi 1 format
#'
#' @return Character vector of age groups in Naomi 2 format
#'
#' @examples
#' recode_naomi1_age_group(c("15-19", "15+", "00+"))
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

#' Update ART and ANC programme data set to Naomi 2.0 specifications
#'
#' @param art Data frame of ART data conforming to Naomi 1.0 schema.
#' @param anc Data frame of ANC testing data conforming to Naomi 1.0 schema.
#' @return Data frame of ART data conforming to Naomi 2.0 schema.
#'
#' @details
#'
#' * Rename `current_art` column to `art_current`.
#' * Recode `year` column to `calendar_quarter` in ART dataset.
#' * Recode `age_group` column from `15-49` format to `Y015_049`.
#' * Recode `ancrt_*` columns to `anc_*`. 
#'
#' @export
recode_naomi1_art <- function(art) {
  art <- dplyr::reame(art, art_current = current_art, calendar_quarter = year)
  art$calendar_quarter <- paste0("CY", art$calendar_quarter, "Q4")
  art$age_group <- recode_naomi1_age_group(art$age_group)
  art
}

#' @rdname recode_naomi1_art
#' @export
recode_naomi1_anc <- function(anc) {
  anc <- dplyr::rename_all(anc, function(x) sub("ancrt", "anc", x))
  anc <- dplyr::rename(anc, anc_tested_pos = anc_test_pos)
  anc$age_group <- recode_naomi1_age_group(anc$age_group)
  anc
}
