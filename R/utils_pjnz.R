
#' Extract the .DP and .PJN from a Spectrum PJNZ
#'
#' Copy a PJNZ file to a new location an delete everything except for the
#' .DP and .PJN files.
#' 
#' @param pjnz file path to source PJNZ
#' @param out file path to save output
#' @param shiny90 file path to external .shiny90 zip (optional)
#' @param force_shiny90 Logical whether or not to force replacement
#'   of a .shiny90 file already in the PJNZ with the provided path.
#'   The default behaviour is not to replace the .shiny90 file
#'   if it already exists in the PJNZ.
#' 
#' @details
#'
#' Both pjnz and out must be length 1. To apply to multiple files, use
#' `Map` function, e.g. `Map(copy_pjnz_extract, pjnz_list, out_list)`.
#' 
#' The file must be renamed (pjnz cannot equal out) to avoid inadvertently
#' deleting components from an archived PJNZ file.
#'
#' The default `force_shiny90 = FLASE)
#'
#' @export
copy_pjnz_extract <- function(pjnz, out, shiny90 = NULL, force_shiny90 = FALSE) {

  stopifnot(length(pjnz) == 1)
  stopifnot(length(out) == 1)
  stopifnot(length(shiny90) <= 1)

  if (!is.null(shiny90) && tools::file_ext(shiny90) != "shiny90") {
    stop("File does not have extension .shiny90: ", shiny90)
  }

  if (force_shiny90 && !is.null(shiny90)) {
    keep_str <- "DP$|PJN$"
  } else {
    keep_str <- "DP$|PJN$|shiny90"
  }

  file.copy(pjnz, out)
  files_list <- utils::unzip(out, list=TRUE)$Name
  files_delete <- grep(keep_str, files_list, value = TRUE, invert = TRUE, ignore.case = TRUE)
  f <- utils::zip(out, files_delete, flags="-d")

  if (!is.null(shiny90) && !check_pjnz_shiny90(out)) {
    zip::zip_append(out, shiny90, mode = "cherry-pick")
  }

  f
}

#' Read Spectrum region code from PJNZ file
#'
#' @param pjnz file path to source PJNZ
#'
#' @export
read_pjnz_region_code <- function(pjnz) {
  pjnfile <- grep(".PJN$", unzip(pjnz, list = TRUE)$Name, value = TRUE)
  pjn <- read.csv(unz(pjnz, pjnfile), as.is = TRUE)
  region_code <- pjn[which(pjn[, 1] == "<Projection Parameters - Subnational Region Name2>") + 3, 4]
  as.integer(region_code)
}


#' Check whether PJNZ contains .shiny90 file
#'
#' @param pjnz file path to PJNZ
#'
#' @return Logical whether PJNZ file contains a .shiny90 file
#'
#' @details
#' TODO: Check whether the .shiny90 file is valid.
#'
#' @export
check_pjnz_shiny90 <- function(pjnz) {
  any(grepl("\\.shiny90$", utils::unzip(pjnz, list = TRUE)$Name))
}


#' Read country from .zip.shiny90 file
#'
#' @param shiny90_zip
#' @return Shiny90 country / region name.
#'
#' @export
read_shiny90_country <- function(shiny90_zip) {
  readLines(unz(shiny90_zip, "country.txt"))
}
