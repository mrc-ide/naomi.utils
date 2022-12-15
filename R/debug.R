#' Download debug from server and upload into sharepoint
#'
#' @param id The model fit or calibrate ID to download debug for
#' @param jobid The issue ID, the name of the folder to create in sharepoint
#' @param dest_folder The root destination folder in sharepoint
#' @param server The folder to download debug from, defaults to production
#'   server
#'
#' @return Path to local debug
#' @export
naomi_debug <- function(id, jobid,
                        dest_folder = "Shared Documents/2023_debug",
                        server = NULL) {
  debug <- hintr::download_debug(id, server = server)
  sp <- spud::sharepoint$new("https://imperiallondon.sharepoint.com")
  folder <- sp$folder("NaomiSupport-WP", path = dest_folder, verify = TRUE)
  debug_folder <- folder$create(file.path(as.character(jobid), id))

  dirs <- list.dirs(debug, recursive = FALSE, full.names = TRUE)
  files <- setdiff(list.files(debug, full.names = TRUE), dirs)
  upload_files(debug_folder, files)

  for (dir in dirs) {
    new_folder <- debug_folder$create(basename(dir))
    subdirs <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
    files <- setdiff(list.files(dir, full.names = TRUE), subdirs)
    upload_files(new_folder, files)
  }
  debug
}

upload_files <- function(sp_folder, files) {
  for (file in files) {
    sp_folder$upload(file, progress = TRUE)
  }
}

#' Prepare output from hintr debug rds for debugging
#'
#' @param jobid The issue ID, the name of the folder in sharepoint
#' @param root The debug root dir
#'
#' @return Path to local debug
#' @export
hintr_inputs_ready <- function(jobid, root = ".") {
  path <- file.path(normalizePath(root), jobid)

  data <- readRDS(file.path(path, "data.rds"))$objects$data
  options <- readRDS(file.path(path, "data.rds"))$objects$options

  data <- lapply(data, function(x){x$path <- file.path(path, "files", x$path); x})

  names(data)[names(data) == "anc"] <- "anc_testing"
  names(data)[names(data) == "programme"] <- "art_number"
  options$verbose <- TRUE

  list(data = data, options = options)
}
