#' Download debug from server into a local ticket folder
#'
#' Downloads to `<root>/<jobid>/`. Needs membership of the hivtools
#' `naomi-debug` GitHub team and a PAT in `NAOMI_DOWNLOAD_DEBUG_TOKEN`.
#'
#' @param id The model fit or calibrate ID to download debug for
#' @param jobid The issue ID, the name of the folder to create under `root`
#' @param root Local debug root; defaults to `NAOMI_DEBUG_ONEDRIVE`, else the
#'   working directory
#' @param server The server to download debug from, defaults to production
#'
#' @return Path to local debug
#' @export
naomi_debug <- function(id, jobid,
                        root = Sys.getenv("NAOMI_DEBUG_ONEDRIVE", "."),
                        server = NULL) {
  dest <- file.path(path.expand(root), as.character(jobid))
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  hintr::download_debug(id, dest = dest, server = server)
}

#' Prepare output from hintr debug rds for debugging
#'
#' @param jobid The model fit or calibrate ID (folder created by `hintr::download_debug()`)
#' @param root The ticket folder, i.e. the path returned by `naomi_debug()`'s `dest`
#'
#' @return Path to local debug
#' @export
hintr_inputs_ready <- function(jobid, root = ".") {
  path <- file.path(normalizePath(root), jobid)

  data <- readRDS(file.path(path, "data.rds"))$variables$data
  options <- readRDS(file.path(path, "data.rds"))$variables$options

  data <- lapply(data, function(x){x$path <- file.path(path, "files", x$path); x})

  names(data)[names(data) == "anc"] <- "anc_testing"
  names(data)[names(data) == "programme"] <- "art_number"
  options$verbose <- TRUE

  list(data = data, options = options)
}
