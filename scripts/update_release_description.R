#! /usr/bin/env Rscript
"Update release descriptions

Usage:
    update_release_description.R  [--dry-run] [--site=<site>] --key=<key>
    update_release_description.R -h | --help

Options
    --dry-run     Run in dry-run mode.
    --site=<site> ADR site to use dev or prod [default: dev].
    --key=<key>   ADR auth key.
" -> doc

args <- docopt::docopt(doc)
dry_run <- args[["dry_run"]]
key <- args[["key"]]
site <- args[["site"]]
if (site == "prod") {
  url <- "https://adr.unaids.org/"
} else if (site == "dev") {
  url <- "https://dev.adr.fjelltopp.org/"
} else {
  stop("Site must be prod or dev")
}
message("Updating descriptions on site ", url)

ckanr::ckanr_setup(url = url, key = key)

list_releases <- function(dataset_id) {
  res <- ckanr:::ckan_GET(
    url = ckanr::get_default_url(),
    method = "dataset_version_list",
    query = list(dataset_id = dataset_id),
    key = ckanr::get_default_key(),
    headers = ckanr:::ctj()
  )
  ckanr:::jsl(res)
}

get_release <- function(package, release_name) {
  releases <- list_releases(package$id)
  if (length(releases) == 0) {
    message(sprintf("No release found for %s - skipping", package$name))
    return(NULL)
  }
  release_id <- NULL
  for (rel in releases) {
    if (rel$name == release_name) {
      release_id <- rel$id
      message(sprintf("Found release %s for package %s",
                      release_name, package$name))
      break
    }
  }
  if (is.null(release_id)) {
    message(sprintf(
      paste0(
        "Release %s not found in package %s with name %s, ",
        "falling back to most recent release"
      ),
      release_name, package$id, package$name
    ))
    release_id <- releases[[1]]$id
  }
  list(package_id = package$id,
       version_id = release_id)
}

update_desc <- function(ids) {
  dataset_id <- ids$dataset_id
  version_id <- ids$version_id
  res <- ckanr:::ckan_POST(url = ckanr::get_default_url(),
                      method = "version_update",
                      body = jsonlite::toJSON(list(
                        dataset_id = dataset_id,
                        version_id = version_id,
                        name = "2022 data transfer",
                        notes = paste0("Data automatically transferred from ",
                                       "2022 dataset. Added blank columns to ANC",
                                       " data for new indicators.")
                      ), auto_unbox = TRUE),
                      key = ckanr::get_default_key(),
                      encode = "json",
                      headers = ckanr:::ctj())
  ckanr:::jsl(res)
}

src <- "country-estimates-23"
packages <- ckanr::package_search(q = sprintf("type:%s", src),
                                  rows = 1000)
releases <- lapply(packages$results, get_release, "naomi_final_2022")
releases <- releases[vapply(releases, function(x) !is.null(x), logical(1))]
lapply(releases, update_desc)
